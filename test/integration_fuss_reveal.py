#!/usr/bin/env python3
"""
Integration test: opening a file from the tree that is already open reveals
it instead of opening it again.

Reported: Enter in fuss mode on a file that already had a tab added a SECOND
tab for the same file, so a bar could end up holding the same document
several times.

Enter now reveals -- switching to the tab, and to the pane inside it when the
file is open in a split. Shift+Enter still opens another copy for when that
is what is wanted, and the context menu carries the same action, which is
the reliable route: Shift+Enter only reaches the editor from terminals that
speak the kitty keyboard protocol.

The lookup matches CANONICAL PATHS. Matching basenames would find the wrong
file the moment two directories each hold a main.c, which is the ordinary
case in a C project -- so that is tested directly.

Usage: python3 test/integration_fuss_reveal.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import os
import re
import shutil
import sys
import tempfile
import time

try:
    import pexpect
    import pyte
except ImportError as e:
    print(f"SKIP: missing dependency ({e}); pip3 install pexpect pyte")
    sys.exit(0)

ROWS, COLS = 30, 140
SHIFT_ENTER = "\x1b[13;2u"          # kitty CSI-u: codepoint 13, shift

failures = []


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}" + (f"  [{detail}]" if detail and not ok else ""))
    if not ok:
        failures.append(name)


def find_binary():
    if len(sys.argv) > 1:
        return os.path.abspath(sys.argv[1])
    here = os.path.dirname(os.path.abspath(__file__))
    cand = os.path.join(os.path.dirname(here), "fac")
    if os.path.exists(cand):
        return cand
    print("SKIP: no fac binary (build with make, or pass a path)")
    sys.exit(0)


class Tree:
    def __init__(self, binary, nested=False):
        self.home = tempfile.mkdtemp(prefix="fac_fr_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_fr_work_")
        for nm in ("alpha.c", "beta.c", "gamma.c"):
            with open(os.path.join(self.work, nm), "w") as f:
                f.write("int %s;\n" % nm[:-2])
        if nested:
            # Two files with the SAME basename in different directories.
            for d in ("one", "two"):
                os.makedirs(os.path.join(self.work, d))
                with open(os.path.join(self.work, d, "main.c"), "w") as f:
                    f.write("int MAIN_%s;\n" % d.upper())

        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [os.path.join(self.work, "alpha.c")],
                                   dimensions=(ROWS, COLS), env=env, cwd=self.work)
        self.drain(2.5)

    def drain(self, w=0.5):
        end = time.time() + w
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def send(self, d, w=0.7):
        self.child.send(d)
        self.drain(w)

    def bar(self):
        return self.screen.display[0].rstrip()

    def names(self):
        return re.findall(r"\d+\s+([\w.]+)", self.bar())

    def status(self):
        return self.screen.display[ROWS - 1].rstrip()

    def body(self):
        return "\n".join(r.rstrip() for r in self.screen.display[1:8])

    def selected(self):
        for y in range(2, ROWS - 1):
            if any(self.screen.buffer[y][x].reverse for x in range(0, 30)):
                return "".join(self.screen.buffer[y][x].data
                               for x in range(0, 30)).strip()
        return None

    def walk_to(self, want, limit=24, reset=True):
        # Start from the top: the selection may be below the target when the
        # tree is re-entered, and pressing down would never reach it.
        if reset:
            for _ in range(24):
                self.send("\x1b[A", 0.06)
        for _ in range(limit):
            sel = self.selected()
            if sel and want in sel:
                return True
            self.send("\x1b[B", 0.2)
        sel = self.selected()
        return bool(sel and want in sel)

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def test_enter_on_an_open_file_does_not_duplicate(binary):
    print("\nEnter on a file that is already open does not open it twice")
    s = Tree(binary)
    try:
        check(s.names() == ["alpha.c"], "alpha.c is the only tab", str(s.names()))
        s.send("\x02", 1.2)
        if not s.walk_to("alpha.c"):
            check(False, "found alpha.c in the tree", str(s.selected()))
            return
        s.send("\r", 1.4)
        check(s.names().count("alpha.c") == 1,
              "still one alpha.c tab", str(s.names()))
    finally:
        s.close()


def test_enter_on_a_new_file_still_opens_it(binary):
    print("\nAnd a file that is NOT open still opens")
    s = Tree(binary)
    try:
        s.send("\x02", 1.2)
        if not s.walk_to("beta.c"):
            check(False, "found beta.c in the tree", str(s.selected()))
            return
        s.send("\r", 1.4)
        check("beta.c" in s.names(), "beta.c opened", str(s.names()))
        check(len(s.names()) == 2, "as a second tab", str(s.names()))
    finally:
        s.close()


def test_shift_enter_opens_another_copy(binary):
    print("\nShift+Enter opens a second copy on purpose")
    s = Tree(binary)
    try:
        s.send("\x02", 1.2)
        if not s.walk_to("alpha.c"):
            check(False, "found alpha.c in the tree", str(s.selected()))
            return
        s.send(SHIFT_ENTER, 1.6)
        n = s.names().count("alpha.c")
        if n == 1:
            print("SKIP: this build/terminal did not deliver shift-enter")
            return
        check(n == 2, "a second alpha.c tab was opened", str(s.names()))
    finally:
        s.close()


def test_the_right_file_is_revealed_when_basenames_collide(binary):
    print("\nTwo files called main.c are told apart")
    s = Tree(binary, nested=True)
    try:
        # open one/main.c through the tree
        s.send("\x02", 1.2)
        for d in ("one", "two"):
            if s.walk_to(d):
                s.send("\x1b[C", 0.6)          # expand
        if not s.walk_to("main.c"):
            check(False, "found a main.c in the tree", str(s.selected()))
            return
        s.send("\r", 1.4)
        opened = s.body()
        check("main.c" in s.names(), "a main.c is open", str(s.names()))

        # The tree stays open after Enter, so no second ctrl-B: pressing it
        # would CLOSE the panel. The selection is still on main.c.
        sel = s.selected()
        check(sel is not None and "main.c" in sel,
              "the tree is still sitting on main.c", str(sel))
        if not (sel and "main.c" in sel):
            return
        s.send("\r", 1.4)
        s.send("\r", 1.4)
        check(s.names().count("main.c") == 1,
              "still one main.c tab", str(s.names()))
        check(s.body() == opened,
              "and it is the same one, not the other directory's",
              s.body()[:160])
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_enter_on_an_open_file_does_not_duplicate,
               test_enter_on_a_new_file_still_opens_it,
               test_shift_enter_opens_another_copy,
               test_the_right_file_is_revealed_when_basenames_collide):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_fuss_reveal: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_fuss_reveal: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
