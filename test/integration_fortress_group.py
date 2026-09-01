#!/usr/bin/env python3
"""
Integration test: Shift+Enter on a directory in fortress offers a tab group.

`fac <dir>/` typed at the built-in terminal already opens the New Tab Group
dialog for that directory. This gives the ctrl-o navigator the same gesture,
without taking anything away: plain Enter still switches the workspace, which
was its only route from inside the editor.

Shift+Enter only exists as a kitty CSI-u report (ESC [ 13 ; 2 u). Terminals
that do not speak that protocol send a plain Enter and cannot tell the two
apart, so Ctrl-G does the same thing and both are named in the footer.

Parsing that sequence also fixed a latent bug: fortress read exactly one
character after `ESC [`, so a CSI-u report leaked its remaining digits into
the type-to-jump search buffer one character at a time.

Usage: python3 test/integration_fortress_group.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import os
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

ROWS, COLS = 30, 120
SHIFT_ENTER = "\x1b[13;2u"
CTRL_G = "\x07"
CTRL_O = "\x0f"

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


class Fortress:
    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_fg_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_fg_work_")
        os.makedirs(os.path.join(self.work, "ch5"))
        for n in ("alpha.c", "beta.c", "gamma.c"):
            with open(os.path.join(self.work, "ch5", n), "w") as f:
                f.write("int %s;\n" % n[:-2])
        with open(os.path.join(self.work, "top.c"), "w") as f:
            f.write("int top;\n")

        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [os.path.join(self.work, "top.c")],
                                   dimensions=(ROWS, COLS), env=env, cwd=self.work)
        self.drain(2.5)

    def drain(self, w=0.6):
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

    def text(self):
        return "\n".join(r.rstrip() for r in self.screen.display)

    def selected(self):
        """Fortress marks the selection with bold, underlined text."""
        for y in range(1, ROWS - 1):
            row = self.screen.display[y]
            d = row.find("│")
            lo = d + 1 if d >= 0 else 0
            cells = [self.screen.buffer[y][x] for x in range(lo, COLS)]
            selected = [c for c in cells if c.bold and c.underscore]
            if selected:
                return "".join(c.data for c in selected).strip()
        return None

    def open_on(self, name, limit=16):
        self.send(CTRL_O, 1.6)
        for _ in range(limit):
            sel = self.selected()
            if sel and name in sel:
                return True
            self.send("\x1b[B", 0.25)
        sel = self.selected()
        return bool(sel and name in sel)

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def _group_case(binary, key, label):
    s = Fortress(binary)
    try:
        if not s.open_on("ch5"):
            check(False, f"{label}: found ch5 in fortress", str(s.selected()))
            return
        s.send(key, 2.0)
        body = s.text()
        check("New Tab Group" in body,
              f"{label}: the tab group dialog appeared", body[:200])
        check("ch5" in body, f"{label}: for ch5", body[:200])
        # Its own files, not the parent's.
        # Scope to the dialog box: the fortress screen behind it still
        # shows the parent directory's entries.
        box = "\n".join(r for r in s.screen.display if "│" in r or "╭" in r)
        check("alpha.c" in box, f"{label}: listing ch5's own files", box[:250])
    finally:
        s.close()


def test_shift_enter_offers_a_group(binary):
    print("\nShift+Enter on a directory offers to make a tab group of it")
    _group_case(binary, SHIFT_ENTER, "shift-enter")


def test_ctrl_g_does_the_same(binary):
    print("\nAnd Ctrl-G does the same, for terminals that cannot send it")
    _group_case(binary, CTRL_G, "ctrl-g")


def test_plain_enter_still_switches_workspace(binary):
    print("\nPlain Enter still switches the workspace")
    s = Fortress(binary)
    try:
        if not s.open_on("ch5"):
            check(False, "found ch5 in fortress", str(s.selected()))
            return
        s.send("\r", 2.2)
        body = s.text()
        check("New Tab Group" not in body,
              "no group dialog", body[:200])
        # The workspace really moved: the file that was open belonged to the
        # OLD workspace and is gone, replaced by ch5's (empty) session.
        bar = s.screen.display[0]
        check("top.c" not in bar,
              "the previous workspace's file is no longer open", bar[:80])
        check("Untitled" in bar or "alpha" in bar,
              "and ch5's session took its place", bar[:80])
    finally:
        s.close()


def test_a_csi_u_report_does_not_reach_the_search(binary):
    print("\nAn unrelated CSI-u report does not leak into type-to-jump")
    s = Fortress(binary)
    try:
        s.send(CTRL_O, 1.6)
        before = s.selected()
        # F-key style CSI-u for 'z' with no modifier: not a binding, and its
        # digits must not be typed into the fuzzy search.
        s.send("\x1b[122;1u", 1.0)
        check(s.selected() == before,
              "the selection did not move", f"{before} -> {s.selected()}")
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_shift_enter_offers_a_group,
               test_ctrl_g_does_the_same,
               test_plain_enter_still_switches_workspace,
               test_a_csi_u_report_does_not_reach_the_search):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_fortress_group: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_fortress_group: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
