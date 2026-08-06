#!/usr/bin/env python3
"""
Integration test: the file-navigation chords still work with the integrated
terminal focused.

Reported: with a terminal docked and focused, ctrl+alt+arrow (and the
ctrl+pageup/pagedown spelling of the same command) were swallowed by the
panel, so reading around a codebase with a terminal open meant hiding and
re-showing the panel for every file.

The panel takes keys first while focused and deliberately swallows what it
does not recognise -- which is right, or an unmapped chord would run an
editor command against the document while the user is looking at a prompt.
The navigation chords are now excepted, by a predicate the panel consults
rather than a second copy of the key list.

Ordinary typing must still reach the shell, so that is checked too: a fix
that let everything through would pass the first half of this file and break
the terminal.

Usage: python3 test/integration_termpanel_nav.py [path-to-fac-binary]
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

ROWS, COLS = 32, 120
NEXT = "\x1b[6;5~"          # ctrl-pagedown
PREV = "\x1b[5;5~"          # ctrl-pageup

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


class Session:
    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_tn_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_tn_work_")
        for nm in ("alpha.c", "beta.c", "gamma.c"):
            with open(os.path.join(self.work, nm), "w") as f:
                f.write("int %s;\n" % nm[:-2])
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home,
               "PS1": "$ ", "SHELL": "/bin/sh"}
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

    def text(self):
        return "\n".join(r.rstrip() for r in self.screen.display)

    def status(self):
        return self.screen.display[ROWS - 1].rstrip()

    def current_file(self):
        """The ACTIVE tab, read from the bar.

        Not the status bar: it carries transient messages, and one of them
        ("... is already open") was what this used to read as a filename.
        """
        row = 0
        run = ""
        for x in range(self.screen.columns):
            cell = self.screen.buffer[row][x]
            if cell.reverse:
                run += cell.data
            elif run:
                break
        m = re.search(r"([\w.]+\.c)", run)
        return m.group(1) if m else None

    def open_all(self):
        """Open the other two files so there is somewhere to navigate to."""
        self.send("\x02", 1.2)
        for _ in range(3):
            self.send("\x1b[B", 0.25)
            self.send("\r", 0.9)
        self.send("\x02", 0.8)

    def open_terminal(self):
        self.send("\x1bt", 2.5)            # alt-t

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def test_navigation_works_with_the_terminal_focused(binary):
    print("\nCtrl+PageDown changes file while the terminal has focus")
    s = Session(binary)
    try:
        s.open_all()
        s.open_terminal()
        before = s.current_file()
        check(before is not None, "a file is showing", s.status()[:80])

        s.send(NEXT, 1.4)
        after = s.current_file()
        check(after is not None and after != before,
              "the active file changed", f"{before} -> {after}")

        s.send(PREV, 1.4)
        check(s.current_file() == before, "and back again",
              f"{after} -> {s.current_file()}")
    finally:
        s.close()


def test_ordinary_typing_still_reaches_the_shell(binary):
    print("\nAnd ordinary keys still go to the shell, not the document")
    s = Session(binary)
    try:
        s.open_terminal()
        s.send("echo FACMARKER", 1.2)
        check("FACMARKER" in s.text(),
              "what was typed appears in the panel", s.text()[-200:])

        s.send("\r", 1.8)
        check(s.text().count("FACMARKER") >= 1,
              "and the shell ran it rather than the editor eating it")

        # The document must be untouched by all that.
        s.send("\x1bt", 1.2)               # hide the panel
        check("int alpha;" in s.text(),
              "the file is unchanged underneath", s.text()[:160])
        check("FACMARKER" not in s.text(),
              "and none of the typing landed in it", s.text()[:200])
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_navigation_works_with_the_terminal_focused,
               test_ordinary_typing_still_reaches_the_shell):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_termpanel_nav: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_termpanel_nav: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
