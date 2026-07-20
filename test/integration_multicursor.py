#!/usr/bin/env python3
"""
Integration test: multi-cursor editing driven by real SGR mouse events.

Regression test for the mouse-multicursor bug: alt-click-placed cursors
sharing a line typed one character early, because per-cursor edits never
shifted the other cursors along with the text. Drives the real binary in
a pty: alt-click (SGR button 8) to add cursors, then type, backspace and
escape, asserting both the screen and the saved file.

Usage: python3 test/integration_multicursor.py [path-to-fac-binary]
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

ROWS, COLS = 30, 100

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
    def __init__(self, binary, content, name="mc.txt"):
        self.home = tempfile.mkdtemp(prefix="fac_mc_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.target = os.path.join(self.home, name)
        with open(self.target, "w") as f:
            f.write(content)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.home)
        self.drain(1.5)

    def drain(self, wait=0.3):
        end = time.time() + wait
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def click(self, row, col, button=0):
        """1-based screen coords, SGR encoding; button 8 = alt+left."""
        self.child.send(f"\x1b[<{button};{col};{row}M")
        self.child.send(f"\x1b[<{button};{col};{row}m")
        self.drain(0.25)

    def find_cell(self, needle):
        for y, r in enumerate(self.screen.display):
            j = r.find(needle)
            if j >= 0:
                return y + 1, j + 1
        return None

    def row_with(self, text):
        for r in self.screen.display:
            if text in r:
                return r.rstrip()
        return None

    def save(self):
        self.child.send("\x13")
        self.drain(0.8)
        with open(self.target) as f:
            return f.read()

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)


def main():
    binary = find_binary()

    # --- The reported bug: same-line cursors after both ']'s, then type.
    s = Session(binary, "mm [1] [2] nn\n")
    r, c = s.find_cell("] [")
    s.click(r, c + 1)                 # primary just after first ]
    r, c = s.find_cell("] nn")
    s.click(r, c + 1, button=8)       # alt-click just after second ]
    s.child.send("X")
    s.drain(0.5)
    check(s.row_with("mm [1]X [2]X nn") is not None,
          "same-line alt-click cursors type at their own positions",
          str(s.row_with("mm")))
    saved = s.save()
    check("mm [1]X [2]X nn" in saved, "saved buffer matches screen", saved.strip())
    s.close()

    # --- The user's bulk-edit flow: cursors at ends of ]s on three lines,
    # type a suffix on all of them at once, then backspace it away again.
    s = Session(binary, "aa [1] bb\ncc [22] dd\nee [333] ff\nzz\n")
    r, c = s.find_cell("zz")
    s.click(r, c)
    for pat in ("] bb", "] dd", "] ff"):
        r, c = s.find_cell(pat)
        s.click(r, c + 1, button=8)
    for ch in "!!":
        s.child.send(ch)
        s.drain(0.2)
    saved = s.save()
    check("aa [1]!! bb" in saved and "cc [22]!! dd" in saved and
          "ee [333]!! ff" in saved and "!!zz" in saved,
          "cross-line bulk insert lands after every ]", saved.strip())

    s.child.send("\x7f")              # backspace once on all four cursors
    s.drain(0.5)
    saved = s.save()
    check("aa [1]! bb" in saved and "cc [22]! dd" in saved and
          "ee [333]! ff" in saved and ("!zz" in saved),
          "multi-cursor backspace removes one char at each cursor", saved.strip())

    # --- Escape collapses to one cursor; typing then edits a single spot.
    s.child.send("\x1b")
    s.drain(0.4)
    s.child.send("Q")
    s.drain(0.4)
    saved = s.save()
    check(saved.count("Q") == 1, "escape collapses to a single cursor",
          saved.strip())
    s.close()

    # --- Alt-click on an existing cursor removes it.
    s = Session(binary, "aa [1] bb\ncc [22] dd\n")
    r, c = s.find_cell("] bb")
    s.click(r, c + 1)
    r2, c2 = s.find_cell("] dd")
    s.click(r2, c2 + 1, button=8)     # add
    s.click(r2, c2 + 1, button=8)     # remove again
    s.child.send("Y")
    s.drain(0.5)
    saved = s.save()
    check(saved.count("Y") == 1 and "aa [1]Y bb" in saved,
          "alt-click toggles a cursor off", saved.strip())
    s.close()

    if failures:
        print(f"integration_multicursor: FAILED ({len(failures)}: {', '.join(failures)})")
        sys.exit(1)
    print("integration_multicursor: ALL PASSED")


if __name__ == "__main__":
    main()
