#!/usr/bin/env python3
"""
Integration test: what a plain cursor move does to a selection.

Two behaviours, and they turned out to be the same fix.

A move without Shift ends the selection. It used to leave the highlight
painted while the caret walked away from it, so the only way to clear it was
Esc -- and the highlight persisted on rows the caret had left, because a
cursor-only repaint touches the caret's line and nothing else.

And the move lands on the END the key was heading for: Left and Up on the
start of the selection, Right and Down on its end. Collapse only, never also
move -- the first press after selecting lands on an end and stops there, the
next moves from it. Moving as well would mean Right after selecting rightwards
skipped a character, with no press that simply lands on the end.

Usage: python3 test/integration_selection.py [path-to-fac-binary]
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

ROWS, COLS = 24, 90
SRC = "".join("line %d aaaa\n" % i for i in range(1, 12))
failures = []

SHIFT_DOWN = "\x1b[1;2B"
SHIFT_UP = "\x1b[1;2A"
SHIFT_RIGHT = "\x1b[1;2C"
UP, DOWN, LEFT, RIGHT = "\x1b[A", "\x1b[B", "\x1b[D", "\x1b[C"
CTRL_HOME = "\x1b[1;5H"


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if not ok:
        failures.append(name)
        if detail:
            for ln in str(detail).split("\n")[:12]:
                print("        " + ln[:96])


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
        self.home = tempfile.mkdtemp(prefix="fac_sel_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.target = os.path.join(self.home, "t.txt")
        with open(self.target, "w") as f:
            f.write(SRC)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.home)
        self.drain(1.8)
        self.send(CTRL_HOME, 0.5)

    def drain(self, w=0.5):
        end = time.time() + w
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                pass
            except pexpect.EOF:
                break

    def send(self, d, w=0.45):
        self.child.send(d)
        self.drain(w)

    def pos(self):
        m = re.search(r"Ln (\d+), Col (\d+)", self.screen.display[-1])
        return (int(m.group(1)), int(m.group(2))) if m else (None, None)

    def highlighted(self):
        """Cells drawn reverse-video in the document area.

        Counted from the SCREEN rather than from any internal flag: the
        original bug was that the state said 'no selection' while the rows
        stayed painted, so only the screen can answer this.
        """
        n = 0
        for y in range(1, ROWS - 1):
            row = self.screen.buffer[y]
            for x in range(COLS):
                if row[x].reverse and row[x].data.strip():
                    n += 1
        return n

    def close(self):
        try:
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)


def test_a_plain_move_clears_the_selection(binary):
    s = Session(binary)
    try:
        for _ in range(3):
            s.send(SHIFT_DOWN, 0.3)
        check(s.highlighted() > 0, "shift+down selects", s.highlighted())

        s.send(UP, 0.6)
        check(s.highlighted() == 0,
              "a plain Up clears the highlight, with no Esc needed",
              f"{s.highlighted()} cells still drawn selected")
    finally:
        s.close()


# The highlight used to survive on rows the caret had left, because the fast
# path repaints the caret's line and nothing else.
def test_no_highlight_is_left_behind_on_other_rows(binary):
    s = Session(binary)
    try:
        for _ in range(3):
            s.send(SHIFT_DOWN, 0.3)
        s.send(UP, 0.6)
        stale = []
        for y in range(1, ROWS - 1):
            row = s.screen.buffer[y]
            cells = "".join(row[x].data if row[x].reverse else "" for x in range(COLS))
            if cells.strip():
                stale.append((y + 1, cells.strip()[:30]))
        check(not stale, "no row anywhere is left painted", str(stale[:4]))
    finally:
        s.close()


def test_up_returns_to_the_start_of_the_selection(binary):
    s = Session(binary)
    try:
        for _ in range(3):
            s.send(SHIFT_DOWN, 0.3)
        check(s.pos()[0] == 4, "selected down to line 4", s.pos())
        s.send(UP, 0.6)
        check(s.pos() == (1, 1),
              "Up returns to where the selection started, not one line up "
              "from where it ended", s.pos())
    finally:
        s.close()


def test_left_and_right_land_on_the_ends(binary):
    s = Session(binary)
    try:
        for _ in range(4):
            s.send(SHIFT_RIGHT, 0.25)
        check(s.pos() == (1, 5), "selected four characters", s.pos())
        s.send(LEFT, 0.6)
        check(s.pos() == (1, 1), "Left lands on the start", s.pos())

        s.send(CTRL_HOME, 0.4)
        for _ in range(4):
            s.send(SHIFT_RIGHT, 0.25)
        s.send(RIGHT, 0.6)
        check(s.pos() == (1, 5),
              "Right lands on the end without stepping past it", s.pos())
    finally:
        s.close()


def test_the_next_press_moves_normally(binary):
    """Collapsing consumes one press; the one after it moves."""
    s = Session(binary)
    try:
        for _ in range(2):
            s.send(SHIFT_DOWN, 0.3)
        s.send(DOWN, 0.6)
        check(s.pos() == (3, 1), "Down collapses onto the end and stops", s.pos())
        s.send(DOWN, 0.5)
        check(s.pos() == (4, 1), "and the next Down moves on from there", s.pos())
    finally:
        s.close()


def test_shift_still_extends(binary):
    """The collapse must not eat Shift+arrow itself."""
    s = Session(binary)
    try:
        for _ in range(2):
            s.send(SHIFT_DOWN, 0.3)
        before = s.highlighted()
        s.send(SHIFT_DOWN, 0.4)
        check(s.highlighted() > before,
              "shift+down still grows the selection",
              f"{before} -> {s.highlighted()}")
        s.send(SHIFT_UP, 0.4)
        check(s.highlighted() > 0, "and shift+up shrinks rather than clearing",
              s.highlighted())
    finally:
        s.close()


def test_typing_over_a_selection_is_unaffected(binary):
    """Selection-replaces-on-type must still work; it is a different path."""
    s = Session(binary)
    try:
        for _ in range(4):
            s.send(SHIFT_RIGHT, 0.25)
        s.send("X", 0.5)
        s.send("\x13", 1.0)                 # ctrl-s
        with open(s.target) as f:
            first = f.read().split("\n")[0]
        check(first == "X 1 aaaa", "typing still replaces the selection",
              repr(first))
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_a_plain_move_clears_the_selection,
               test_no_highlight_is_left_behind_on_other_rows,
               test_up_returns_to_the_start_of_the_selection,
               test_left_and_right_land_on_the_ends,
               test_the_next_press_moves_normally,
               test_shift_still_extends,
               test_typing_over_a_selection_is_unaffected):
        try:
            fn(binary)
        except Exception as exc:                        # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    if failures:
        print(f"\nintegration_selection: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("\nintegration_selection: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
