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
    def __init__(self, binary, text=None):
        self.home = tempfile.mkdtemp(prefix="fac_sel_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.target = os.path.join(self.home, "t.txt")
        with open(self.target, "w") as f:
            f.write(SRC if text is None else text)
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


def test_a_key_that_names_a_destination_goes_there(binary):
    """Collapse-and-stop is for the arrows, not for a jump.

    ctrl-home means the top of the document. It was lumped in with the
    arrows, so with a selection live it collapsed onto the selection edge
    and went nowhere -- and since nothing looked broken on screen, the
    damage came later: keys pressed after it counted from the wrong line,
    and text was typed into the wrong part of the file."""
    s = Session(binary)
    try:
        # Start well below the top. Selecting from line 1 would collapse TO
        # line 1, which is also the right answer -- the assertion would pass
        # whether or not ctrl-home did anything.
        for _ in range(5):
            s.send(DOWN, 0.15)
        for _ in range(3):
            s.send(SHIFT_DOWN, 0.25)
        s.send(CTRL_HOME, 0.7)
        check(s.pos() == (1, 1),
              "ctrl-home reaches the top even with a selection live", s.pos())

        # And the mirror, from a backwards selection.
        for _ in range(4):
            s.send(DOWN, 0.15)
        for _ in range(2):
            s.send(SHIFT_UP, 0.25)
        s.send(CTRL_HOME, 0.7)
        check(s.pos() == (1, 1),
              "and from a selection made upwards", s.pos())
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


FUNC = 'int keep;\nvoid f(void)\n{\n    puts("hi");\n}\nint tail;\n'


def saved(s):
    s.send("\x13", 0.9)
    with open(s.target) as f:
        return f.read()


def test_cut_removes_exactly_what_was_selected(binary):
    """Reported: cutting a function left its closing brace behind.

    Selected with one Shift+Right and then Shift+Down to the last line, so
    the selection ends at column 2 of the brace line -- the brace IS inside
    it, and the highlight showed as much. Cut deleted everything except the
    brace while ALSO putting it on the clipboard, so pasting it back gave two.

    The copy and the delete each worked out the extent for themselves and
    disagreed at the end of a multi-line span. Asserted on the BUFFER so it
    holds with or without a system clipboard.
    """
    print("\nCut removes exactly the selected span")
    s = Session(binary, text=FUNC)
    try:
        s.send("\x1b[B")                       # line 2
        s.send("\x1b[1;2C")                    # shift+right
        for _ in range(3):
            s.send(SHIFT_DOWN)                  # down to the brace line
        s.send("\x18", 0.9)                    # ctrl-x
        got = saved(s)
        check(got == "int keep;\n\nint tail;\n",
              "the closing brace goes with the rest of the selection",
              repr(got))
        check("}" not in got, "no brace is left behind", repr(got))
    finally:
        s.close()


def test_cut_of_a_partial_last_line(binary):
    """The general case: the selection ends part way along its last line."""
    print("\nA selection ending mid-line cuts to exactly there")
    s = Session(binary, text="abcd\nefgh\nijkl\n")
    try:
        s.send("\x1b[C")                       # column 2 of line 1
        s.send("\x1b[1;2B")                    # shift+down -> (2,2)
        s.send("\x18", 0.9)
        got = saved(s)
        # "bcd\ne" comes out; "a" and "fgh" join.
        check(got == "afgh\nijkl\n",
              "the span from (1,2) to (2,2) is what goes", repr(got))
    finally:
        s.close()


def test_paste_replaces_a_selection(binary):
    """Reported: pasting over a selection kept it and inserted alongside.

    Driven with a BRACKETED paste, which is what a terminal-native paste
    sends and needs no system clipboard -- and which was a second copy of the
    same bug, on its own code path.
    """
    print("\nPasting over a selection replaces it")
    s = Session(binary, text="AAAA\nBBBB\nCCCC\n")
    try:
        s.send("\x1b[B")
        s.send("\x01")                          # home of line 2
        s.send("\x1b[1;2F")                     # shift+end -> select BBBB
        s.send("\x1b[200~ZZ\x1b[201~", 0.9)
        got = saved(s)
        check(got == "AAAA\nZZ\nCCCC\n",
              "the selected text is gone and the paste took its place",
              repr(got))
        check("BBBB" not in got, "nothing of the selection survives", repr(got))
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_cut_removes_exactly_what_was_selected,
               test_cut_of_a_partial_last_line,
               test_paste_replaces_a_selection,
               test_a_plain_move_clears_the_selection,
               test_no_highlight_is_left_behind_on_other_rows,
               test_up_returns_to_the_start_of_the_selection,
               test_left_and_right_land_on_the_ends,
               test_the_next_press_moves_normally,
               test_a_key_that_names_a_destination_goes_there,
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
