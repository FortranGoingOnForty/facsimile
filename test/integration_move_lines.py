#!/usr/bin/env python3
"""
Integration test: alt+up / alt+down move the whole SELECTION.

They used to move exactly one line -- the active cursor's -- no matter how
much was selected. Select three lines, press alt+up, and one line jumped out
of the middle of the block while the highlight stayed painted where it was.
The selection then described text it no longer covered, and a second press
worked from that wrong idea of the range.

VSCode moves the selected block as a unit and carries the selection with it,
so the same text stays selected and the gesture is repeatable. That is what
is asserted here.

Content is checked against the SAVED FILE rather than the screen: the point
of the bug is corruption, and only the file can say whether the document
survived. Selection is checked on the screen, because it is drawn with
reverse video and nothing else can report it.

Usage: python3 test/integration_move_lines.py [path-to-fac-binary]
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

ROWS, COLS = 24, 90

# Distinct, same-length markers: a swap is unmistakable, and no line is a
# substring of another.
MARKERS = ["ALPHA", "BRAVO", "CHARLIE", "DELTA", "ECHO", "FOXTROT", "GOLF", "HOTEL"]
SRC = "".join("%s\n" % m for m in MARKERS)

UP, DOWN = "\x1b[A", "\x1b[B"
SHIFT_DOWN = "\x1b[1;2B"
SHIFT_UP = "\x1b[1;2A"
ALT_UP, ALT_DOWN = "\x1b[1;3A", "\x1b[1;3B"
CTRL_HOME = "\x1b[1;5H"
CTRL_END = "\x1b[1;5F"
CTRL_S = "\x13"
CTRL_Z = "\x1a"

failures = []


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
        self.home = tempfile.mkdtemp(prefix="fac_ml_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.target = os.path.join(self.home, "t.txt")
        with open(self.target, "w") as f:
            f.write(SRC)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
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

    def goto_line(self, n):
        """Column 1 of line n, with no selection."""
        self.send(CTRL_HOME, 0.5)
        for _ in range(n - 1):
            self.send(DOWN, 0.2)

    def select_down(self, n):
        for _ in range(n):
            self.send(SHIFT_DOWN, 0.25)

    def saved(self):
        """The document as it is on disk after a save."""
        self.send(CTRL_S, 1.0)
        with open(self.target) as f:
            return f.read()

    def selected_markers(self):
        """Which markers are drawn reverse-video (i.e. selected).

        Read off the screen, not from any internal flag: the original bug was
        that the highlight stayed put while the text moved out from under it,
        so only the screen can tell them apart.
        """
        out = []
        for y in range(1, ROWS - 1):
            row = self.screen.buffer[y]
            text = "".join(row[x].data for x in range(COLS)
                           if row[x].reverse).strip()
            for m in MARKERS:
                if m in text:
                    out.append(m)
        return out

    def close(self):
        try:
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)


def order(text):
    return [ln for ln in text.split("\n") if ln]


def test_selection_moves_up_as_a_block(binary):
    print("\nalt+up moves the whole selection, not one line of it")
    s = Session(binary)
    try:
        s.goto_line(3)                      # CHARLIE
        s.select_down(2)                    # anchor (3,1) .. caret (5,1) => 3..4
        check(s.selected_markers() == ["CHARLIE", "DELTA"],
              "CHARLIE and DELTA are selected to begin with",
              s.selected_markers())

        s.send(ALT_UP, 0.8)
        got = order(s.saved())
        check(got == ["ALPHA", "CHARLIE", "DELTA", "BRAVO",
                      "ECHO", "FOXTROT", "GOLF", "HOTEL"],
              "the two selected lines moved up together", got)
        check(s.selected_markers() == ["CHARLIE", "DELTA"],
              "and the same two lines are still selected",
              s.selected_markers())
    finally:
        s.close()


def test_selection_moves_down_as_a_block(binary):
    print("\nalt+down does the same downwards")
    s = Session(binary)
    try:
        s.goto_line(3)
        s.select_down(2)
        s.send(ALT_DOWN, 0.8)
        got = order(s.saved())
        check(got == ["ALPHA", "BRAVO", "ECHO", "CHARLIE",
                      "DELTA", "FOXTROT", "GOLF", "HOTEL"],
              "the block moved down over ECHO", got)
        check(s.selected_markers() == ["CHARLIE", "DELTA"],
              "the selection came with it", s.selected_markers())
    finally:
        s.close()


def test_it_is_repeatable_and_reversible(binary):
    print("\nRepeated presses keep working, and undo the document exactly")
    s = Session(binary)
    try:
        s.goto_line(3)
        s.select_down(3)                    # 3..5: CHARLIE DELTA ECHO
        for _ in range(2):
            s.send(ALT_DOWN, 0.7)
        got = order(s.saved())
        check(got == ["ALPHA", "BRAVO", "FOXTROT", "GOLF",
                      "CHARLIE", "DELTA", "ECHO", "HOTEL"],
              "two presses moved the block two lines", got)
        for _ in range(2):
            s.send(ALT_UP, 0.7)
        check(s.saved() == SRC, "and two back restores the file byte for byte",
              repr(s.saved()))
    finally:
        s.close()


def test_the_last_line_survives_a_round_trip(binary):
    print("\nMoving a block across the final line does not corrupt it")
    s = Session(binary)
    try:
        s.goto_line(6)                      # FOXTROT
        s.select_down(2)                    # 6..7: FOXTROT GOLF
        s.send(ALT_DOWN, 0.8)               # over HOTEL, the last line
        got = order(s.saved())
        check(got == ["ALPHA", "BRAVO", "CHARLIE", "DELTA", "ECHO",
                      "HOTEL", "FOXTROT", "GOLF"],
              "the block moved past the last line", got)
        check(s.saved().endswith("GOLF\n"),
              "the file still ends with a newline", repr(s.saved()[-12:]))
        s.send(ALT_UP, 0.8)
        check(s.saved() == SRC, "and moving back restores it exactly",
              repr(s.saved()))
    finally:
        s.close()


def test_the_edges_are_no_ops(binary):
    print("\nA block against an edge does not move, and nothing is lost")
    s = Session(binary)
    try:
        s.goto_line(1)
        s.select_down(2)                    # 1..2
        s.send(ALT_UP, 0.8)
        check(s.saved() == SRC, "alt+up at the top of the file did nothing",
              repr(s.saved()))

        # The file ends with a newline, so its final line is the empty one
        # after it -- the same line VSCode shows there. THAT is the bottom.
        # Twice: the first press only collapses the live selection onto its
        # end, which is what a plain move is supposed to do.
        s.send(CTRL_END, 0.6)
        s.send(CTRL_END, 0.6)
        s.send(ALT_DOWN, 0.8)
        check(s.saved() == SRC, "alt+down on the final line did nothing",
              repr(s.saved()))
    finally:
        s.close()


def test_the_empty_final_line_is_a_line_like_any_other(binary):
    print("\nThe empty line after the trailing newline moves like any other")
    s = Session(binary)
    try:
        s.goto_line(7)
        s.select_down(2)                    # 7..8: GOLF and HOTEL
        s.send(ALT_DOWN, 0.8)
        # The block swaps with the empty final line, which lands above it.
        check(s.saved() == "ALPHA\nBRAVO\nCHARLIE\nDELTA\nECHO\nFOXTROT\n"
                           "\nGOLF\nHOTEL",
              "the block moved below the empty final line", repr(s.saved()))
        s.send(ALT_UP, 0.8)
        check(s.saved() == SRC, "and moving back restores the file exactly",
              repr(s.saved()))
    finally:
        s.close()


def test_undo_puts_the_block_back(binary):
    print("\nCtrl-Z undoes a block move")
    s = Session(binary)
    try:
        s.goto_line(3)
        s.select_down(2)
        s.send(ALT_DOWN, 0.8)
        check(order(s.saved())[2] == "ECHO", "the block moved",
              order(s.saved()))
        # The edit is now two large buffer operations rather than a long run
        # of single-character ones; undo has to treat it as one step either
        # way.
        for _ in range(4):
            s.send(CTRL_Z, 0.5)
            if s.saved() == SRC:
                break
        check(s.saved() == SRC, "undo restored the document", repr(s.saved()))
    finally:
        s.close()


def test_a_bare_cursor_still_moves_one_line(binary):
    print("\nWith no selection it is still a single-line move")
    s = Session(binary)
    try:
        s.goto_line(3)
        s.send(ALT_UP, 0.8)
        got = order(s.saved())
        check(got == ["ALPHA", "CHARLIE", "BRAVO", "DELTA",
                      "ECHO", "FOXTROT", "GOLF", "HOTEL"],
              "CHARLIE alone moved up", got)
        check(s.selected_markers() == [],
              "and no selection was invented", s.selected_markers())
    finally:
        s.close()


def test_a_selection_within_one_line_moves_that_line(binary):
    print("\nA selection inside a single line moves just that line")
    s = Session(binary)
    try:
        s.goto_line(3)
        s.send("\x1b[1;2C", 0.3)            # shift+right: part of CHARLIE
        s.send("\x1b[1;2C", 0.3)
        s.send(ALT_DOWN, 0.8)
        got = order(s.saved())
        check(got == ["ALPHA", "BRAVO", "DELTA", "CHARLIE",
                      "ECHO", "FOXTROT", "GOLF", "HOTEL"],
              "CHARLIE moved down once", got)
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_selection_moves_up_as_a_block,
               test_selection_moves_down_as_a_block,
               test_it_is_repeatable_and_reversible,
               test_the_last_line_survives_a_round_trip,
               test_the_edges_are_no_ops,
               test_the_empty_final_line_is_a_line_like_any_other,
               test_undo_puts_the_block_back,
               test_a_bare_cursor_still_moves_one_line,
               test_a_selection_within_one_line_moves_that_line):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_move_lines: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_move_lines: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
