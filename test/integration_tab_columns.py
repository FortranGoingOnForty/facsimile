#!/usr/bin/env python3
"""
Integration test: vertical movement keeps its place ON SCREEN, not its
character index.

Reported from a tab-indented Makefile. With the caret on one line's '=',
directly beneath the '=' above it, ctrl+alt+shift+up put the new cursor in
the middle of the word `gcc` instead of on the '='.

A tab is ONE character but several display cells, so two lines whose leading
whitespace differs put the same character column in different places on
screen. Vertical movement carried the character column straight across, and
the caret drifted by exactly the difference. Measured on the reported file:

    CC\t\t\t= gcc              '=' is character 6
    CFLAGS\t\t= -Wall ...      '=' is character 9

so character 9 taken up from the second line lands on the last 'c' of `gcc`.
Plain arrow-up drifted identically -- it was never only a multi-cursor bug.

The goal is now held in display cells and converted per line. That must not
cost the goal-column behaviour every editor has, so passing THROUGH a short
line and out the other side is checked too.

Usage: python3 test/integration_tab_columns.py [path-to-fac-binary]
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

ROWS, COLS = 24, 100

# The reported file's shape. Every '=' lines up on screen; none of them share
# a character column.
TABBED = ("CC\t\t\t= gcc\n"
          "CFLAGS\t\t= -Wall -Wextra\n"
          "CPPFLAGS\t= -MMD -MP\n")
# The same thing to look at, with no tabs in it: the control.
SPACED = ("CC          = gcc\n"
          "CFLAGS      = -Wall -Wextra\n"
          "CPPFLAGS    = -MMD -MP\n")

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
    def __init__(self, binary, content, name="Makefile"):
        self.home = tempfile.mkdtemp(prefix="fac_tc_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_tc_work_")
        self.target = os.path.join(self.work, name)
        with open(self.target, "w") as f:
            f.write(content)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.work)
        self.drain(2.2)

    def drain(self, w=0.4):
        end = time.time() + w
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def send(self, d, w=0.3):
        self.child.send(d)
        self.drain(w)

    def caret(self):
        """Where the caret is on SCREEN, and what character is under it."""
        y, x = self.screen.cursor.y, self.screen.cursor.x
        return x + 1, self.screen.display[y][x]

    def to_char(self, line, char_col):
        """Put the caret on a line at a character column, by walking."""
        self.send("\x1b[H", 0.2)
        for _ in range(line - 1):
            self.send("\x1b[B", 0.12)
        self.send("\x1b[H", 0.15)
        for _ in range(char_col - 1):
            self.send("\x1b[C", 0.06)
        self.drain(0.3)

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def test_arrow_up_holds_the_screen_column(binary):
    print("\nArrow-up keeps the caret in the same place on screen")
    for label, content, name, char_col in (
            ("tab indented", TABBED, "Makefile", 9),
            ("space indented", SPACED, "spaced.mk", 13)):
        s = Session(binary, content, name)
        try:
            s.to_char(2, char_col)
            col, ch = s.caret()
            check(ch == "=", f"{label}: the caret starts on the '='", repr(ch))

            s.send("\x1b[A", 0.5)
            up_col, up_ch = s.caret()
            check(up_col == col and up_ch == "=",
                  f"{label}: arrow-up lands on the '=' above",
                  f"col {col} -> {up_col}, on {up_ch!r}")
        finally:
            s.close()


def test_add_cursor_above_holds_the_screen_column(binary):
    print("\nAnd so does a cursor added above")
    s = Session(binary, TABBED, "Makefile")
    try:
        s.to_char(2, 9)
        col, ch = s.caret()
        check(ch == "=", "the caret starts on the '='", repr(ch))

        s.child.send("\x1b[1;8A")          # ctrl+alt+shift+up
        s.drain(1.0)
        new_col, new_ch = s.caret()
        check(new_col == col and new_ch == "=",
              "the added cursor is on the '=' above, not inside `gcc`",
              f"col {col} -> {new_col}, on {new_ch!r}")
    finally:
        s.close()


def test_the_goal_column_survives_a_short_line(binary):
    print("\nThe goal column is not lost passing through a short line")
    s = Session(binary, "a" * 20 + "\nbb\n" + "c" * 20 + "\n", "t.txt")
    try:
        s.to_char(1, 16)
        start, _ = s.caret()

        s.send("\x1b[B", 0.4)
        short, _ = s.caret()
        check(short < start, "the short line clamps the caret",
              f"{start} -> {short}")

        s.send("\x1b[B", 0.4)
        back, _ = s.caret()
        check(back == start, "and the column comes back on the next line",
              f"{start} -> {short} -> {back}")
    finally:
        s.close()


def test_a_tab_indent_beside_a_space_indent(binary):
    """The sharpest form: one line indented with a TAB, the line above it
    indented with SPACES to the same width. Their character columns differ by
    three; their screen columns do not."""
    print("\nA tab-indented line lines up with a space-indented one")
    s = Session(binary, "int f(void)\n    int b = 2;\n\treturn 0;\n", "m.c")
    try:
        # Walk along line 3 until the caret is on the 'r' of return, rather
        # than assuming which character index that is.
        s.to_char(3, 1)
        col = ch = None
        for _ in range(8):
            col, ch = s.caret()
            if ch == "r":
                break
            s.send("\x1b[C", 0.12)
        check(ch == "r", "the caret is on the 'r' of return", repr(ch))

        s.send("\x1b[A", 0.5)
        up_col, up_ch = s.caret()
        check(up_col == col,
              "moving up onto the space-indented line holds the screen column",
              f"{col} -> {up_col}, on {up_ch!r}")

        s.send("\x1b[B", 0.5)
        down_col, down_ch = s.caret()
        check(down_col == col and down_ch == "r",
              "and coming back lands on the same character",
              f"{col} -> {down_col}, on {down_ch!r}")
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_arrow_up_holds_the_screen_column,
               test_add_cursor_above_holds_the_screen_column,
               test_the_goal_column_survives_a_short_line,
               test_a_tab_indent_beside_a_space_indent):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_tab_columns: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_tab_columns: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
