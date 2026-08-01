#!/usr/bin/env python3
"""
Integration test: undo and redo, driven through the real key handling.

Three things were reported and all three were measured before being fixed.

Undo was invisible off screen -- the caret was only restored when the saved
state had a SELECTION, so ordinarily the text changed and the caret did not,
and the viewport had nothing to scroll to.

Redo was not the inverse of undo. A special case jumped to the newest state,
so three undos were followed by one redo that put all three back at once.

And a run of edits of any kind was a single undo as long as the caret never
moved, so typing, then backspacing, then pasting collapsed into one step.

Usage: python3 test/integration_undo.py [path-to-fac-binary]
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

ROWS, COLS = 24, 80
UNDO = "\x1a"
REDO = "\x1d"          # ctrl-] ; ctrl-shift-z is the other binding

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
    def __init__(self, binary, text):
        self.home = tempfile.mkdtemp(prefix="fac_un_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_un_work_")
        self.target = os.path.join(self.work, "t.txt")
        with open(self.target, "w") as f:
            f.write(text)
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

    def send(self, d, w=0.35):
        self.child.send(d)
        self.drain(w)

    def body(self, n=4):
        return [self.screen.display[i].rstrip()[6:] for i in range(1, n + 1)]

    def status(self):
        return self.screen.display[ROWS - 1].rstrip()

    def caret(self):
        m = re.search(r"Ln (\d+), Col (\d+)", self.status())
        return (int(m.group(1)), int(m.group(2))) if m else None

    def saved(self):
        self.send("\x13", 0.8)
        with open(self.target) as f:
            return f.read()

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def undos_until(s, want, limit=8):
    """How many undo presses it takes to get line 1 back to `want`."""
    for n in range(1, limit + 1):
        s.send(UNDO)
        if s.body(1)[0] == want:
            return n
    return -1


def test_an_offscreen_undo_scrolls_to_itself(binary):
    print("\nUndoing something off screen brings it into view")
    s = Session(binary, "".join(f"line{i}\n" for i in range(1, 201)))
    try:
        s.send("\x05")
        s.send("X", 0.5)
        check(s.body(1)[0] == "line1X", "edited line 1", str(s.body(1)))

        for _ in range(12):
            s.send("\x1b[6~", 0.1)          # page down, far away
        far = s.caret()
        check(far is not None and far[0] > 100,
              "scrolled a long way from the edit", str(far))

        s.send(UNDO, 0.7)
        check(s.body(1)[0] == "line1",
              "the undo is on screen afterwards", str(s.body(1)))
        here = s.caret()
        check(here is not None and here[0] == 1,
              "and the caret is at the edit, not where it was left", str(here))
    finally:
        s.close()


def test_redo_steps_one_edit_at_a_time(binary):
    print("\nEach redo restores exactly one edit")
    s = Session(binary, "aaa\nbbb\nccc\n")
    try:
        s.send("\x05"); s.send("1")
        s.send("\x1b[B"); s.send("\x05"); s.send("2")
        s.send("\x1b[B"); s.send("\x05"); s.send("3")
        check(s.body(3) == ["aaa1", "bbb2", "ccc3"], "three edits", str(s.body(3)))

        for _ in range(3):
            s.send(UNDO)
        check(s.body(3) == ["aaa", "bbb", "ccc"], "three undos clear them",
              str(s.body(3)))

        s.send(REDO)
        check(s.body(3) == ["aaa1", "bbb", "ccc"], "the first redo restores ONE",
              str(s.body(3)))
        s.send(REDO)
        check(s.body(3) == ["aaa1", "bbb2", "ccc"], "the second the next",
              str(s.body(3)))
        s.send(REDO)
        check(s.body(3) == ["aaa1", "bbb2", "ccc3"], "the third the last",
              str(s.body(3)))
    finally:
        s.close()


def test_a_typed_run_is_one_undo(binary):
    print("\nA run of typing is a single undo")
    s = Session(binary, "abc\n")
    try:
        s.send("\x05")
        for c in "XYZ":
            s.send(c, 0.2)
        check(s.body(1)[0] == "abcXYZ", "typed a run", str(s.body(1)))
        check(undos_until(s, "abc") == 1, "one press undoes all of it")
    finally:
        s.close()


def test_typing_then_deleting_is_two(binary):
    print("\nChanging the kind of edit ends the run")
    s = Session(binary, "abc\n")
    try:
        s.send("\x05")
        for c in "XYZ":
            s.send(c, 0.2)
        s.send("\x7f", 0.2)
        s.send("\x7f", 0.2)
        check(s.body(1)[0] == "abcX", "typed then deleted", str(s.body(1)))
        check(undos_until(s, "abcXYZ") == 1,
              "the first undo takes back the deletions only")
        check(undos_until(s, "abc") == 1, "and the second the typing")
    finally:
        s.close()


def test_a_pause_ends_the_run(binary):
    print("\nA pause ends the run")
    s = Session(binary, "abc\n")
    try:
        s.send("\x05")
        s.send("X", 0.9)                    # longer than the idle boundary
        s.send("Y", 0.2)
        check(s.body(1)[0] == "abcXY", "typed either side of a pause",
              str(s.body(1)))
        check(undos_until(s, "abcX") == 1, "the pause split them")
        check(undos_until(s, "abc") == 1, "and the earlier one is its own")
    finally:
        s.close()


def test_a_paste_is_its_own_undo(binary):
    print("\nA paste is never merged into the typing around it")
    s = Session(binary, "abc\n")
    try:
        s.send("\x05")
        s.send("X", 0.2)
        s.send("\x1b[200~QQ\x1b[201~", 0.6)
        check(s.body(1)[0] == "abcXQQ", "typed then pasted", str(s.body(1)))
        check(undos_until(s, "abcX") == 1, "one undo takes back the paste")
        check(undos_until(s, "abc") == 1, "and another the typing")
    finally:
        s.close()


def test_undo_after_a_multi_cursor_edit(binary):
    print("\nA multi-cursor edit undoes in one press")
    s = Session(binary, "aaa\naaa\naaa\n")
    try:
        # Alt-click a second cursor on line 2, then type.
        s.child.send("\x1b[<0;9;2M\x1b[<0;9;2m")
        s.drain(0.4)
        s.child.send("\x1b[<8;9;3M\x1b[<8;9;3m")
        s.drain(0.4)
        s.send("Z", 0.5)
        before = s.body(3)
        # Where the Z lands depends on the column the click resolved to; what
        # matters is that TWO lines changed, from one keystroke.
        check(sum(1 for r in before if "Z" in r) == 2,
              "both cursors typed", str(before))

        s.send(UNDO, 0.6)
        check(s.body(3) == ["aaa", "aaa", "aaa"],
              "one undo takes back the whole edit", str(s.body(3)))
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_an_offscreen_undo_scrolls_to_itself,
               test_redo_steps_one_edit_at_a_time,
               test_a_typed_run_is_one_undo,
               test_typing_then_deleting_is_two,
               test_a_pause_ends_the_run,
               test_a_paste_is_its_own_undo,
               test_undo_after_a_multi_cursor_edit):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_undo: FAILED ({len(failures)}): " + ", ".join(failures))
        return 1
    print("integration_undo: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
