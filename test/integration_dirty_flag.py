#!/usr/bin/env python3
"""
Integration test: the unsaved-changes asterisk means the text differs from
what was saved.

Reported: open a file, type something, see the asterisk, undo the change
without saving so the file is byte-for-byte what it started as -- and the
asterisk stays. It had nowhere to go: modified was a one-way latch, set by
buffer_insert/buffer_delete and cleared only by a save.

Two places had to stop latching, which is why fixing one looked like it did
nothing. The tab's flag is now derived from a signature of the text taken at
load or save; and the buffer's own flag is corrected alongside it, because
the main loop copies buffer%modified straight onto the tab on every turn and
would otherwise put the stale claim back a moment later.

Usage: python3 test/integration_dirty_flag.py [path-to-fac-binary]
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
    def __init__(self, binary, text="int one;\nint two;\nint three;\n"):
        self.home = tempfile.mkdtemp(prefix="fac_df_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_df_work_")
        self.target = os.path.join(self.work, "t.c")
        with open(self.target, "w") as f:
            f.write(text)
        self.original = text
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

    def send(self, d, w=0.5):
        self.child.send(d)
        self.drain(w)

    def dirty(self):
        """Is the tab bar showing an unsaved-changes marker?"""
        return any(mark in self.screen.display[0] for mark in ("*", "●"))

    def body(self, n=3):
        return [self.screen.display[i].rstrip()[6:] for i in range(1, n + 1)]

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def test_undoing_the_only_edit_clears_the_mark(binary):
    print("\nUndoing the one edit made clears the asterisk")
    s = Session(binary)
    try:
        before = s.body()
        check(not s.dirty(), "the file starts clean")

        s.send("Z", 0.7)
        check(s.dirty(), "typing marks it dirty")
        check(s.body() != before, "and the text really changed", str(s.body()))

        s.send("\x1a", 0.9)                    # undo
        check(s.body() == before, "undo restored the text", str(s.body()))
        check(not s.dirty(),
              "so the asterisk is gone -- the file matches what is on disk")
    finally:
        s.close()


def test_a_run_of_edits_undone_clears_the_mark(binary):
    print("\nSo does undoing several edits back to the start")
    s = Session(binary)
    try:
        before = s.body()
        for c in "abc":
            s.send(c, 0.25)
        s.send("\x1b[B", 0.25)                 # break the run
        for c in "de":
            s.send(c, 0.25)
        check(s.dirty(), "several edits, dirty")

        for _ in range(8):
            s.send("\x1a", 0.4)
            if s.body() == before:
                break
        check(s.body() == before, "undone back to the original", str(s.body()))
        check(not s.dirty(), "and clean again")
    finally:
        s.close()


def test_redo_makes_it_dirty_again(binary):
    print("\nRedoing the change brings the asterisk back")
    s = Session(binary)
    try:
        before = s.body()
        s.send("Z", 0.7)
        s.send("\x1a", 0.9)
        check(not s.dirty(), "clean after the undo")
        s.send("\x1d", 0.9)                    # ctrl-] redo
        check(s.body() != before, "redo put the change back", str(s.body()))
        check(s.dirty(), "so it is dirty again")
    finally:
        s.close()


def test_an_edit_that_is_not_undone_stays_dirty(binary):
    print("\nA real change still reads as a real change")
    s = Session(binary)
    try:
        s.send("Z", 0.7)
        s.send("Y", 0.5)
        s.send("\x1a", 0.9)                    # undo only part of it
        check(s.dirty(), "still dirty while any change remains", str(s.body()))
        with open(s.target) as f:
            check(f.read() == s.original,
                  "and nothing was written to disk behind our back")
    finally:
        s.close()


def test_saving_then_editing_and_undoing(binary):
    print("\nAfter a save, the saved text is the new clean state")
    s = Session(binary)
    try:
        s.send("Z", 0.6)
        s.send("\x13", 1.0)                    # ctrl-s
        check(not s.dirty(), "clean right after saving")
        saved = s.body()

        s.send("Q", 0.6)
        check(s.dirty(), "editing after the save marks it again")
        s.send("\x1a", 0.9)
        check(s.body() == saved, "undo returns to the SAVED text", str(s.body()))
        check(not s.dirty(), "which is clean, not the pre-save text")
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_undoing_the_only_edit_clears_the_mark,
               test_a_run_of_edits_undone_clears_the_mark,
               test_redo_makes_it_dirty_again,
               test_an_edit_that_is_not_undone_stays_dirty,
               test_saving_then_editing_and_undoing):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_dirty_flag: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_dirty_flag: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
