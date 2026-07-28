#!/usr/bin/env python3
"""
Integration test: a tab group on screen.

Row 1 collapses a group's tabs into one entry carrying a live count; row 2
lists the members; the document reflows down to make room. The reflow is the
part most likely to break silently -- the bar's height feeds viewport
scrolling, caret placement and page size, and when one of those disagrees the
caret walks off the last drawn line.

Usage: python3 test/integration_tabgroups.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import glob
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

ROWS, COLS = 24, 100
N_FILES = 5
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
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    cand = os.path.join(root, "fac")
    if os.path.exists(cand):
        return cand
    for p in glob.glob(os.path.join(root, "build", "gfortran_*", "app", "fac")):
        return p
    print("SKIP: no fac binary found (run make first)")
    sys.exit(0)


class Session:
    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_tg_")
        cfg = os.path.join(self.home, ".config", "fac")
        os.makedirs(cfg)
        with open(os.path.join(cfg, "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.ws = os.path.join(self.home, "ws")
        os.makedirs(self.ws)
        self.names = [f"file{i:02d}.txt" for i in range(1, N_FILES + 1)]
        for n in self.names:
            with open(os.path.join(self.ws, n), "w") as f:
                f.write("".join(f"{n} line {i}\n" for i in range(1, 60)))
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [os.path.join(self.ws, self.names[0])],
                                   dimensions=(ROWS, COLS), env=env, cwd=self.ws)
        self.drain(1.6)

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

    def send(self, data, w=0.5):
        self.child.send(data)
        self.drain(w)

    def row(self, n):
        return self.screen.display[n - 1].rstrip()

    def status(self):
        return self.screen.display[ROWS - 1].rstrip()

    def in_tree(self):
        return self.status().startswith("«")

    def tree_selection(self):
        for y in range(1, ROWS - 1):
            r = self.screen.buffer[y]
            cells = "".join(r[x].data if r[x].reverse else ""
                            for x in range(30)).strip()
            if len(cells) > 1 and cells not in ("✗", "↑"):
                return cells
        return None

    def gutter_line(self, screen_row):
        """The buffer line number drawn in the gutter of `screen_row`, or None."""
        m = re.match(r"\s*(\d+)\s", self.row(screen_row))
        return int(m.group(1)) if m else None

    def palette(self, name):
        self.send("\x10", 0.7)
        self.send(name, 0.6)
        self.send("\r", 1.2)

    def close(self):
        try:
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)


def open_all(s):
    """Open every file as its own tab, via the file tree."""
    for n in s.names[1:]:
        if not s.in_tree():
            s.send("\x02", 0.8)
        if not s.in_tree():
            break
        for _ in range(2 * N_FILES + 8):
            sel = s.tree_selection()
            if sel and sel.split()[0] == n:
                break
            s.send("\x1b[B", 0.12)
        s.send("\r", 0.7)
    if s.in_tree():
        s.send("\x02", 0.7)


def test_row_one_collapses_the_group(binary):
    s = Session(binary)
    try:
        open_all(s)
        before = s.row(1)
        check(any(n in before for n in s.names),
              "before grouping, row 1 lists individual tabs", before)

        s.palette("Group All Tabs")
        check(re.search(r"\(\d+\)", s.row(1)) is not None,
              "row 1 shows a group entry with a count", s.row(1))
        check(f"({N_FILES})" in s.row(1),
              f"and the count is the live member total ({N_FILES})", s.row(1))
        check(not any(n in s.row(1) for n in s.names),
              "members no longer appear individually on row 1", s.row(1))
    finally:
        s.close()


def test_row_two_lists_the_members(binary):
    s = Session(binary)
    try:
        open_all(s)
        s.palette("Group All Tabs")
        row2 = s.row(2)
        missing = [n for n in s.names if n not in row2]
        check(not missing, "row 2 lists every member", f"{row2!r} missing {missing}")
    finally:
        s.close()


def test_the_document_reflows_down(binary):
    """The bar's height feeds viewport scrolling, caret placement and page
    size. If any of them disagrees the caret walks off the last drawn line."""
    s = Session(binary)
    try:
        open_all(s)
        check(s.gutter_line(2) == 1,
              "without a group the document starts on row 2", s.row(2))

        s.palette("Group All Tabs")
        check(s.gutter_line(2) is None,
              "with a group, row 2 is no longer document text", s.row(2))
        check(s.gutter_line(3) == 1,
              "the document starts on row 3 instead", s.row(3))
    finally:
        s.close()


def test_the_caret_stays_on_a_drawn_line(binary):
    """Arrow down through the whole file and assert the caret never leaves the
    drawn region -- the failure mode when the bar's height is not respected."""
    s = Session(binary)
    try:
        open_all(s)
        s.palette("Group All Tabs")
        bad = []
        for step in range(40):
            s.send("\x1b[B", 0.06)
            y = s.screen.cursor.y + 1
            if y < 3 or y > ROWS - 1:
                bad.append((step, y))
        check(not bad,
              "the caret stays between the group row and the status bar",
              str(bad[:5]))
    finally:
        s.close()


def test_the_count_follows_a_close(binary):
    s = Session(binary)
    try:
        open_all(s)
        s.palette("Group All Tabs")
        check(f"({N_FILES})" in s.row(1), "starts at the full count", s.row(1))
        s.send("\x17", 1.2)                       # ctrl-w: close a tab
        check(f"({N_FILES - 1})" in s.row(1),
              "the count drops when a member closes", s.row(1))
    finally:
        s.close()


def group_some_and_leave(s):
    """Group the first four files and leave file05 outside, then sit on it.

    A preview only makes sense for a group you are NOT in -- inside one, the
    member row is pinned instead.
    """
    for n in s.names[1:4]:
        if not s.in_tree():
            s.send("\x02", 0.8)
        if not s.in_tree():
            return False
        for _ in range(2 * N_FILES + 8):
            sel = s.tree_selection()
            if sel and sel.split()[0] == n:
                break
            s.send("\x1b[B", 0.12)
        s.send("\r", 0.7)
    if s.in_tree():
        s.send("\x02", 0.7)
    s.palette("Group All Tabs")
    # open the last file: it lands outside the group
    if not s.in_tree():
        s.send("\x02", 0.8)
    for _ in range(2 * N_FILES + 8):
        sel = s.tree_selection()
        if sel and sel.split()[0] == s.names[-1]:
            break
        s.send("\x1b[B", 0.12)
    s.send("\r", 0.8)
    if s.in_tree():
        s.send("\x02", 0.7)
    return "(4)" in s.row(1)


def motion(s, row, col, w=0.9):
    """Bare pointer motion: mode 1003 reports it as button 35 (32 + no button)."""
    s.child.send(f"\x1b[<35;{col};{row}M")
    s.drain(w)


def test_hover_previews_without_reflowing(binary):
    """The whole point of an overlay: the document must not shift as the
    pointer crosses the bar."""
    s = Session(binary)
    try:
        if not group_some_and_leave(s):
            print("SKIP: could not build a group with a tab outside it")
            return
        before_row3 = s.gutter_line(3)
        check(s.gutter_line(2) is not None,
              "outside the group, row 2 is document text", s.row(2))

        motion(s, 1, 3)                       # over the group entry
        check(s.names[0] in s.row(2),
              "hovering a group previews its members on row 2", s.row(2))
        check(s.gutter_line(3) == before_row3,
              "and the document does NOT move",
              f"row3 was line {before_row3}, now {s.gutter_line(3)}")
    finally:
        s.close()


def test_the_preview_clears(binary):
    s = Session(binary)
    try:
        if not group_some_and_leave(s):
            print("SKIP: could not build a group with a tab outside it")
            return
        original = s.row(2)
        motion(s, 1, 3)
        check(s.row(2) != original, "the preview appeared", s.row(2))

        motion(s, 12, 40)                     # pointer off the bar
        check(s.gutter_line(2) is not None,
              "moving off the bar restores the document row", s.row(2))
    finally:
        s.close()


def test_a_keystroke_dismisses_the_preview(binary):
    """The pointer can leave the terminal without a final motion event, which
    would otherwise strand the overlay."""
    s = Session(binary)
    try:
        if not group_some_and_leave(s):
            print("SKIP: could not build a group with a tab outside it")
            return
        motion(s, 1, 3)
        check(s.names[0] in s.row(2), "the preview is up", s.row(2))
        s.send("\x1b[B", 0.8)                 # any key
        check(s.gutter_line(2) is not None,
              "a keystroke clears it", s.row(2))
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_row_one_collapses_the_group,
               test_row_two_lists_the_members,
               test_the_document_reflows_down,
               test_the_caret_stays_on_a_drawn_line,
               test_the_count_follows_a_close,
               test_hover_previews_without_reflowing,
               test_the_preview_clears,
               test_a_keystroke_dismisses_the_preview):
        try:
            fn(binary)
        except Exception as exc:            # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    if failures:
        print(f"\nintegration_tabgroups: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("\nintegration_tabgroups: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
