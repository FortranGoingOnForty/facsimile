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


def previewing(s):
    """True when row 2 lists a group's members rather than document text."""
    return s.gutter_line(2) is None and s.names[0] in s.row(2)


def motion_bytes(s, row, col, w=0.7):
    """Send bare motion and return how many bytes the editor emitted.

    A repaint is thousands of bytes; staying on the same group should cost
    approximately none. Motion is reported per cell of pointer travel, so a
    repaint per event would be a frame per cell crossed.
    """
    s.child.send(f"\x1b[<35;{col};{row}M")
    total = 0
    end = time.time() + w
    while time.time() < end:
        try:
            data = s.child.read_nonblocking(65536, 0.1)
            total += len(data)
            s.stream.feed(data.decode("utf-8", "replace"))
        except pexpect.TIMEOUT:
            continue
        except pexpect.EOF:
            break
    return total


# The point of the feature: the pointer can reach what it is previewing.
def test_the_preview_survives_moving_into_it(binary):
    s = Session(binary)
    try:
        if not group_some_and_leave(s):
            print("SKIP: could not build a group with a tab outside it")
            return
        base3 = s.gutter_line(3)

        motion(s, 1, 3)                       # onto the group entry
        check(previewing(s), "hovering the entry previews the members", s.row(2))

        motion(s, 2, 3)                       # down into the strip
        check(previewing(s),
              "moving down into the strip KEEPS the preview", s.row(2))
        check(s.gutter_line(3) == base3,
              "and the document still has not moved",
              f"row3 was {base3}, now {s.gutter_line(3)}")

        # Past the last member, still inside the drawn band. Deliberate: the
        # band is one thing to the eye, so it is one region here.
        motion(s, 2, 60)
        check(previewing(s),
              "and the padding past the last member counts as inside", s.row(2))
    finally:
        s.close()


def test_leaving_the_area_hides_the_preview(binary):
    s = Session(binary)
    try:
        if not group_some_and_leave(s):
            print("SKIP: could not build a group with a tab outside it")
            return

        # Down through the strip and on into the document.
        motion(s, 1, 3)
        motion(s, 2, 3)
        check(previewing(s), "previewing from inside the strip", s.row(2))
        motion(s, 3, 20)
        check(not previewing(s),
              "moving on into the document hides it", s.row(2))

        # Sideways off the entry along row 1, without ever entering the strip.
        motion(s, 1, 3)
        check(previewing(s), "previewing again from the entry", s.row(2))
        motion(s, 1, 60)
        check(not previewing(s),
              "moving sideways off the entry hides it", s.row(2))

        # Well below the bar.
        motion(s, 1, 3)
        motion(s, 2, 10)
        motion(s, 4, 10)
        check(not previewing(s), "and so does moving further down", s.row(2))
    finally:
        s.close()


# Motion is reported per CELL of travel once any-motion tracking is on, so a
# frame per event means crossing a terminal costs a hundred frames. A motion
# that changes nothing must therefore draw NOTHING -- not a cheap frame, none.
def test_a_motion_that_changes_nothing_draws_nothing(binary):
    s = Session(binary)
    try:
        if not group_some_and_leave(s):
            print("SKIP: could not build a group with a tab outside it")
            return

        # Over the document, no preview up: nothing anywhere differs.
        idle = motion_bytes(s, 6, 20) + motion_bytes(s, 6, 24)
        check(idle == 0, "idle pointer motion emits no bytes at all",
              f"{idle} bytes")

        appearing = motion_bytes(s, 1, 3)      # state changes: must draw
        check(appearing > 0, "showing the preview does draw", f"{appearing} bytes")
        check(previewing(s), "and the preview is up", s.row(2))

        staying = motion_bytes(s, 2, 8) + motion_bytes(s, 2, 12)
        check(staying == 0,
              "and moving within the strip emits nothing either",
              f"{staying} bytes")
        check(previewing(s), "with the preview still up", s.row(2))

        leaving = motion_bytes(s, 6, 20)       # state changes: must draw
        check(leaving > 0, "hiding it draws again", f"{leaving} bytes")
        check(not previewing(s), "and it is gone", s.row(2))
    finally:
        s.close()


def test_row_two_inside_a_group_is_not_a_preview_area(binary):
    """Inside a group, row 2 is the pinned member row and no preview exists.

    The rect must be armed only when a preview was actually drawn, or hovering
    there would keep alive a hover for a strip that is not on screen.
    """
    s = Session(binary)
    try:
        open_all(s)
        s.palette("Group All Tabs")
        if "(" not in s.row(1):
            print("SKIP: could not form the group")
            return
        check(s.gutter_line(3) == 1, "inside a group the bar is two rows",
              s.row(3))
        before = s.row(2)
        motion(s, 2, 10)
        check(s.row(2) == before,
              "hovering the pinned row changes nothing", f"{before!r} -> {s.row(2)!r}")
        check(s.gutter_line(3) == 1, "and the document stays put", s.row(3))
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


def test_super_ctrl_arrows_navigate_without_moving_the_caret(binary):
    """Modifier 13 used to fall through to an empty prefix, so the terminator
    was appended to nothing and super+ctrl+left arrived as a bare 'left'. It
    moved the caret instead of doing nothing."""
    s = Session(binary)
    try:
        open_all(s)
        first = s.status()
        cy, cx = s.screen.cursor.y, s.screen.cursor.x

        s.child.send("\x1b[1;13D")            # super+ctrl+left
        s.drain(0.9)
        check(s.status() != first, "super+ctrl+left moves to another entry",
              f"{first[:60]!r} -> {s.status()[:60]!r}")
        check((s.screen.cursor.y, s.screen.cursor.x) == (cy, cx),
              "and does NOT move the caret",
              f"{(cy, cx)} -> {(s.screen.cursor.y, s.screen.cursor.x)}")

        s.child.send("\x1b[1;13C")            # super+ctrl+right
        s.drain(0.9)
        check(s.status() == first, "super+ctrl+right comes back", s.status()[:60])
    finally:
        s.close()


def test_lock_states_do_not_break_the_chord(binary):
    """Num lock adds 128 to the modifier. A case table could not cover that;
    the bitmask decode ignores the high bits."""
    s = Session(binary)
    try:
        open_all(s)
        first = s.status()
        s.child.send("\x1b[1;141D")           # super+ctrl+left with num lock
        s.drain(0.9)
        check(s.status() != first,
              "super+ctrl+left still navigates with num lock on",
              f"{first[:60]!r} -> {s.status()[:60]!r}")
    finally:
        s.close()


def test_the_old_chords_still_do_what_they_did(binary):
    """Modifiers 2..7 are pinned byte-for-byte, so nothing that worked before
    may change."""
    s = Session(binary)
    try:
        open_all(s)
        s.send("\x1b[C" * 6, 0.5)             # move into the line
        before = s.screen.cursor.x
        s.child.send("\x1b[1;5D")             # ctrl+left: word left
        s.drain(0.6)
        check(s.screen.cursor.x < before,
              "ctrl+left still moves by word", f"{before} -> {s.screen.cursor.x}")

        first = s.status()
        s.child.send("\x1b[6;5~")             # ctrl+pagedown
        s.drain(0.9)
        check(s.status() != first, "ctrl+pagedown still changes entry",
              s.status()[:60])
    finally:
        s.close()


def with_subdir(s):
    """Add a directory of files so the tree has a directory row to Enter on."""
    d = os.path.join(s.ws, "lib")
    os.makedirs(d, exist_ok=True)
    for n in ("one.c", "two.c", "three.c"):
        with open(os.path.join(d, n), "w") as f:
            f.write("int x;\n")
    return d


def open_picker_on_lib(s):
    """Enter on the lib/ row in the tree. False if it could not be reached."""
    with_subdir(s)
    s.send("\x02", 1.2)
    for _ in range(20):
        sel = s.tree_selection()
        if sel and "lib" in sel:
            break
        s.send("\x1b[B", 0.15)
    else:
        return False
    s.send("\r", 1.4)
    return "New Tab Group" in "\n".join(s.screen.display)


def test_enter_on_a_directory_opens_the_dialog(binary):
    """It used to do nothing at all: the handler guarded on is_directory with
    no else branch."""
    s = Session(binary)
    try:
        check(open_picker_on_lib(s), "Enter on a directory opens the group dialog",
              "\n".join(r for r in s.screen.display if r.strip())[:400])
        body = "\n".join(s.screen.display)
        check("Name  lib/" in body, "the name is pre-filled from the directory", body[:300])
        check("[ ] one.c" in body, "its files are listed with checkboxes", body[:300])
        check("0 selected" in body, "and nothing is ticked yet", body[:300])
    finally:
        s.close()


# Reported: only first-level directories opened the dialog. The selectable
# list stored a directory's NAME where a file got its full path, and at depth 1
# those are the same string -- so 'ch5' resolved and 'ch5/printaf' did not.
def test_enter_works_on_a_nested_directory(binary):
    s = Session(binary)
    try:
        deep = os.path.join(s.ws, "ch5", "printaf")
        os.makedirs(deep, exist_ok=True)
        for n in ("main.c", "util.c"):
            with open(os.path.join(deep, n), "w") as f:
                f.write("int x;\n")

        s.send("\x02", 1.2)
        for _ in range(20):                     # to ch5/
            sel = s.tree_selection()
            if sel and "ch5" in sel:
                break
            s.send("\x1b[B", 0.15)
        else:
            check(False, "could not reach ch5/ in the tree", s.row(3))
            return

        s.send("\x1b[C", 1.2)                   # right: descend into ch5/
        for _ in range(20):                     # to printaf/
            sel = s.tree_selection()
            if sel and "printaf" in sel:
                break
            s.send("\x1b[B", 0.15)
        else:
            check(False, "could not reach printaf/ inside ch5/",
                  "\n".join(r for r in s.screen.display if r.strip())[:300])
            return

        s.send("\r", 1.5)
        body = "\n".join(s.screen.display)
        check("New Tab Group" in body,
              "Enter on a nested directory opens the dialog too", body[:400])
        check("Name  printaf/" in body,
              "pre-filled from the nested directory, not its parent", body[:300])
        check("[ ] main.c" in body,
              "and it lists the files actually inside it", body[:300])
    finally:
        s.close()


def test_ticking_and_creating(binary):
    s = Session(binary)
    try:
        if not open_picker_on_lib(s):
            print("SKIP: could not open the dialog")
            return
        s.send("\t", 0.4)                     # focus the list
        s.send("\x1b[B", 0.3)                 # past ../
        s.send(" ", 0.4)                       # tick
        s.send(" ", 0.4)                       # tick
        check("2 selected" in "\n".join(s.screen.display),
              "space ticks files", "\n".join(s.screen.display)[:300])

        s.send("\r", 1.8)                     # create
        check("New Tab Group" not in "\n".join(s.screen.display),
              "the dialog closes on create", s.row(2))
        check("(2)" in s.row(1), "row 1 shows the new group with its count", s.row(1))
        check("one.c" in s.row(2), "row 2 lists the members it opened", s.row(2))
    finally:
        s.close()


def test_closing_a_member_does_not_crash(binary):
    """Ctrl-W inside a group used to segfault the editor.

    Group members are read from disk lazily, so all but the one you land on
    have no buffer at all. close_tab_without_prompt copied the NEXT tab's
    pane buffer straight out, and the next tab after closing a member is
    normally one of the deferred ones -- a null dereference. The code
    predates deferred tabs, so nothing was watching for it.

    Nothing about editing a group is needed to reach this: open a group,
    press Ctrl-W, and the editor dies.
    """
    s = Session(binary)
    try:
        if not open_picker_on_lib(s):
            print("SKIP: could not open the dialog")
            return
        s.send("\t", 0.4)
        s.send("\x1b[B", 0.3)
        s.send(" ", 0.4)
        s.send(" ", 0.4)
        s.send(" ", 0.4)                      # three members: two deferred
        s.send("\r", 1.8)
        check("(3)" in s.row(1), "a group of three exists", s.row(1))

        s.send("\x17", 2.0)                   # ctrl-w on the active member
        check(s.child.isalive(), "the editor survived closing a member",
              f"signal {s.child.signalstatus}")
        check("(2)" in s.row(1), "and the group is down to two", s.row(1))

        # It has to have landed somewhere real, not on an empty buffer that
        # a later save would write over the file.
        s.send("\x17", 2.0)
        check(s.child.isalive(), "and again for the last deferred member",
              f"signal {s.child.signalstatus}")
    finally:
        s.close()


def test_escape_creates_nothing(binary):
    s = Session(binary)
    try:
        if not open_picker_on_lib(s):
            print("SKIP: could not open the dialog")
            return
        before = s.row(1)
        s.send("\t", 0.4)
        s.send("\x1b[B", 0.3)
        s.send(" ", 0.4)
        s.send("\x1b", 1.2)                   # esc
        check("New Tab Group" not in "\n".join(s.screen.display),
              "escape closes the dialog", s.row(2))
        check("(" not in s.row(1) or s.row(1) == before,
              "and creates no group", f"{before!r} -> {s.row(1)!r}")
    finally:
        s.close()


def test_ctrl_q_is_not_trapped(binary):
    """No modal may trap the user."""
    s = Session(binary)
    try:
        if not open_picker_on_lib(s):
            print("SKIP: could not open the dialog")
            return
        s.send("\x11", 1.5)                   # ctrl-q
        alive = s.child.isalive()
        # Either it quit, or it is showing a save prompt -- both mean ctrl-q
        # reached the editor rather than being swallowed.
        body = "\n".join(s.screen.display)
        check(not alive or "New Tab Group" not in body,
              "ctrl-q passes through the dialog", body[:200])
    finally:
        s.close()


def test_groups_survive_a_restart(binary):
    """Workspace state is only saved when fac was opened on a DIRECTORY, so
    this runs in workspace mode rather than on a single file."""
    s = Session(binary)
    home, ws = s.home, s.ws
    try:
        # Everything happens in workspace mode: state is only written when fac
        # was opened on a directory.
        s.child.terminate(force=True)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": home}
        env.pop("XDG_CONFIG_HOME", None)
        s.screen = pyte.Screen(COLS, ROWS)
        s.stream = pyte.Stream(s.screen)
        s.child = pexpect.spawn(binary, [ws], dimensions=(ROWS, COLS),
                                env=env, cwd=ws)
        s.drain(2.2)
        open_all(s)
        s.palette("Group All Tabs")
        before = s.row(1)
        if "(" not in before:
            print("SKIP: no tabs restored to group in workspace mode")
            return
        s.send("\x11", 1.6)                    # ctrl-q
        s.drain(1.5)

        state = os.path.join(ws, ".fac", "workspace.json")
        check(os.path.exists(state), "quitting writes the workspace file", state)
        if not os.path.exists(state):
            return
        text = open(state).read()
        # 1.2 adds the terminal panel height; tab groups arrived in 1.1 and
        # every addition since has been additive, so the number only goes up.
        check('"version": "1.2"' in text, "at schema version 1.2", text[:200])
        check('"tab_groups"' in text, "with a tab_groups array", text[:200])
        check(text.index('"tab_groups"') < text.index('"tabs"'),
              "written before the tabs, so members can reference it")

        # Reopen once more: the group should come back.
        s.screen = pyte.Screen(COLS, ROWS)
        s.stream = pyte.Stream(s.screen)
        s.child = pexpect.spawn(binary, [ws], dimensions=(ROWS, COLS),
                                env=env, cwd=ws)
        s.drain(2.5)
        check(re.search(r"\(\d+\)", s.row(1)) is not None,
              "the group is back after a restart", s.row(1))
        check(s.row(1) == before, "with the same label and count",
              f"{before!r} -> {s.row(1)!r}")
    finally:
        s.close()


def test_an_old_state_file_still_loads(binary):
    """A 1.0 file has no tab_groups key and no group on its tabs. It must
    restore exactly as it always did, with no migration step."""
    s = Session(binary)
    home, ws = s.home, s.ws
    try:
        s.child.terminate(force=True)
        os.makedirs(os.path.join(ws, ".fac"), exist_ok=True)
        with open(os.path.join(ws, ".fac", "workspace.json"), "w") as f:
            f.write('{\n'
                    '  "version": "1.0",\n'
                    f'  "workspace_path": "{ws}",\n'
                    '  "last_opened": "2026-01-01",\n'
                    '  "tabs": [\n'
                    '    {\n'
                    '      "filename": "file01.txt", \n'
                    '      "is_orphan": false, \n'
                    '      "modified": false, \n'
                    '      "panes": [\n'
                    '        {\n'
                    '          "x_start": 0.0000,\n'
                    '          "y_start": 0.0000,\n'
                    '          "x_end": 1.0000,\n'
                    '          "y_end": 1.0000,\n'
                    '          "filename": "file01.txt",\n'
                    '          "cursor_line": 1,\n'
                    '          "cursor_column": 1,\n'
                    '          "viewport_line": 1,\n'
                    '          "viewport_column": 1\n'
                    '        }\n'
                    '      ],\n'
                    '      "active_pane": 1\n'
                    '    }\n'
                    '  ],\n'
                    '  "active_tab": 1,\n'
                    '  "fuss_mode": { "active": false, "width": 30 }\n'
                    '}\n')

        env = {**os.environ, "TERM": "xterm-256color", "HOME": home}
        env.pop("XDG_CONFIG_HOME", None)
        s.screen = pyte.Screen(COLS, ROWS)
        s.stream = pyte.Stream(s.screen)
        s.child = pexpect.spawn(binary, [ws], dimensions=(ROWS, COLS),
                                env=env, cwd=ws)
        s.drain(2.5)
        check("file01.txt" in s.row(1),
              "a 1.0 state file restores its tab", s.row(1))
        check(re.search(r"\(\d+\)", s.row(1)) is None,
              "and produces no groups", s.row(1))
    finally:
        s.close()


def active_member(s):
    """Which file is showing, from the status bar path."""
    m = re.search(r"(file\d+\.txt)", s.status())
    return m.group(1) if m else None


def inside_group(s):
    """A two-row tab bar means we are inside a group.

    Row 3 carries buffer line 1 when the bar is two rows, line 2 when it is
    one -- so the gutter reports containment without trusting a label.
    """
    return s.gutter_line(3) == 1


def test_left_right_walks_the_members_then_exits(binary):
    """Left/right is one continuous line through every open file.

    A group is not a single stop: stepping into one lands on its edge member
    and each further press advances within it, until stepping off the last
    member leaves. That last part is what makes ctrl-pagedown a way out of a
    group when the window manager has eaten super+ctrl+up.
    """
    s = Session(binary)
    try:
        if not group_some_and_leave(s):
            check(False, "walk: could not form the group")
            return
        check(active_member(s) == "file05.txt",
              "starts outside the group on file05", active_member(s))
        check(not inside_group(s), "and the bar is one row")

        # Right: into the group at its FIRST member, then along it.
        seen = []
        for _ in range(6):
            s.send("\x1b[6;5~", 0.85)
            seen.append((active_member(s), inside_group(s)))

        check(seen[0] == ("file01.txt", True),
              "stepping right enters the group at its first member", seen[0])
        check([m for m, _ in seen[:4]] ==
              ["file01.txt", "file02.txt", "file03.txt", "file04.txt"],
              "and then walks the members in order", [m for m, _ in seen[:4]])
        check(all(g for _, g in seen[:4]), "staying inside throughout", seen[:4])
        check(seen[4] == ("file05.txt", False),
              "stepping off the last member LEAVES the group", seen[4])
        check(seen[5] == ("file01.txt", True),
              "and the walk wraps back round", seen[5])
    finally:
        s.close()


def test_the_walk_is_reversible(binary):
    """Retrace the steps and you visit the same files in reverse.

    Only true because entering from the right lands on the LAST member rather
    than the remembered one -- otherwise walking left out of a group and back
    in would skip every member between the edge and where you last were.
    """
    s = Session(binary)
    try:
        if not group_some_and_leave(s):
            check(False, "reverse: could not form the group")
            return

        # Entering from the right must land on the LAST member. This is the
        # assertion that actually discriminates: a group treated as a single
        # stop is symmetric, so a bare there-and-back check passes even when
        # left/right does nothing but toggle in and out.
        s.send("\x1b[5;5~", 0.9)           # left, from file05 into the group
        check(active_member(s) == "file04.txt",
              "stepping LEFT into a group lands on its LAST member",
              active_member(s))
        check(inside_group(s), "and really is inside it")

        s.send("\x1b[6;5~", 0.9)           # right, straight back out
        check(active_member(s) == "file05.txt" and not inside_group(s),
              "and stepping right returns whence it came", active_member(s))

        forward = []
        for _ in range(5):
            s.send("\x1b[6;5~", 0.85)
            forward.append(active_member(s))
        check(forward == ["file01.txt", "file02.txt", "file03.txt",
                          "file04.txt", "file05.txt"],
              "the forward walk visits five distinct files", forward)

        backward = []
        for _ in range(5):
            s.send("\x1b[5;5~", 0.85)
            backward.append(active_member(s))

        # Walking back re-treads the forward path: the last forward step is
        # already where we stand, so the return trip is the rest, reversed,
        # ending where the walk began.
        check(backward == list(reversed(forward[:-1])) + ["file05.txt"],
              "walking left retraces the walk right",
              f"fwd={forward} back={backward}")
        check(active_member(s) == "file05.txt",
              "landing back where it started", active_member(s))
    finally:
        s.close()


def test_leaving_from_the_middle_still_takes_one_press(binary):
    """super+ctrl+up leaves from any member, not just an edge one.

    The member walk must not have made the explicit exit worse: it is the
    binding that gets you out of a forty-file group in one press.
    """
    s = Session(binary)
    try:
        if not group_some_and_leave(s):
            check(False, "middle-exit: could not form the group")
            return
        s.send("\x1b[6;5~", 0.85)          # into the group, member 1
        s.send("\x1b[6;5~", 0.85)          # member 2 -- the middle
        check(inside_group(s) and active_member(s) == "file02.txt",
              "sitting on a middle member", active_member(s))

        s.send("\x1b[1;13A", 0.9)          # super+ctrl+up
        check(not inside_group(s), "super+ctrl+up leaves from the middle",
              f"still inside, on {active_member(s)}")
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
               test_the_preview_survives_moving_into_it,
               test_leaving_the_area_hides_the_preview,
               test_a_motion_that_changes_nothing_draws_nothing,
               test_row_two_inside_a_group_is_not_a_preview_area,
               test_the_preview_clears,
               test_a_keystroke_dismisses_the_preview,
               test_super_ctrl_arrows_navigate_without_moving_the_caret,
               test_left_right_walks_the_members_then_exits,
               test_the_walk_is_reversible,
               test_leaving_from_the_middle_still_takes_one_press,
               test_lock_states_do_not_break_the_chord,
               test_the_old_chords_still_do_what_they_did,
               test_enter_on_a_directory_opens_the_dialog,
               test_enter_works_on_a_nested_directory,
               test_ticking_and_creating,
               test_closing_a_member_does_not_crash,
               test_escape_creates_nothing,
               test_ctrl_q_is_not_trapped,
               test_groups_survive_a_restart,
               test_an_old_state_file_still_loads):
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
