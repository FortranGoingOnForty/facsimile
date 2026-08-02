#!/usr/bin/env python3
"""
Integration test: dragging a tab to reorder it.

Click and hold a tab, move, release. A ghost follows the pointer, the bar
shows where the tab would land, and the drop commits it.

The design that everything here rests on: **nothing in the tabs array changes
until the release**. The drag carries a target and the renderer draws the bar
as if the held entry were already there. That is why "release off the bar
changes nothing" is a one-line guarantee rather than an undo, and it is what
the snap-back cases below are really testing.

SGR encoding used throughout:
    press    \x1b[<0;COL;ROWM      left button down
    move     \x1b[<32;COL;ROWM     32 = motion bit, low bits 0 = left held
    release  \x1b[<0;COL;ROWm

Usage: python3 test/integration_tabdrag.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import json
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

ROWS, COLS = 30, 120

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
    NAMES = ("alpha.c", "beta.c", "gamma.c", "delta.c")

    def __init__(self, binary, n=3):
        self.home = tempfile.mkdtemp(prefix="fac_td_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.root = tempfile.mkdtemp(prefix="fac_td_work_")
        self.work = os.path.join(self.root, "ws")
        os.makedirs(self.work)
        self.names = self.NAMES[:n]
        for nm in self.names:
            with open(os.path.join(self.work, nm), "w") as f:
                f.write("int %s;\n" % nm[:-2])

        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [os.path.join(self.work, self.names[0])],
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

    def send(self, d, w=0.6):
        self.child.send(d)
        self.drain(w)

    def click(self, row, col, button=0):
        self.child.send(f"\x1b[<{button};{col};{row}M")
        self.drain(0.25)
        self.child.send(f"\x1b[<{button};{col};{row}m")
        self.drain(0.8)

    def open_all(self):
        self.send("\x02", 1.2)
        for nm in self.names[1:]:
            hit = self.find(nm)
            if hit:
                self.click(hit[0], hit[1] + 1)
            self.drain(0.5)
        self.send("\x02", 1.0)

    def find(self, text):
        for y, r in enumerate(self.screen.display):
            j = r.find(text)
            if j >= 0:
                return y + 1, j + 1
        return None

    def text(self):
        return "\n".join(self.screen.display)

    def tab_bar(self):
        return self.screen.display[0].rstrip()

    def row2(self):
        return self.screen.display[1].rstrip()

    def status(self):
        return self.screen.display[ROWS - 1].rstrip()

    def order(self):
        """Tab names in bar order, ignoring the numbering."""
        return re.findall(r"\[\d+:\s*([\w.]+)", self.tab_bar())

    def entry_col(self, text, row=1):
        j = self.screen.display[row - 1].find(text)
        return j + 1 if j >= 0 else None

    def drag(self, from_col, to_col, from_row=1, to_row=None, release=True):
        """Press, walk the pointer across, release."""
        to_row = from_row if to_row is None else to_row
        self.child.send(f"\x1b[<0;{from_col};{from_row}M")
        self.drain(0.4)
        step = 1 if to_col >= from_col else -1
        for col in range(from_col + step, to_col + step, step * 2):
            self.child.send(f"\x1b[<32;{col};{from_row}M")
            self.drain(0.1)
        if to_row != from_row:
            self.child.send(f"\x1b[<32;{to_col};{to_row}M")
            self.drain(0.3)
        if release:
            self.child.send(f"\x1b[<0;{to_col};{to_row}m")
            self.drain(1.0)

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.root, ignore_errors=True)


def test_drag_right_and_left(binary):
    print("\nA tab can be carried along the bar in either direction")
    s = Session(binary)
    try:
        s.open_all()
        check(s.order() == ["alpha.c", "beta.c", "gamma.c"],
              "three tabs in the order they were opened", str(s.order()))

        s.drag(s.entry_col("alpha.c"), s.entry_col("gamma.c"))
        check(s.order() == ["beta.c", "gamma.c", "alpha.c"],
              "dragged to the end, the others keeping their order",
              str(s.order()))

        s.drag(s.entry_col("alpha.c"), s.entry_col("beta.c"))
        check(s.order() == ["alpha.c", "beta.c", "gamma.c"],
              "and back to the front", str(s.order()))
    finally:
        s.close()


def test_a_click_is_still_a_click(binary):
    print("\nPressing without moving does not reorder anything")
    s = Session(binary)
    try:
        s.open_all()
        before = s.order()
        col = s.entry_col("alpha.c")
        s.child.send(f"\x1b[<0;{col};1M")
        s.drain(0.4)
        s.child.send(f"\x1b[<0;{col};1m")
        s.drain(0.8)
        check(s.order() == before, "the order is untouched",
              f"{before} -> {s.order()}")
        check("alpha.c" in s.status(), "and it switched to that tab, as a click does",
              s.status())
    finally:
        s.close()


def test_picking_a_tab_up_does_not_open_it(binary):
    """A tab can be carried without ever being looked at.

    Pressing used to switch immediately. It does not, and that is what makes
    dropping one into the document work: the split has to appear beside the
    document that is ALREADY there, not beside the file being dropped.
    """
    print("\nHolding a tab does not open it; a click still does")
    s = Session(binary)
    try:
        s.open_all()
        check("gamma.c" in s.status(), "gamma.c is showing", s.status())

        col = s.entry_col("alpha.c")
        s.child.send(f"\x1b[<0;{col};1M")
        s.drain(0.6)
        check("gamma.c" in s.status(),
              "pressing alpha.c with the button held leaves gamma.c showing",
              s.status())

        # Carry it somewhere and back, still without releasing.
        for c in range(col, col + 10, 3):
            s.child.send(f"\x1b[<32;{c};1M")
            s.drain(0.1)
        check("gamma.c" in s.status(), "and dragging it does not either",
              s.status())

        s.child.send(f"\x1b[<0;{col + 9};1m")
        s.drain(1.0)

        # But a plain click, with no movement at all, still switches.
        col = s.entry_col("alpha.c")
        s.child.send(f"\x1b[<0;{col};1M")
        s.drain(0.4)
        s.child.send(f"\x1b[<0;{col};1m")
        s.drain(1.0)
        check("alpha.c" in s.status(), "clicking it opens it as it always did",
              s.status())
    finally:
        s.close()


def test_release_off_the_bar_changes_nothing(binary):
    print("\nA drop that lands nowhere snaps back")
    s = Session(binary)
    try:
        s.open_all()
        before = s.order()
        col = s.entry_col("alpha.c")
        s.child.send(f"\x1b[<0;{col};1M")
        s.drain(0.4)
        for c in range(col + 1, col + 14, 2):
            s.child.send(f"\x1b[<32;{c};1M")
            s.drain(0.1)
        moved = s.order()
        check(moved != before, "the bar previewed the move while dragging",
              f"{before} -> {moved}")
        # Wander into the document and release there.
        s.child.send("\x1b[<32;50;12M")
        s.drain(0.4)
        s.child.send("\x1b[<0;50;12m")
        s.drain(1.0)
        check(s.order() == before, "and released off the bar it is unchanged",
              f"{before} -> {s.order()}")
    finally:
        s.close()


def test_moving_a_tab_does_not_change_what_is_showing(binary):
    print("\nMoving a tab leaves the document alone")
    s = Session(binary)
    try:
        s.open_all()
        before = s.status()
        check("gamma.c" in before, "gamma.c is showing", before)
        s.drag(s.entry_col("alpha.c"), s.entry_col("gamma.c"))
        check("alpha.c" in s.tab_bar(), "the moved tab is still on the bar",
              s.tab_bar())
        check("gamma.c" in s.status(),
              "and the file being read is the same one as before", s.status())
    finally:
        s.close()


def test_a_group_moves_as_one_block(binary):
    print("\nA group entry carries its members with it")
    s = Session(binary)
    try:
        s.open_all()
        # Group the first two files via the tree, leaving gamma.c ungrouped.
        s.send("\x10", 0.7)
        s.send("Group All Tabs", 0.6)
        s.send("\r", 1.5)
        check("(" in s.tab_bar(), "a group was formed", s.tab_bar())
        check("ws/" in s.tab_bar(), "named for the workspace", s.tab_bar())

        members_before = s.row2()
        check(len(members_before.strip()) > 0, "its members are on row 2",
              repr(members_before))

        # Nothing to reorder against with a single entry, so the assertion
        # that matters here is that the group row SURVIVES a drag.
        col = s.entry_col("ws/")
        s.drag(col, col + 6)
        check("(" in s.tab_bar(), "the group is still a group", s.tab_bar())
        check(len(s.row2().strip()) > 0, "and its member row is still shown",
              repr(s.row2()))
    finally:
        s.close()


def test_reordering_within_a_group(binary):
    print("\nMembers can be reordered on row 2")
    s = Session(binary)
    try:
        s.open_all()
        s.send("\x10", 0.7)
        s.send("Group All Tabs", 0.6)
        s.send("\r", 1.5)
        before = s.row2()
        names = before.split()
        check(len(names) >= 3, "three members on the row", repr(before))
        if len(names) < 3:
            return

        first, last = names[0], names[-1]
        c_first = s.entry_col(first, row=2)
        c_last = s.entry_col(last, row=2)
        s.drag(c_first, c_last, from_row=2)

        after = s.row2().split()
        check(after != names, "the member order changed",
              f"{names} -> {after}")
        check(set(after) == set(names), "with the same members",
              f"{names} -> {after}")
        # Row 2 order is the ordinals; the tabs array must not have moved.
        check("(" in s.tab_bar(), "and row 1 still shows one group entry",
              s.tab_bar())
    finally:
        s.close()


def test_carrying_a_member_out_of_its_group(binary):
    print("\nA member dragged onto row 1 leaves the group")
    s = Session(binary, n=4)
    try:
        s.open_all()
        s.send("\x10", 0.7)
        s.send("Group All Tabs", 0.6)
        s.send("\r", 1.6)
        check("(4)" in s.tab_bar(), "all four are in one group", s.tab_bar())

        names = s.row2().split()
        victim = names[-1]
        col = s.entry_col(victim, row=2)
        # Up onto row 1, landing on its blank tail -- with the group as the
        # only entry there is nothing else to aim at, which is exactly why
        # the empty part of a strip has to be a valid drop.
        s.child.send(f"\x1b[<0;{col};2M")
        s.drain(0.4)
        for c in range(col, col + 8, 2):
            s.child.send(f"\x1b[<32;{c};2M")
            s.drain(0.1)
        s.child.send(f"\x1b[<32;{col + 8};1M")
        s.drain(0.4)
        s.child.send(f"\x1b[<0;{col + 8};1m")
        s.drain(1.2)

        check("(3)" in s.tab_bar(), "the group is down to three", s.tab_bar())
        check(victim in s.tab_bar(), "and the member is a tab in its own right",
              s.tab_bar())
    finally:
        s.close()


def test_dwelling_over_a_group_opens_it_to_drop_into(binary):
    print("\nHolding a tab over a group opens it, and it can be dropped in")
    s = Session(binary, n=4)
    try:
        s.open_all()
        s.send("\x10", 0.7)
        s.send("Group All Tabs", 0.6)
        s.send("\r", 1.6)

        # Take one out first, so there is something to put back.
        names = s.row2().split()
        victim = names[-1]
        col = s.entry_col(victim, row=2)
        s.child.send(f"\x1b[<0;{col};2M")
        s.drain(0.4)
        for c in range(col, col + 8, 2):
            s.child.send(f"\x1b[<32;{c};2M")
            s.drain(0.1)
        s.child.send(f"\x1b[<32;{col + 8};1M")
        s.drain(0.4)
        s.child.send(f"\x1b[<0;{col + 8};1m")
        s.drain(1.2)
        check("(3)" in s.tab_bar(), "staged: three in the group, one out",
              s.tab_bar())

        src = s.entry_col(victim)
        grp = s.entry_col("ws/")
        if src is None or grp is None:
            check(False, "found both entries on row 1", s.tab_bar())
            return

        s.child.send(f"\x1b[<0;{src};1M")
        s.drain(0.4)
        for c in range(src + 1, grp + 2, 2):
            s.child.send(f"\x1b[<32;{c};1M")
            s.drain(0.1)
        check("alpha" not in s.row2(),
              "the group is closed while merely passing over it", repr(s.row2()))

        # Rest on it. The strip opens on a DWELL, so that dragging past a
        # group on the way elsewhere does not flash its members open.
        for _ in range(8):
            s.child.send(f"\x1b[<32;{grp + 2};1M")
            s.drain(0.12)
        check("alpha" in s.row2(), "resting on it opens its member strip",
              repr(s.row2()))

        drop = s.entry_col("beta.c", row=2)
        if drop is None:
            check(False, "found a member to aim at", repr(s.row2()))
            return
        s.child.send(f"\x1b[<32;{drop};2M")
        s.drain(0.4)
        s.child.send(f"\x1b[<0;{drop};2m")
        s.drain(1.2)

        check("(4)" in s.tab_bar(), "dropping in put it back in the group",
              s.tab_bar())
        after = s.row2().split()
        check(victim in after, "it is on the member row", repr(s.row2()))
        check(after.index(victim) < after.index("beta.c"),
              "at the position it was dropped on, not appended",
              repr(s.row2()))
    finally:
        s.close()


def two_group_session(binary):
    """A workspace restored with two groups: one/{a,b}.c and two/{x,y}.c."""
    home = tempfile.mkdtemp(prefix="fac_td_home_")
    os.makedirs(os.path.join(home, ".config", "fac"))
    with open(os.path.join(home, ".config", "fac", "state.json"), "w") as f:
        f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                ' "version": "1.0"}\n')
    root = tempfile.mkdtemp(prefix="fac_td_work_")
    ws = os.path.join(root, "ws")
    os.makedirs(ws)
    for d, files in (("one", ("a.c", "b.c")), ("two", ("x.c", "y.c"))):
        os.makedirs(os.path.join(ws, d))
        for fn in files:
            with open(os.path.join(ws, d, fn), "w") as f:
                f.write("int v;\n")

    def tab(fn, gid, o):
        return {"filename": fn, "is_orphan": False, "modified": False,
                "group": gid, "group_ordinal": o,
                "panes": [{"x_start": 0.0, "y_start": 0.0, "x_end": 1.0,
                           "y_end": 1.0, "filename": fn, "cursor_line": 1,
                           "cursor_column": 1, "viewport_line": 1,
                           "viewport_column": 1}],
                "active_pane": 1}

    doc = {"version": "1.2", "workspace_path": ws, "last_opened": "20260731",
           "tab_groups": [
               {"id": 1, "label": "one/", "dir_path": os.path.join(ws, "one"),
                "active_member": os.path.join(ws, "one/a.c")},
               {"id": 2, "label": "two/", "dir_path": os.path.join(ws, "two"),
                "active_member": os.path.join(ws, "two/x.c")}],
           "tabs": [tab("one/a.c", 1, 1), tab("one/b.c", 1, 2),
                    tab("two/x.c", 2, 1), tab("two/y.c", 2, 2)],
           "active_tab": 1, "fuss_mode": False}
    os.makedirs(os.path.join(ws, ".fac"))
    with open(os.path.join(ws, ".fac", "workspace.json"), "w") as f:
        json.dump(doc, f, indent=2)

    env = {**os.environ, "TERM": "xterm-256color", "HOME": home}
    env.pop("XDG_CONFIG_HOME", None)
    env.pop("FAC_SESSION", None)
    s = Session.__new__(Session)
    s.home, s.root, s.work = home, root, ws
    s.screen = pyte.Screen(COLS, ROWS)
    s.stream = pyte.Stream(s.screen)
    s.child = pexpect.spawn(binary, [ws], dimensions=(ROWS, COLS), env=env, cwd=ws)
    s.drain(3.0)
    return s


def test_one_drag_from_one_group_into_another(binary):
    """The whole point of the dwell: no intermediate drop.

    While inside a group, row 2 is that group's pinned member row -- so the
    group you are leaving used to own that row for the whole drag and no other
    group could be reached without letting go first. During a drag the row
    follows the pointer instead.
    """
    print("\nA member moves between groups in a single continuous drag")
    s = two_group_session(binary)
    try:
        check("one/ (2)" in s.tab_bar() and "two/ (2)" in s.tab_bar(),
              "two groups of two", s.tab_bar())
        check("b.c" in s.row2(), "we are inside group one", repr(s.row2()))

        src = s.entry_col("b.c", row=2)
        g2 = s.entry_col("two/")
        if src is None or g2 is None:
            check(False, "found the member and the other group", s.tab_bar())
            return

        # Press on row 2, and never release until the very end.
        s.child.send(f"\x1b[<0;{src};2M")
        s.drain(0.4)
        s.child.send(f"\x1b[<32;{src};1M")
        s.drain(0.3)
        for c in range(src, g2 + 3, 2):
            s.child.send(f"\x1b[<32;{c};1M")
            s.drain(0.1)
        for _ in range(8):
            s.child.send(f"\x1b[<32;{g2 + 2};1M")
            s.drain(0.12)

        check("x.c" in s.row2(),
              "resting on the other group shows ITS members on row 2",
              repr(s.row2()))
        check("a.c" not in s.row2(),
              "and no longer the group being left", repr(s.row2()))

        drop = s.entry_col("y.c", row=2)
        if drop is None:
            check(False, "found somewhere to drop", repr(s.row2()))
            return
        s.child.send(f"\x1b[<32;{drop};2M")
        s.drain(0.4)
        s.child.send(f"\x1b[<0;{drop};2m")
        s.drain(1.2)

        check("one/ (1)" in s.tab_bar(), "the source group lost a member",
              s.tab_bar())
        check("two/ (3)" in s.tab_bar(), "and the target gained one",
              s.tab_bar())
        # Row 2 still belongs to the group we are INSIDE, which is the one
        # the file left -- the drag did not switch anywhere. Enter the target
        # group to see what it now holds.
        check("b.c" not in s.row2(), "it is out of the group it came from",
              repr(s.row2()))
        g2 = s.entry_col("two/")
        if g2 is None:
            check(False, "found the target group", s.tab_bar())
            return
        s.click(1, g2 + 2)
        after = s.row2().split()
        check("b.c" in after, "the file is in the target group now",
              repr(s.row2()))
        if "b.c" in after and "y.c" in after:
            check(after.index("b.c") < after.index("y.c"),
                  "at the position it was dropped on", repr(s.row2()))
    finally:
        s.close()


def narrow_session(binary, n_files, cols):
    """A session with `n_files` tabs open in a `cols`-wide window.

    Narrow on purpose: the chevrons only exist when the bar overflows, and
    six short names in sixty columns is the smallest thing that does.
    """
    home = tempfile.mkdtemp(prefix="fac_td_home_")
    os.makedirs(os.path.join(home, ".config", "fac"))
    with open(os.path.join(home, ".config", "fac", "state.json"), "w") as f:
        f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                ' "version": "1.0"}\n')
    root = tempfile.mkdtemp(prefix="fac_td_work_")
    ws = os.path.join(root, "ws")
    os.makedirs(ws)
    names = [f"file{i}.c" for i in range(1, n_files + 1)]
    for nm in names:
        with open(os.path.join(ws, nm), "w") as f:
            f.write("int v;\n")

    env = {**os.environ, "TERM": "xterm-256color", "HOME": home}
    env.pop("XDG_CONFIG_HOME", None)
    env.pop("FAC_SESSION", None)
    s = Session.__new__(Session)
    s.home, s.root, s.work, s.names = home, root, ws, names
    s.screen = pyte.Screen(cols, ROWS)
    s.stream = pyte.Stream(s.screen)
    s.child = pexpect.spawn(binary, [os.path.join(ws, names[0])],
                            dimensions=(ROWS, cols), env=env, cwd=ws)
    s.drain(2.5)
    s.open_all()
    return s


def reverse_cells(s, r0, r1, c0, c1):
    """How many cells in this rectangle are drawn reverse-video.

    The split preview is a band of reverse-video SPACES, so it is invisible
    in screen.display -- which shows characters, not attributes. Counting the
    attribute is the only way to see it, and is what it actually is.
    """
    n = 0
    for y in range(r0 - 1, min(r1, s.screen.lines)):
        for x in range(c0 - 1, min(c1, s.screen.columns)):
            if s.screen.buffer[y][x].reverse:
                n += 1
    return n


def carry_to(s, name, col, row):
    """Grab `name` from the bar and carry it to (row, col), without releasing."""
    src = s.entry_col(name)
    s.child.send(f"\x1b[<0;{src};1M")
    s.drain(0.4)
    for c in range(src, min(col, COLS), 6):
        s.child.send(f"\x1b[<32;{c};1M")
        s.drain(0.08)
    s.child.send(f"\x1b[<32;{col};{row}M")
    s.drain(0.5)


def test_an_edge_previews_a_split(binary):
    print("\nCarrying a tab to an edge previews the split it would make")
    s = Session(binary)
    try:
        s.open_all()
        mid = COLS // 2
        # Middle of the document: no edge, so nothing is offered.
        carry_to(s, "alpha.c", mid, 12)
        # Rows either side of the pointer: the GHOST is reverse video too and
        # sits on the pointer's own row, so counting that row would find the
        # label and call it a preview.
        centre = reverse_cells(s, 3, 10, 30, 90) + reverse_cells(s, 14, 25, 30, 90)
        check(centre == 0, "the middle of the pane offers no split",
              f"{centre} reverse cells")

        # Right edge.
        s.child.send(f"\x1b[<32;{COLS - 3};12M")
        s.drain(0.5)
        right = reverse_cells(s, 3, 25, COLS - 25, COLS)
        left_side = reverse_cells(s, 3, 25, 2, 30)
        check(right > 100, "the right quarter is banded", f"{right} cells")
        check(left_side == 0, "and the left is not", f"{left_side} cells")

        # Bottom edge.
        s.child.send(f"\x1b[<32;{mid};26M")
        s.drain(0.5)
        bottom = reverse_cells(s, 24, 28, 10, COLS - 10)
        check(bottom > 100, "moving to the bottom bands that instead",
              f"{bottom} cells")

        s.child.send(f"\x1b[<0;{mid};26m")
        s.drain(1.2)
    finally:
        s.close()


def test_dropping_at_an_edge_makes_a_split(binary):
    print("\nDropping at an edge splits, and takes the tab off the bar")
    s = Session(binary)
    try:
        s.open_all()
        check("gamma.c" in s.status(), "gamma.c is showing before the grab",
              s.status())

        carry_to(s, "alpha.c", COLS - 3, 12)
        s.child.send(f"\x1b[<0;{COLS - 3};12m")
        s.drain(1.5)

        check("alpha.c" not in s.tab_bar(), "alpha.c has left the bar",
              s.tab_bar())
        check("beta.c" in s.tab_bar() and "gamma.c" in s.tab_bar(),
              "the other tabs are untouched", s.tab_bar())

        body = s.text()
        check("int alpha;" in body, "its text is on screen, in a pane",
              body[:200])
        check("int gamma;" in body,
              "beside what was showing before it was grabbed", body[:200])
        # Two panes means two pane headers on the same row.
        check(body.count("[alpha.c]") + body.count("[gamma.c]") >= 2,
              "two panes, labelled", repr(s.screen.display[1]))
    finally:
        s.close()


def test_the_middle_of_the_document_snaps_back(binary):
    print("\nDropping in the middle of the document changes nothing")
    s = Session(binary)
    try:
        s.open_all()
        before = s.order()
        carry_to(s, "alpha.c", COLS // 2, 12)
        s.child.send(f"\x1b[<0;{COLS // 2};12m")
        s.drain(1.2)
        check(s.order() == before, "the bar is unchanged",
              f"{before} -> {s.order()}")
        check("gamma.c" in s.status(),
              "and the document is the one that was already there",
              s.status())
    finally:
        s.close()


def test_a_modified_tab_will_not_split_off(binary):
    print("\nA tab with unsaved changes refuses rather than losing them")
    s = Session(binary)
    try:
        s.open_all()
        # gamma.c is active; dirty it, then try to carry it to an edge.
        s.send("Z", 0.8)
        check("*" in s.tab_bar(), "gamma.c is modified", s.tab_bar())

        carry_to(s, "gamma.c", COLS - 3, 12)
        s.child.send(f"\x1b[<0;{COLS - 3};12m")
        s.drain(1.2)

        check("gamma.c" in s.tab_bar(), "it is still on the bar", s.tab_bar())
        check("before splitting" in s.text() or "Save" in s.text(),
              "and it said why", s.status())
    finally:
        s.close()


def test_the_bar_never_renders_torn(binary):
    """Reported: tabs rendering in pieces while dragging over them.

    The ghost was drawn ON the tab bar, overwriting the entries underneath --
    [1: a alpha.c [3: gamma.c] and so on. The bar already shows where the held
    tab will land, so a label on top of it was redundant as well as
    destructive. Balanced brackets is the cheap invariant that catches it.
    """
    print("\nThe bar stays intact while a tab is carried over it")
    s = Session(binary, n=4)
    try:
        s.open_all()
        src = s.entry_col("alpha.c")
        s.child.send(f"\x1b[<0;{src};1M")
        s.drain(0.4)
        torn = []
        for c in range(src + 1, 60):
            s.child.send(f"\x1b[<32;{c};1M")
            s.drain(0.07)
            bar = s.tab_bar()
            if bar.count("[") != bar.count("]"):
                torn.append((c, bar))
        check(not torn, "no torn entry at any column",
              f"{len(torn)} of them, first: {torn[0] if torn else ''}")
        s.child.send("\x1b[<0;59;1m")
        s.drain(1.0)
    finally:
        s.close()


def test_no_ghost_is_left_behind(binary):
    """Reported: a ghost stuck at the far right of a sparse bar.

    It could be painted past the end of the bar's own window -- which is
    narrower than the screen whenever the file tree is open -- and the next
    frame's clear does not reach that far.
    """
    print("\nNothing is left painted on the bar after a drag")
    s = Session(binary, n=4)
    try:
        s.open_all()
        s.send("\x02", 1.2)                      # tree open: narrower bar
        src = s.entry_col("alpha.c")
        if src is None:
            check(False, "found the tab with the tree open", s.tab_bar())
            return
        s.child.send(f"\x1b[<0;{src};1M")
        s.drain(0.4)
        for c in range(src, COLS - 2, 8):
            s.child.send(f"\x1b[<32;{c};1M")
            s.drain(0.1)
        s.child.send(f"\x1b[<0;{COLS - 4};1m")
        s.drain(1.2)
        bar = s.tab_bar()
        check(bar.count("[") == bar.count("]"), "the bar is intact", bar)
        # The ghost reads " alpha.c " with no brackets; a bare name outside
        # any entry is the stranded label.
        check("alpha.c ]" in bar or "alpha.c*]" in bar,
              "alpha.c appears only as a real entry", bar)
    finally:
        s.close()


def test_the_group_strip_closes_when_you_leave(binary):
    """Reported: the strip persisted after moving off, and the bar lagged."""
    print("\nA group's strip closes as soon as the pointer leaves it")
    s = Session(binary, n=4)
    try:
        s.open_all()
        s.send("\x10", 0.7)
        s.send("Group All Tabs", 0.6)
        s.send("\r", 1.6)
        names = s.row2().split()
        victim = names[-1]
        c = s.entry_col(victim, row=2)
        s.child.send(f"\x1b[<0;{c};2M")
        s.drain(0.4)
        for x in range(c, c + 8, 2):
            s.child.send(f"\x1b[<32;{x};2M")
            s.drain(0.1)
        s.child.send(f"\x1b[<32;{c + 8};1M")
        s.drain(0.4)
        s.child.send(f"\x1b[<0;{c + 8};1m")
        s.drain(1.2)

        src = s.entry_col(victim)
        grp = s.entry_col("ws/")
        if src is None or grp is None:
            check(False, "staged a tab outside the group", s.tab_bar())
            return
        s.child.send(f"\x1b[<0;{src};1M")
        s.drain(0.4)
        for x in range(src + 1, grp + 2, 2):
            s.child.send(f"\x1b[<32;{x};1M")
            s.drain(0.1)
        for _ in range(8):
            s.child.send(f"\x1b[<32;{grp + 2};1M")
            s.drain(0.12)
        check("alpha.c" in s.row2(), "the strip opened on the dwell",
              repr(s.row2()))

        s.child.send(f"\x1b[<32;{src};1M")
        s.drain(0.4)
        check("alpha.c" not in s.row2(),
              "and closed immediately on moving off it", repr(s.row2()))
        s.child.send(f"\x1b[<0;{src};1m")
        s.drain(1.0)
    finally:
        s.close()


def test_returning_to_the_bar_cancels_a_split(binary):
    """Reported: an edge preview survived a change of mind and split anyway."""
    print("\nGoing to an edge and back to the bar cancels the split")
    s = Session(binary)
    try:
        s.open_all()
        src = s.entry_col("alpha.c")
        s.child.send(f"\x1b[<0;{src};1M")
        s.drain(0.4)
        s.child.send(f"\x1b[<32;{COLS - 3};12M")
        s.drain(0.5)
        armed = reverse_cells(s, 3, 25, COLS - 40, COLS)
        check(armed > 100, "the split is previewed at the edge", f"{armed} cells")

        s.child.send(f"\x1b[<32;{src + 20};1M")
        s.drain(0.5)
        left = reverse_cells(s, 3, 25, COLS - 40, COLS)
        check(left == 0, "coming back to the bar clears the preview",
              f"{left} cells")

        s.child.send(f"\x1b[<0;{src + 20};1m")
        s.drain(1.2)
        check("alpha.c" in s.tab_bar(), "and the drop lands on the bar",
              s.tab_bar())
        check(s.text().count("int gamma;") + s.text().count("int alpha;") <= 1,
              "with no split created", s.text()[:200])
    finally:
        s.close()


def test_the_preview_is_the_pane_that_appears(binary):
    """Reported: the band showed a third of what the split produced."""
    print("\nThe band is exactly where the new pane lands")
    s = Session(binary)
    try:
        s.open_all()
        carry_to(s, "alpha.c", COLS - 3, 12)
        band = [x + 1 for x in range(COLS) if s.screen.buffer[11][x].reverse]
        check(bool(band), "a band is shown")
        if not band:
            return
        # A vertical split halves the pane, so the band starts at the middle.
        check(abs(band[0] - (COLS // 2 + 1)) <= 2,
              "it starts at the halfway column, not a quarter in",
              f"starts at {band[0]}, half is {COLS // 2 + 1}")
        check(band[-1] >= COLS - 1, "and runs to the edge", f"ends {band[-1]}")

        # A quarter in from the edge is well inside the pane and must not arm.
        s.child.send(f"\x1b[<32;{COLS - 3 - COLS // 4};12M")
        s.drain(0.4)
        n = reverse_cells(s, 3, 10, 2, COLS) + reverse_cells(s, 14, 25, 2, COLS)
        check(n == 0, "a quarter in from the edge arms nothing",
              f"{n} cells")
        s.child.send(f"\x1b[<0;{COLS - 3 - COLS // 4};12m")
        s.drain(1.0)
    finally:
        s.close()


def test_the_left_edge_puts_the_pane_on_the_left(binary):
    """The split always builds its new pane on the right, so a left drop had
    to be previewed on the left and then produced on the right."""
    print("\nDropping on the left edge really does split leftwards")
    s = Session(binary)
    try:
        s.open_all()
        carry_to(s, "alpha.c", 3, 12)
        s.child.send("\x1b[<0;3;12m")
        s.drain(1.5)
        header = s.screen.display[1]
        ia, ig = header.find("[alpha.c]"), header.find("[gamma.c]")
        check(ia >= 0 and ig >= 0, "two panes appeared", header.rstrip()[:100])
        if ia >= 0 and ig >= 0:
            check(ia < ig, "the carried file is the LEFT pane",
                  header.rstrip()[:100])
    finally:
        s.close()


def stage_one_out(s):
    """Group everything, then pull the last member out onto the bar."""
    s.send("\x10", 0.7)
    s.send("Group All Tabs", 0.6)
    s.send("\r", 1.6)
    names = s.row2().split()
    victim = names[-1]
    c = s.entry_col(victim, row=2)
    s.child.send(f"\x1b[<0;{c};2M")
    s.drain(0.4)
    for x in range(c, c + 8, 2):
        s.child.send(f"\x1b[<32;{x};2M")
        s.drain(0.1)
    s.child.send(f"\x1b[<32;{c + 8};1M")
    s.drain(0.4)
    s.child.send(f"\x1b[<0;{c + 8};1m")
    s.drain(1.2)
    return victim


def test_a_group_does_not_run_from_the_pointer(binary):
    """Reported: impossible to hover a group, because it moves aside.

    Pointing at a group made it the reorder target, which slid it out of the
    way instantly -- so the only way to hover it was to aim where it had
    been. Resting on a group entry now freezes the bar instead.
    """
    print("\nA group entry stays put while a tab rests on it")
    s = Session(binary, n=4)
    try:
        s.open_all()
        victim = stage_one_out(s)
        src = s.entry_col(victim)
        grp = s.entry_col("ws/")
        if src is None or grp is None:
            check(False, "staged a loose tab and a group", s.tab_bar())
            return
        settled = s.tab_bar()

        s.child.send(f"\x1b[<0;{src};1M")
        s.drain(0.3)
        # FAST, deliberately: slower than the dwell and the member row opens,
        # which stops row 1 shifting for a different reason and would let a
        # missing freeze pass unnoticed. This is the freeze on its own.
        moved = []
        for x in range(grp, grp + 6):
            s.child.send(f"\x1b[<32;{x};1M")
            s.drain(0.05)
            if s.tab_bar() != settled:
                moved.append(x)
        check(not moved, "the bar does not shift while on the group entry",
              f"shifted at columns {moved}")

        # Now rest, and the members appear.
        s.drain(0.6)
        check("alpha.c" in s.row2(), "and resting opens its members",
              repr(s.row2()))
        s.child.send(f"\x1b[<0;{grp + 5};1m")
        s.drain(1.0)
    finally:
        s.close()


def test_sweeping_past_a_group_does_not_open_it(binary):
    print("\nSweeping past a group leaves it alone")
    s = Session(binary, n=4)
    try:
        s.open_all()
        victim = stage_one_out(s)
        src = s.entry_col(victim)
        grp = s.entry_col("ws/")
        if src is None or grp is None:
            check(False, "staged", s.tab_bar())
            return
        s.child.send(f"\x1b[<0;{src};1M")
        s.drain(0.3)
        for x in range(src + 1, grp + 12, 2):
            s.child.send(f"\x1b[<32;{x};1M")
            s.drain(0.04)
        check("alpha.c" not in s.row2(),
              "the member row never flashed open", repr(s.row2()))
        s.child.send(f"\x1b[<0;{grp + 11};1m")
        s.drain(1.0)
    finally:
        s.close()


def test_row_two_shows_where_the_tab_will_land(binary):
    """Reported: no indication of where the held tab goes in row 2."""
    print("\nThe member row shows the incoming tab in place")
    s = Session(binary, n=4)
    try:
        s.open_all()
        victim = stage_one_out(s)
        src = s.entry_col(victim)
        grp = s.entry_col("ws/")
        if src is None or grp is None:
            check(False, "staged", s.tab_bar())
            return
        s.child.send(f"\x1b[<0;{src};1M")
        s.drain(0.3)
        s.child.send(f"\x1b[<32;{grp + 2};1M")
        s.drain(0.6)
        check(victim in s.row2(), "it appears in the row while still on row 1",
              repr(s.row2()))

        mid = s.entry_col("beta.c", row=2)
        if mid is None:
            check(False, "found a member to aim at", repr(s.row2()))
            return
        s.child.send(f"\x1b[<32;{mid};2M")
        s.drain(0.5)
        preview = s.row2().split()
        check(victim in preview and "beta.c" in preview,
              "and moves to the aimed position", repr(s.row2()))
        if victim in preview and "beta.c" in preview:
            check(preview.index(victim) < preview.index("beta.c"),
                  "ahead of the member under the pointer", repr(s.row2()))

        s.child.send(f"\x1b[<0;{mid};2m")
        s.drain(1.3)
        landed = s.row2().split()
        check(landed == preview, "and the drop lands exactly where shown",
              f"{preview} -> {landed}")
    finally:
        s.close()


def test_the_chevron_scrolls_whatever_is_active(binary):
    """Reported twice over, and one cause.

    The bar laid itself out following the active entry on EVERY redraw. So a
    chevron click that would push the active tab off the right was undone
    before it could be seen -- the chevron worked only while the tab beside
    it happened to be active -- and a tab opening at the far right pinned the
    bar there with everything else behind a chevron that would not move.
    """
    print("\nThe chevron scrolls regardless of which tab is active")
    s = narrow_session(binary, 6, 60)
    try:
        bar = s.tab_bar()
        check("<" in bar, "opening six tabs in a narrow window overflows", bar)
        check("file6.c" in bar, "and the newest tab is the one on screen", bar)
        if "<" not in bar:
            return

        col = bar.find("<") + 1
        steps = [bar]
        for _ in range(4):
            s.click(1, col)
            steps.append(s.tab_bar())
        check(steps[1] != steps[0], "the first click scrolls", str(steps[:2]))
        check("file1.c" in steps[-1],
              "and it keeps going past where the active tab drops off, "
              "all the way to the first", str(steps[-1]))
        check("file6.c" not in steps[-1],
              "the active tab is off screen, which is allowed", steps[-1])

        before = s.tab_bar()
        s.send("\x1b[B", 0.5)
        s.send("\x1b[A", 0.5)
        check(s.tab_bar() == before, "and the position survives a redraw",
              f"{before!r} -> {s.tab_bar()!r}")

        # Switching to an off-screen tab must still reveal it. Land on a
        # VISIBLE one first: file6 was already active -- scrolling does not
        # change which tab you are in -- so jumping to it would be a no-op
        # and would prove nothing.
        vis = s.entry_col("file2.c")
        if vis is not None:
            s.click(1, vis + 3)
        check("file2.c" in s.status(), "moved to a visible tab", s.status())
        s.send("\x1b6", 1.0)
        check("file6.c" in s.status(), "jumped to the hidden tab", s.status())
        check("file6.c" in s.tab_bar(),
              "and switching to it scrolled the bar to reveal it", s.tab_bar())
    finally:
        s.close()



def mismatched_order_session(binary):
    """Two groups, with the tab ARRAY in a different order from the bar.

    getopt/ holds one file; tail/ holds three. The array is written
    tail.c-first while the ordinals still display main.c first, because the
    bug being pinned was a fallback that scanned the ARRAY -- with the two
    orders identical it picks the right file by luck and proves nothing.

    Each file's text carries its own marker so a pane can be identified by
    content rather than by a header that might itself be wrong.
    """
    home = tempfile.mkdtemp(prefix="fac_td_home_")
    os.makedirs(os.path.join(home, ".config", "fac"))
    with open(os.path.join(home, ".config", "fac", "state.json"), "w") as f:
        f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                ' "version": "1.0"}\n')
    root = tempfile.mkdtemp(prefix="fac_td_work_")
    ws = os.path.join(root, "ws")
    for d, files in (("getopt", ("long.c",)),
                     ("tail", ("main.c", "tail.c", "tail.h"))):
        os.makedirs(os.path.join(ws, d))
        for fn in files:
            tok = (d + "_" + fn).replace(".", "_").upper()
            with open(os.path.join(ws, d, fn), "w") as f:
                f.writelines("int %s_%d;\n" % (tok, i) for i in range(1, 20))

    def tab(fn, gid, o):
        return {"filename": fn, "is_orphan": False, "modified": False,
                "group": gid, "group_ordinal": o,
                "panes": [{"x_start": 0.0, "y_start": 0.0, "x_end": 1.0,
                           "y_end": 1.0, "filename": fn, "cursor_line": 1,
                           "cursor_column": 1, "viewport_line": 1,
                           "viewport_column": 1}],
                "active_pane": 1}

    doc = {"version": "1.2", "workspace_path": ws, "last_opened": "20260801",
           "tab_groups": [
               {"id": 1, "label": "getopt/", "dir_path": os.path.join(ws, "getopt"),
                "active_member": os.path.join(ws, "getopt/long.c")},
               {"id": 2, "label": "tail/", "dir_path": os.path.join(ws, "tail"),
                "active_member": os.path.join(ws, "tail/main.c")}],
           # tail.c first in the ARRAY, main.c first on the BAR.
           "tabs": [tab("getopt/long.c", 1, 1), tab("tail/tail.c", 2, 2),
                    tab("tail/main.c", 2, 1), tab("tail/tail.h", 2, 3)],
           "active_tab": 1, "fuss_mode": False}
    os.makedirs(os.path.join(ws, ".fac"))
    with open(os.path.join(ws, ".fac", "workspace.json"), "w") as f:
        json.dump(doc, f, indent=2)

    env = {**os.environ, "TERM": "xterm-256color", "HOME": home}
    env.pop("XDG_CONFIG_HOME", None)
    env.pop("FAC_SESSION", None)
    s = Session.__new__(Session)
    s.home, s.root, s.work = home, root, ws
    s.screen = pyte.Screen(COLS, ROWS)
    s.stream = pyte.Stream(s.screen)
    s.child = pexpect.spawn(binary, [ws], dimensions=(ROWS, COLS), env=env, cwd=ws)
    s.drain(3.0)
    return s


def test_a_tab_dropped_on_its_own_document_splits_no_stranger(binary):
    """Reported twice from a real session, and it destroys the gesture's point.

    Carrying a tab into a group ACTIVATES it, so straight afterwards the
    document on screen is the tab still being held. Dropping it at an edge
    then had no host, and the code picked the first tab in the array -- an
    unrelated file, in no particular order -- while the preview had been
    drawn over something else entirely.
    """
    print("\nDropping a tab onto its own document never splits a third file")
    s = mismatched_order_session(binary)
    try:
        # Carry long.c out of getopt/ and into the tail/ group.
        src = s.entry_col("long.c", row=2) or s.entry_col("long.c", row=1)
        row = 2 if s.entry_col("long.c", row=2) else 1
        tgt = s.entry_col("tail/", row=1)
        if not (src and tgt):
            check(False, "found long.c and the tail/ group on the bar",
                  f"{s.tab_bar()} / {s.row2()}")
            return
        s.drag(src, tgt, from_row=row, to_row=1)
        s.drain(1.5)
        check("long.c" in s.row2(), "long.c joined the tail/ group", s.row2())
        check("GETOPT_LONG_C" in s.text(),
              "and is the document on screen, because joining activated it")

        # Now carry it into the document and drop it at the right edge.
        src2 = s.entry_col("long.c", row=2)
        if not src2:
            check(False, "long.c is on the member row", s.row2())
            return
        # carry_to looks on row 1; long.c is a group MEMBER now, on row 2.
        s.child.send(f"\x1b[<0;{src2};2M")
        s.drain(0.4)
        for c in range(src2, COLS - 3, 6):
            s.child.send(f"\x1b[<32;{c};2M")
            s.drain(0.08)
        for r in (4, 8, 12):
            s.child.send(f"\x1b[<32;{COLS - 3};{r}M")
            s.drain(0.2)
        s.drain(0.6)
        s.child.send(f"\x1b[<0;{COLS - 3};12m")
        s.drain(1.8)

        body = s.text()
        check("TAIL_TAIL_C" not in body and "TAIL_TAIL_H" not in body,
              "no unrelated file was pulled into a pane", body[:300])
        # Either nothing happened, or the split is against what was showing.
        if "GETOPT_LONG_C" in body and "TAIL_MAIN_C" in body:
            check(True, "a split against the document that was showing")
        else:
            check("long.c" in s.row2() or "long.c" in s.tab_bar(),
                  "or the drop was refused and the tab stayed on the bar",
                  s.row2())
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_drag_right_and_left,
               test_a_click_is_still_a_click,
               test_picking_a_tab_up_does_not_open_it,
               test_release_off_the_bar_changes_nothing,
               test_moving_a_tab_does_not_change_what_is_showing,
               test_a_group_moves_as_one_block,
               test_reordering_within_a_group,
               test_carrying_a_member_out_of_its_group,
               test_dwelling_over_a_group_opens_it_to_drop_into,
               test_one_drag_from_one_group_into_another,
               test_an_edge_previews_a_split,
               test_dropping_at_an_edge_makes_a_split,
               test_the_middle_of_the_document_snaps_back,
               test_a_modified_tab_will_not_split_off,
               test_the_bar_never_renders_torn,
               test_no_ghost_is_left_behind,
               test_the_group_strip_closes_when_you_leave,
               test_returning_to_the_bar_cancels_a_split,
               test_the_preview_is_the_pane_that_appears,
               test_the_left_edge_puts_the_pane_on_the_left,
               test_a_group_does_not_run_from_the_pointer,
               test_sweeping_past_a_group_does_not_open_it,
               test_row_two_shows_where_the_tab_will_land,
               test_the_chevron_scrolls_whatever_is_active,
               test_a_tab_dropped_on_its_own_document_splits_no_stranger):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_tabdrag: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_tabdrag: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
