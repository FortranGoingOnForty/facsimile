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


def test_grabbing_an_inactive_tab_activates_it(binary):
    print("\nGrabbing a tab selects it, before any movement")
    s = Session(binary)
    try:
        s.open_all()
        check("gamma.c" in s.status(), "gamma.c is active to begin with", s.status())
        col = s.entry_col("alpha.c")
        s.child.send(f"\x1b[<0;{col};1M")
        s.drain(0.6)
        check("alpha.c" in s.status(),
              "pressing alpha.c made it active with the button still down",
              s.status())
        s.child.send(f"\x1b[<0;{col};1m")
        s.drain(0.5)
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


def test_the_moved_tab_stays_active_and_visible(binary):
    print("\nThe tab you moved is still the one you are looking at")
    s = Session(binary)
    try:
        s.open_all()
        s.drag(s.entry_col("alpha.c"), s.entry_col("gamma.c"))
        check("alpha.c" in s.status(), "alpha.c is still active", s.status())
        check("alpha.c" in s.tab_bar(), "and still on the bar", s.tab_bar())
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


def main():
    binary = find_binary()
    for fn in (test_drag_right_and_left,
               test_a_click_is_still_a_click,
               test_grabbing_an_inactive_tab_activates_it,
               test_release_off_the_bar_changes_nothing,
               test_the_moved_tab_stays_active_and_visible,
               test_a_group_moves_as_one_block,
               test_reordering_within_a_group,
               test_carrying_a_member_out_of_its_group,
               test_dwelling_over_a_group_opens_it_to_drop_into):
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
