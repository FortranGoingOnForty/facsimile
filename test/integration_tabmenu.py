#!/usr/bin/env python3
"""
Integration test: the right-click menu on a tab.

Tabs were the one clickable surface with no context menu -- a left click
switched to them and a right click was swallowed. They now offer Close Tab,
Close Other Tabs, Copy Path and Remove from Group.

The load-bearing case is closing a tab that is NOT the active one. Ctrl-W
closes whatever you are looking at, so every close path in the editor was
written around editor%active_tab_index; a menu row acts on the tab you
right-clicked, which is usually a different one. Getting that wrong closes the
file the user was reading instead of the one they aimed at, so it is asserted
by name rather than by count.

Usage: python3 test/integration_tabmenu.py [path-to-fac-binary]
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

ROWS, COLS = 30, 110

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
    """An editor with alpha.c, beta.c and gamma.c all open as tabs."""

    NAMES = ("alpha.c", "beta.c", "gamma.c")

    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_tm_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        # A SHORT leaf name: "Group All Tabs" labels the group after the
        # workspace directory, and a long temp name is clipped by the tab
        # bar's per-entry cell budget -- taking the "(3)" count with it.
        self.root = tempfile.mkdtemp(prefix="fac_tm_work_")
        self.work = os.path.join(self.root, "ws")
        os.makedirs(self.work)
        for n in self.NAMES:
            with open(os.path.join(self.work, n), "w") as f:
                f.write("int %s;\n" % n[:-2])

        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [os.path.join(self.work, self.NAMES[0])],
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

    def status(self):
        return self.screen.display[ROWS - 1].rstrip()

    def open_all(self):
        """Open the other two files as tabs, through the file tree.

        The tree is opened ONCE and closed at the end. Clicking a tree row
        opens the file but leaves the tree up, so toggling per file closes it
        and the next lookup finds nothing.
        """
        self.send("\x02", 1.2)                   # ctrl-b
        for n in self.NAMES[1:]:
            hit = self.find(n)
            if hit:
                self.click(hit[0], hit[1] + 1)
            self.drain(0.6)
        self.send("\x02", 1.0)                   # and closed again

    def tab_entry(self, name):
        """Screen position of a tab entry on row 1, or None."""
        row = self.screen.display[0]
        j = row.find(name)
        return (1, j + 1) if j >= 0 else None

    def right_click_tab(self, name):
        hit = self.tab_entry(name)
        if not hit:
            return False
        self.click(hit[0], hit[1], button=2)
        return "Close Tab" in self.text()

    def choose(self, label):
        hit = self.find(label)
        if not hit:
            return False
        self.click(hit[0], hit[1] + 1)
        return True

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.root, ignore_errors=True)


def test_the_menu_appears(binary):
    print("\nRight-clicking a tab offers a menu")
    s = Session(binary)
    try:
        s.open_all()
        check(all(n in s.tab_bar() for n in s.NAMES),
              "all three files are open", s.tab_bar())
        check(s.right_click_tab("alpha.c"), "the menu appeared", s.text()[:300])
        body = s.text()
        check("Close Other Tabs" in body, "Close Other Tabs is offered")
        check("Copy Path" in body, "Copy Path is offered")
        check("Remove from Group" in body, "Remove from Group is offered")
    finally:
        s.close()


def test_close_acts_on_the_tab_you_clicked(binary):
    print("\nClose Tab closes the tab clicked, not the active one")
    s = Session(binary)
    try:
        s.open_all()
        # Active is gamma.c (opened last). Right-click alpha.c instead.
        active_before = s.status()
        check("gamma.c" in active_before, "gamma.c is the active tab",
              active_before)

        check(s.right_click_tab("alpha.c"), "the menu appeared on alpha.c")
        check(s.choose("Close Tab"), "chose Close Tab")

        bar = s.tab_bar()
        check("alpha.c" not in bar, "alpha.c is closed", bar)
        check("beta.c" in bar, "beta.c is untouched", bar)
        check("gamma.c" in bar, "and gamma.c, which was never the target", bar)
    finally:
        s.close()


def test_close_others_keeps_the_one_you_clicked(binary):
    print("\nClose Other Tabs keeps the tab clicked")
    s = Session(binary)
    try:
        s.open_all()
        check(s.right_click_tab("beta.c"), "the menu appeared on beta.c")
        check(s.choose("Close Other Tabs"), "chose Close Other Tabs")

        bar = s.tab_bar()
        check("beta.c" in bar, "beta.c survived", bar)
        check("alpha.c" not in bar, "alpha.c is gone", bar)
        check("gamma.c" not in bar, "gamma.c is gone", bar)
        # And we are looking at the survivor, not at a closed file's text.
        check("beta.c" in s.status(), "and it is the active tab", s.status())
    finally:
        s.close()


def test_a_modified_tab_is_not_closed_silently(binary):
    print("\nClosing a MODIFIED tab prompts first")
    s = Session(binary)
    try:
        s.open_all()
        # gamma.c is active; dirty it, then close it from the menu.
        s.send("Z", 0.8)
        check("*" in s.tab_bar(), "gamma.c is modified", s.tab_bar())
        check(s.right_click_tab("gamma.c"), "the menu appeared")
        check(s.choose("Close Tab"), "chose Close Tab")
        body = s.text()
        check("Unsaved changes" in body or "[s]ave" in body,
              "a save prompt appeared", body[-400:])
        s.send("c", 1.2)                      # cancel
        check("gamma.c" in s.tab_bar(), "cancelling kept the tab", s.tab_bar())
    finally:
        s.close()


def test_moving_after_a_ctrl_click_keeps_the_menu(binary):
    """Reported: the menu vanished as soon as you moved towards it.

    The cause was not where it opened. SGR reports a POINTER MOVEMENT with
    bit 32, and that is orthogonal to which modifiers happen to be down --
    but the decoder tested the modifier bits first, so a movement made while
    ctrl was still held was indistinguishable from a ctrl-CLICK. The menu's
    motion branch never saw it and it fell through to "anything else
    dismisses". Letting go of ctrl before moving worked fine, which is what
    made it look like a placement problem.
    """
    print("\nMoving with ctrl still held does not dismiss the menu")
    s = Session(binary)
    try:
        s.open_all()
        hit = s.tab_entry("alpha.c")
        check(hit is not None, "found the tab entry", s.tab_bar())
        if hit is None:
            return
        s.click(hit[0], hit[1], button=16)          # ctrl + left click
        check("Close Tab" in s.text(), "ctrl+click opened the menu")

        # 35 = motion, no button, no modifier. The case that always worked.
        s.child.send(f"\x1b[<35;{hit[1] + 2};3M")
        s.drain(0.8)
        check("Close Tab" in s.text(), "plain movement keeps it")

        # 51 = the same motion with ctrl STILL HELD (32 | 3 | 16).
        s.child.send(f"\x1b[<51;{hit[1] + 2};4M")
        s.drain(0.8)
        check("Close Tab" in s.text(),
              "and so does movement with ctrl still held", s.text()[:300])
    finally:
        s.close()


def test_the_menu_hangs_under_the_row_clicked(binary):
    """No dead space between the pointer and the menu it opened."""
    print("\nA tab-bar menu opens directly under the pointer")
    s = Session(binary)
    try:
        s.open_all()
        hit = s.tab_entry("beta.c")
        if hit is None:
            check(False, "found the tab entry", s.tab_bar())
            return
        s.click(hit[0], hit[1], button=2)
        top = 0
        for i, r in enumerate(s.screen.display):
            if "┌" in r:
                top = i + 1
                break
        check(top == hit[0] + 1,
              "the menu starts on the row below the click",
              f"clicked row {hit[0]}, menu top {top}")
    finally:
        s.close()


def test_remove_from_group(binary):
    print("\nRemove from Group takes a member out but keeps the tab")
    s = Session(binary)
    try:
        s.open_all()
        s.send("\x10", 0.7)                   # palette
        s.send("Group All Tabs", 0.6)
        s.send("\r", 1.5)
        check("(" in s.tab_bar(), "a group was formed", s.tab_bar())

        m = re.search(r"\((\d+)\)", s.tab_bar())
        before = int(m.group(1)) if m else 0
        check(before == 3, "with all three in it", s.tab_bar())

        # Members are drawn on row 2 while inside the group; that entry is a
        # tab too, and must get the same menu.
        row2 = s.screen.display[1]
        j = row2.find("alpha.c")
        check(j >= 0, "alpha.c is on the members row", row2.rstrip())
        s.click(2, j + 1, button=2)
        check("Close Tab" in s.text(), "right-clicking a member gives a menu",
              s.text()[:300])
        check(s.choose("Remove from Group"), "chose Remove from Group")

        m = re.search(r"\((\d+)\)", s.tab_bar())
        after = int(m.group(1)) if m else 0
        check(after == 2, "the group is down to two", s.tab_bar())
        check("alpha.c" in s.tab_bar(), "and alpha.c is still open, ungrouped",
              s.tab_bar())
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_the_menu_appears,
               test_moving_after_a_ctrl_click_keeps_the_menu,
               test_the_menu_hangs_under_the_row_clicked,
               test_close_acts_on_the_tab_you_clicked,
               test_close_others_keeps_the_one_you_clicked,
               test_a_modified_tab_is_not_closed_silently,
               test_remove_from_group):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_tabmenu: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_tabmenu: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
