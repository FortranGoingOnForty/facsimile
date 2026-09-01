#!/usr/bin/env python3
"""
Integration test: editing an existing tab group.

A group could be created and never changed. Adding a file meant opening it and
letting it join by side effect; removing one meant closing the tab. The create
dialog is now a two-mode dialog -- the same list, the same navigation, opened
with the group's current members already ticked.

Reached by right-clicking a group entry in the tab bar, which gives a menu
(Edit / Rename / Dissolve) rather than opening the modal outright.

The load-bearing case is REMOVAL. Unticking a member closes its tab, and
closing a tab renumbers every index above it, so a removal that worked by
index rather than by path would close the wrong file. That is asserted by
name, on a group where the removed file is deliberately not the last one.

Usage: python3 test/integration_groupedit.py [path-to-fac-binary]
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

ROWS, COLS = 32, 100

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
    """An editor on a workspace with chapter/{one,two,three}.c and root.c."""

    def __init__(self, binary, extra_dir=False):
        self.home = tempfile.mkdtemp(prefix="fac_ge_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_ge_work_")
        os.makedirs(os.path.join(self.work, "chapter"))
        for n in ("one.c", "two.c", "three.c"):
            with open(os.path.join(self.work, "chapter", n), "w") as f:
                f.write("int %s;\n" % n[:-2])
        if extra_dir:
            os.makedirs(os.path.join(self.work, "other"))
            with open(os.path.join(self.work, "other", "far.c"), "w") as f:
                f.write("int far;\n")
        with open(os.path.join(self.work, "root.c"), "w") as f:
            f.write("int root;\n")

        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [os.path.join(self.work, "root.c")],
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

    def members_row(self):
        return self.screen.display[1].rstrip()

    def group_count(self):
        """The (n) in a tab-bar group entry, or 0 if there is no group."""
        m = re.search(r"\((\d+)\)", self.tab_bar())
        return int(m.group(1)) if m else 0

    def selected_count(self):
        """The 'N selected' the dialog footer shows."""
        m = re.search(r"(\d+) selected", self.text())
        return int(m.group(1)) if m else -1

    def make_group(self, names=("one.c", "three.c")):
        """Create chapter/ as a group holding `names`, via the tree."""
        self.send("\x02", 1.2)                       # ctrl-b, file tree
        for _ in range(14):
            if self.find("chapter"):
                break
            self.send("\x1b[B", 0.25)
        self.send("\r", 1.5)                         # Enter -> create dialog
        for n in names:
            hit = self.find("[ ] " + n)
            if hit:
                self.click(hit[0], hit[1] + 2)
        self.send("\r", 2.0)

    def open_edit_dialog(self):
        """Right-click the group entry, then choose Edit Group..."""
        hit = self.find("chapter/ (")
        if not hit:
            return False
        self.click(hit[0], hit[1] + 3, button=2)
        row = self.find("Edit Group")
        if not row:
            return False
        self.click(row[0], row[1] + 2)
        return True

    def toggle_in_dialog(self, label):
        """Click a dialog row by its visible text, toggling its tick."""
        for prefix in ("[✓] ", "[ ] "):
            hit = self.find(prefix + label)
            if hit:
                self.click(hit[0], hit[1] + 2)
                return True
        return False

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def test_right_click_gives_a_menu(binary):
    print("\nRight-clicking a group entry offers Edit / Rename / Dissolve")
    s = Session(binary)
    try:
        s.make_group()
        check(s.group_count() == 2, "the group exists with 2 members",
              s.tab_bar())
        hit = s.find("chapter/ (")
        s.click(hit[0], hit[1] + 3, button=2)
        body = s.text()
        check("Edit Group" in body, "Edit Group... is offered", body[:200])
        check("Rename Group" in body, "Rename Group... is offered")
        check("Dissolve Group" in body, "Dissolve Group is offered")
    finally:
        s.close()


def test_edit_opens_with_members_ticked(binary):
    print("\nThe dialog opens on the group, members already ticked")
    s = Session(binary)
    try:
        s.make_group()
        check(s.open_edit_dialog(), "the dialog opened")
        body = s.text()
        check("Edit Tab Group" in body, "titled Edit Tab Group", body[:300])
        check("enter save" in body, "the footer says save, not create")
        check("[✓] one.c" in body, "one.c is ticked", body)
        check("[✓] three.c" in body, "three.c is ticked")
        check("[ ] two.c" in body, "two.c is not")
        check(s.selected_count() == 2, "the footer counts 2 selected",
              str(s.selected_count()))
    finally:
        s.close()


def test_ticking_adds_a_member(binary):
    print("\nTicking another file adds it to the group")
    s = Session(binary)
    try:
        s.make_group()
        s.open_edit_dialog()
        check(s.toggle_in_dialog("two.c"), "ticked two.c")
        s.send("\r", 2.0)
        check(s.group_count() == 3, "the group now holds 3", s.tab_bar())
        check("two.c" in s.members_row(), "two.c is on the members row",
              s.members_row())
    finally:
        s.close()


def test_unticking_closes_the_tab(binary):
    print("\nUnticking a member closes its tab")
    # three.c deliberately, NOT the last member: closing a tab renumbers every
    # index above it, so a removal working by index would take the wrong file.
    s = Session(binary)
    try:
        s.make_group(names=("one.c", "two.c", "three.c"))
        check(s.group_count() == 3, "the group holds 3 to begin with",
              s.tab_bar())
        s.open_edit_dialog()
        check(s.toggle_in_dialog("two.c"), "unticked two.c")
        s.send("\r", 2.0)
        check(s.group_count() == 2, "the group now holds 2", s.tab_bar())
        check("two.c" not in s.members_row(), "two.c is gone",
              s.members_row())
        check("one.c" in s.members_row(), "one.c survived", s.members_row())
        check("three.c" in s.members_row(), "and so did three.c",
              s.members_row())
    finally:
        s.close()


def test_cancel_changes_nothing(binary):
    print("\nEsc leaves the group exactly as it was")
    s = Session(binary)
    try:
        s.make_group()
        before = s.tab_bar()
        members_before = s.members_row()
        s.open_edit_dialog()
        s.toggle_in_dialog("two.c")           # would add
        s.toggle_in_dialog("one.c")           # would remove
        s.send("\x1b", 1.5)
        check(s.tab_bar() == before, "the tab bar is unchanged",
              f"{before!r} -> {s.tab_bar()!r}")
        check(s.members_row() == members_before, "so is the members row",
              f"{members_before!r} -> {s.members_row()!r}")
    finally:
        s.close()


def test_the_name_field_renames_the_group(binary):
    print("\nEditing the name renames the group")
    s = Session(binary)
    try:
        s.make_group()
        s.open_edit_dialog()
        # Focus starts on the name field. Clear it and type a new one.
        for _ in range(12):
            s.send("\x7f", 0.1)
        s.send("bundle", 0.4)
        s.send("\r", 2.0)
        check("bundle" in s.tab_bar(), "the tab bar shows the new name",
              s.tab_bar())
        check("chapter/ (" not in s.tab_bar(), "and not the old one",
              s.tab_bar())
    finally:
        s.close()


def test_a_modified_member_is_not_closed_silently(binary):
    print("\nUnticking a MODIFIED member prompts, and cancel keeps it")
    s = Session(binary)
    try:
        s.make_group(names=("one.c", "two.c", "three.c"))
        # Dirty whatever we landed on, and find out WHICH from the status bar
        # rather than assuming -- the tab creation lands on is an
        # implementation detail, and guessing it wrong silently turns this
        # into a test of the clean-file path.
        s.send("X", 0.8)
        # The members row uses the semantic modified glyph: a filled circle
        # in Unicode mode and an asterisk for the ASCII fallback.
        m = re.search(r"(\w+\.c)[●*]", s.members_row())
        dirty = m.group(1) if m else None
        check(dirty is not None, "found which member is now modified",
              s.members_row())
        if dirty is None:
            return
        print(f"      (the modified member is {dirty})")
        s.open_edit_dialog()
        body = s.text()
        # The dialog has to say which rows have unsaved work BEFORE Enter,
        # because unticking one is what closes it.
        check("•" in body, "the modified member is marked in the dialog",
              body[:600])
        s.toggle_in_dialog(dirty)
        s.send("\r", 2.0)
        after = s.text()
        check("Save" in after or "save" in after,
              "a save prompt appeared rather than a silent close", after[-400:])
        # Cancel it. Both the tab and its membership must survive.
        s.send("c", 1.5)
        check(s.group_count() == 3, "the group still holds 3", s.tab_bar())
        check(dirty in s.members_row(), "the modified file is still a member",
              s.members_row())
    finally:
        s.close()


def test_a_member_in_another_directory_survives(binary):
    print("\nA member outside the listed directory is kept, not dropped")
    s = Session(binary)
    try:
        s.make_group()
        s.open_edit_dialog()
        # Walk up and into another directory: the ticked set is by path and
        # must survive the walk, or confirming here would drop both members.
        up = s.find("▴ ..")
        check(up is not None, "the .. row is there")
        s.click(up[0], up[1] + 2)
        check(s.selected_count() == 2, "still 2 selected after walking up",
              str(s.selected_count()))
        s.send("\r", 2.0)
        check(s.group_count() == 2, "and the group is intact after saving",
              s.tab_bar())
    finally:
        s.close()


def test_a_restored_group_opens_with_members_ticked(binary):
    """The reported bug: after a restart, nothing was ticked.

    workspace.json stores a group's dir_path RELATIVE to the workspace while
    its members' paths are absolute. Restoring left the group's directory
    meaning "wherever the process happens to be", so no member could ever
    match a file listed in it and the dialog opened with every box empty.

    Every other case in this file builds its group in the same session, where
    the directory is absolute because the dialog put it there -- which is
    exactly why they all passed while this was broken.
    """
    print("\nA group restored from workspace.json opens with members ticked")
    home = tempfile.mkdtemp(prefix="fac_ge_home_")
    os.makedirs(os.path.join(home, ".config", "fac"))
    with open(os.path.join(home, ".config", "fac", "state.json"), "w") as f:
        f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                ' "version": "1.0"}\n')
    ws = tempfile.mkdtemp(prefix="fac_ge_work_")
    os.makedirs(os.path.join(ws, "chapter", "sub"))
    for n in ("main.c", "helper.c", "extra.c"):
        with open(os.path.join(ws, "chapter", "sub", n), "w") as f:
            f.write("int x;\n")

    def tab(fn, gid, ordi):
        return {"filename": fn, "is_orphan": False, "modified": False,
                "group": gid, "group_ordinal": ordi,
                "panes": [{"x_start": 0.0, "y_start": 0.0, "x_end": 1.0,
                           "y_end": 1.0, "filename": fn, "cursor_line": 1,
                           "cursor_column": 1, "viewport_line": 1,
                           "viewport_column": 1}],
                "active_pane": 1}

    doc = {"version": "1.2", "workspace_path": ws, "last_opened": "20260731",
           "tab_groups": [{"id": 1, "label": "chapter/",
                           "dir_path": "chapter/sub",        # RELATIVE
                           "active_member": os.path.join(ws, "chapter/sub/main.c")}],
           "tabs": [tab("chapter/sub/main.c", 1, 1),
                    tab("chapter/sub/helper.c", 1, 2)],
           "active_tab": 1, "fuss_mode": False}
    os.makedirs(os.path.join(ws, ".fac"))
    with open(os.path.join(ws, ".fac", "workspace.json"), "w") as f:
        json.dump(doc, f, indent=2)

    env = {**os.environ, "TERM": "xterm-256color", "HOME": home}
    env.pop("XDG_CONFIG_HOME", None)
    env.pop("FAC_SESSION", None)
    s = Session.__new__(Session)
    s.home, s.work = home, ws
    s.screen = pyte.Screen(COLS, ROWS)
    s.stream = pyte.Stream(s.screen)
    s.child = pexpect.spawn(binary, [ws], dimensions=(ROWS, COLS), env=env, cwd=ws)
    s.drain(3.0)
    try:
        check(s.group_count() == 2, "the group came back with two members",
              s.tab_bar())
        hit = s.find("chapter/ (")
        check(hit is not None, "its entry is on the tab bar", s.tab_bar())
        if hit is None:
            return
        s.click(hit[0], hit[1] + 2, button=2)
        row = s.find("Edit Group")
        check(row is not None, "the menu opened")
        if row is None:
            return
        s.click(row[0], row[1] + 2)

        body = s.text()
        check("Edit Tab Group" in body, "the dialog opened", body[:300])
        check("[\u2713] main.c" in body, "main.c is ticked", body)
        check("[\u2713] helper.c" in body, "helper.c is ticked", body)
        check("[ ] extra.c" in body, "and extra.c, a non-member, is not", body)
        check(s.selected_count() == 2, "the footer counts both",
              str(s.selected_count()))
    finally:
        s.close()


def main():
    binary = find_binary()
    test_right_click_gives_a_menu(binary)
    test_edit_opens_with_members_ticked(binary)
    test_ticking_adds_a_member(binary)
    test_unticking_closes_the_tab(binary)
    test_cancel_changes_nothing(binary)
    test_the_name_field_renames_the_group(binary)
    test_a_modified_member_is_not_closed_silently(binary)
    test_a_member_in_another_directory_survives(binary)
    test_a_restored_group_opens_with_members_ticked(binary)

    print()
    if failures:
        print(f"integration_groupedit: FAILED ({len(failures)}): "
              + ", ".join(failures))
        sys.exit(1)
    print("integration_groupedit: ALL PASSED")


if __name__ == "__main__":
    main()
