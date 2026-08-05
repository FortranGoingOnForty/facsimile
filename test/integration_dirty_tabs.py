#!/usr/bin/env python3
"""
Integration test: many tabs may hold unsaved changes at once.

Reported after the dirty-flag rework: editing a group member and switching
away cleared its asterisk, so the change looked saved. It was never written
-- the text stayed in the tab's buffer -- but a mark that goes out on its
own cannot be trusted, and the point of the mark is to be trusted.

The cause was that dirtiness lived in THREE places that copied each other:
buffer%modified, editor%modified, and the tab's own flag. Leaving a tab
copied editor%modified onto the tab, and nothing that edits text ever sets
editor%modified, so the mark was cleared by the act of leaving.

The tab now owns the answer, derived from the text; the other two follow it.
These tests are about the property that matters: several files can be dirty
at the same time, each independently, and none of them is written until it
is asked to be.

Usage: python3 test/integration_dirty_tabs.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import json
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

ROWS, COLS = 30, 150
MEMBERS = ("main.c", "two.c", "three.c", "four.c")

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


class Group:
    """One tab group whose members are all open."""

    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_dt_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.root = tempfile.mkdtemp(prefix="fac_dt_work_")
        self.ws = os.path.join(self.root, "ws")
        os.makedirs(os.path.join(self.ws, "sort"))

        self.original = {}
        tabs = []
        for o, fn in enumerate(MEMBERS, start=1):
            rel = f"sort/{fn}"
            body = "int %s;\nint second;\n" % fn[:-2]
            with open(os.path.join(self.ws, rel), "w") as f:
                f.write(body)
            self.original[rel] = body
            tabs.append({"filename": rel, "is_orphan": False, "modified": False,
                         "group": 1, "group_ordinal": o,
                         "panes": [{"x_start": 0.0, "y_start": 0.0, "x_end": 1.0,
                                    "y_end": 1.0, "filename": rel,
                                    "cursor_line": 1, "cursor_column": 1,
                                    "viewport_line": 1, "viewport_column": 1}],
                         "active_pane": 1})
        doc = {"version": "1.2", "workspace_path": self.ws,
               "last_opened": "20260801",
               "tab_groups": [{"id": 1, "label": "sort/",
                               "dir_path": os.path.join(self.ws, "sort"),
                               "active_member": os.path.join(self.ws, "sort", MEMBERS[0])}],
               "tabs": tabs, "active_tab": 1, "fuss_mode": False}
        os.makedirs(os.path.join(self.ws, ".fac"))
        with open(os.path.join(self.ws, ".fac", "workspace.json"), "w") as f:
            json.dump(doc, f, indent=2)

        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.ws], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.ws)
        self.drain(3.0)

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

    def send(self, d, w=0.7):
        self.child.send(d)
        self.drain(w)

    def marked(self):
        """Member names currently showing an unsaved-changes asterisk."""
        return sorted(w.rstrip("*") for w in self.screen.display[1].split()
                      if w.endswith("*"))

    def showing(self):
        return self.screen.display[2].rstrip()[6:44]

    def changed_on_disk(self):
        out = []
        for rel, body in self.original.items():
            with open(os.path.join(self.ws, rel)) as f:
                if f.read() != body:
                    out.append(rel)
        return sorted(out)

    def next_member(self):
        self.send("\x1b[6;5~", 1.0)

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.root, ignore_errors=True)


def test_the_mark_survives_switching_away(binary):
    print("\nAn unsaved change stays marked after switching away and back")
    s = Group(binary)
    try:
        s.send("Z", 0.8)
        check(s.marked() == ["main.c"], "main.c is marked", str(s.marked()))
        text = s.showing()

        s.next_member()
        check(s.marked() == ["main.c"],
              "still marked while another member is showing", str(s.marked()))
        check(s.changed_on_disk() == [],
              "and nothing was written", str(s.changed_on_disk()))

        s.send("\x1b[5;5~", 1.0)              # back
        check(s.showing() == text, "the edit is still in the buffer",
              f"{text!r} -> {s.showing()!r}")
        check(s.marked() == ["main.c"], "and still marked", str(s.marked()))
    finally:
        s.close()


def test_several_files_dirty_at_once(binary):
    print("\nSeveral members can hold unsaved changes at the same time")
    s = Group(binary)
    try:
        for i in range(3):
            s.send("Q", 0.6)
            s.next_member()
        marked = s.marked()
        check(len(marked) == 3, "three members are marked", str(marked))
        check(s.changed_on_disk() == [],
              "with nothing written to disk", str(s.changed_on_disk()))
    finally:
        s.close()


def test_saving_one_leaves_the_others_marked(binary):
    print("\nSaving one file clears only that one")
    s = Group(binary)
    try:
        for i in range(3):
            s.send("Q", 0.6)
            s.next_member()
        before = s.marked()
        if len(before) != 3:
            check(False, "three members marked to begin with", str(before))
            return

        # go back to the first and save it
        for _ in range(3):
            s.send("\x1b[5;5~", 0.8)
        s.send("\x13", 1.2)
        after = s.marked()
        check(len(after) == 2, "one fewer marked after saving",
              f"{before} -> {after}")
        check(s.changed_on_disk() == ["sort/main.c"],
              "and exactly one file was written", str(s.changed_on_disk()))
    finally:
        s.close()


def test_each_file_keeps_its_own_text(binary):
    print("\nEach member keeps its own text across the switching")
    s = Group(binary)
    try:
        s.send("A", 0.7)
        first = s.showing()
        s.next_member()
        second_before = s.showing()
        s.send("B", 0.7)
        second_after = s.showing()
        check(second_after != second_before, "the second member took its edit")
        check("main" not in second_after,
              "and is not showing the first member's text", second_after)

        s.send("\x1b[5;5~", 1.0)
        check(s.showing() == first, "the first member is unchanged by all that",
              f"{first!r} -> {s.showing()!r}")
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_the_mark_survives_switching_away,
               test_several_files_dirty_at_once,
               test_saving_one_leaves_the_others_marked,
               test_each_file_keeps_its_own_text):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_dirty_tabs: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_dirty_tabs: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
