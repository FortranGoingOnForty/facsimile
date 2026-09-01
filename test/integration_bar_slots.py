#!/usr/bin/env python3
"""
Integration test: a tab bar entry lands where the preview said it would,
however far it is dragged.

Reported from a workspace with two groups and seven loose tabs: dragging a
group to the END of the bar showed a preview with it last, and then the drop
put it back near where it started. Moving it one slot at a time worked.

A bar SLOT counts a group as one entry; the reorder routines take an index
into the tabs array, where a group occupies one index per member. The drop
passed the slot straight through, so the destination was short by exactly
the members hidden behind the groups it had passed. One slot at a time
worked because nothing multi-member lay between.

Both a group and a lone tab go through the conversion, since a lone tab
dragged past a group had the same problem -- it would land inside the
group's run.

Usage: python3 test/integration_bar_slots.py [path-to-fac-binary]
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

ROWS, COLS = 30, 150

# Two groups with several members each, then loose tabs. The members are what
# make slots and indices disagree.
GROUPS = {"alpha": ("a1.c", "a2.c", "a3.c", "a4.c", "a5.c", "a6.c"),
          "bravo": ("b1.c", "b2.c", "b3.c", "b4.c")}
LOOSE = ("one.c", "two.c", "three.c", "four.c", "five.c")

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


class Bar:
    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_bs_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.root = tempfile.mkdtemp(prefix="fac_bs_work_")
        self.ws = os.path.join(self.root, "ws")
        os.makedirs(self.ws)

        groups, tabs, gid = [], [], 0
        for d, files in GROUPS.items():
            os.makedirs(os.path.join(self.ws, d))
            gid += 1
            groups.append({"id": gid, "label": d + "/",
                           "dir_path": os.path.join(self.ws, d),
                           "active_member": os.path.join(self.ws, d, files[0])})
            for o, fn in enumerate(files, start=1):
                with open(os.path.join(self.ws, d, fn), "w") as f:
                    f.write("int %s_%s;\n" % (d, fn[:-2]))
                tabs.append(self._tab(f"{d}/{fn}", gid, o))
        for fn in LOOSE:
            with open(os.path.join(self.ws, fn), "w") as f:
                f.write("int %s;\n" % fn[:-2])
            tabs.append(self._tab(fn, 0, 0))

        doc = {"version": "1.2", "workspace_path": self.ws,
               "last_opened": "20260801", "tab_groups": groups, "tabs": tabs,
               "active_tab": 1, "fuss_mode": False}
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

    @staticmethod
    def _tab(fn, gid, o):
        return {"filename": fn, "is_orphan": False, "modified": False,
                "group": gid, "group_ordinal": o,
                "panes": [{"x_start": 0.0, "y_start": 0.0, "x_end": 1.0,
                           "y_end": 1.0, "filename": fn, "cursor_line": 1,
                           "cursor_column": 1, "viewport_line": 1,
                           "viewport_column": 1}],
                "active_pane": 1}

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

    def bar(self):
        return self.screen.display[0].rstrip()

    def entries(self):
        """Bar entries in order, as short names."""
        tokens = re.findall(r"([\w.-]+/)\s+\(\d+\)|\d+\s+([\w.-]+)", self.bar())
        return [group or filename for group, filename in tokens]

    def col_of(self, label):
        """Column inside the entry showing `label`."""
        b = self.bar()
        j = b.find(label)
        return j + 1 if j >= 0 else None

    def drag(self, from_col, to_col):
        self.child.send(f"\x1b[<0;{from_col};1M")
        self.drain(0.5)
        step = 4 if to_col >= from_col else -4
        for c in range(from_col, to_col, step):
            self.child.send(f"\x1b[<32;{c};1M")
            self.drain(0.10)
        self.child.send(f"\x1b[<32;{to_col};1M")
        self.drain(0.7)
        preview = self.entries()
        self.child.send(f"\x1b[<0;{to_col};1m")
        self.drain(1.6)
        return preview, self.entries()

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.root, ignore_errors=True)


def test_a_group_dragged_to_the_end_lands_there(binary):
    print("\nA group dragged to the end of the bar lands at the end")
    s = Bar(binary)
    try:
        start = s.entries()
        check(start[:2] == ["alpha/", "bravo/"],
              "two groups lead the bar", str(start))
        src = s.col_of("bravo/")
        preview, after = s.drag(src, len(s.bar()) + 4)
        check(preview[-1] == "bravo/",
              "the preview shows it last", str(preview))
        check(after[-1] == "bravo/",
              "and it IS last after the drop", str(after))
        check(after == preview,
              "the drop matches the preview exactly",
              f"{preview} -> {after}")
    finally:
        s.close()


def test_a_group_dragged_back_to_the_front(binary):
    print("\nAnd can be dragged back to the front in one move")
    s = Bar(binary)
    try:
        src = s.col_of("bravo/")
        s.drag(src, len(s.bar()) + 4)          # send it to the end first
        src = s.col_of("bravo/")
        preview, after = s.drag(src, 2)
        check(after[0] == "bravo/", "it is first after the drop", str(after))
        check(after == preview, "and matches the preview",
              f"{preview} -> {after}")
    finally:
        s.close()


def test_one_slot_at_a_time_still_works(binary):
    print("\nA single-slot nudge still advances by one")
    s = Bar(binary)
    try:
        before = s.entries()
        i = before.index("bravo/")
        target = s.col_of(before[i + 1])
        preview, after = s.drag(s.col_of("bravo/"), target + 3)
        check(after.index("bravo/") == i + 1,
              "it moved exactly one slot right",
              f"{before} -> {after}")
    finally:
        s.close()


def test_a_lone_tab_dragged_past_a_group(binary):
    print("\nA lone tab dragged to the end clears the groups' members")
    s = Bar(binary)
    try:
        before = s.entries()
        lbl = [e for e in before if e.endswith(".c")][0]
        preview, after = s.drag(s.col_of(lbl), len(s.bar()) + 4)
        check(after[-1].endswith(".c"), "a loose tab is last", str(after))
        check(after == preview, "and the drop matches the preview",
              f"{preview} -> {after}")
        check(after.count("alpha/") == 1 and after.count("bravo/") == 1,
              "both groups survived intact", str(after))
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_a_group_dragged_to_the_end_lands_there,
               test_a_group_dragged_back_to_the_front,
               test_one_slot_at_a_time_still_works,
               test_a_lone_tab_dragged_past_a_group):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_bar_slots: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_bar_slots: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
