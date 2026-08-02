#!/usr/bin/env python3
"""
Integration test: the fuss row that is HIGHLIGHTED is the row enter acts on.

Reported from a workspace with several tab groups: with the tree open, the
highlight sat on a directory, and pressing enter opened an unrelated file
from further down the tree instead of offering to make a tab group.

The tree draws its rows by walking expanded directories; the keyboard
indexes a separate list built by the same walk. refresh_tree_state built
that list, and THEN reveal_open_files opened one directory per open tab --
so every child revealed was on screen but missing from the list, and the
index the highlight meant was not the index enter used. The gap was exactly
the number of revealed children, which is why it grew with the number of
groups and looked arbitrary.

The list is now marked stale whenever a directory opens and rebuilt at the
point of use, so a future caller cannot reintroduce this by forgetting to
rebuild.

Usage: python3 test/integration_fuss_cursor.py [path-to-fac-binary]
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

ROWS, COLS = 30, 120

# Several groups, each in its own directory, so opening the tree reveals
# several directories' worth of children after the list has been built.
LAYOUT = {
    "alpha": ("main.c", "alpha.c", "alpha.h"),
    "bravo": ("main.c", "bravo.c"),
    "charlie": ("main.c", "charlie.c", "charlie.h", "Makefile"),
    "target": ("one.c", "two.c", "three.h"),
}
LOOSE = ("zeta.c", "yankee.c")

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


class Tree:
    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_fc_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.root = tempfile.mkdtemp(prefix="fac_fc_work_")
        self.ws = os.path.join(self.root, "ws")
        os.makedirs(self.ws)
        for d, files in LAYOUT.items():
            os.makedirs(os.path.join(self.ws, d))
            for fn in files:
                with open(os.path.join(self.ws, d, fn), "w") as f:
                    f.write("int %s_%s;\n" % (d, fn.replace(".", "_")))
        for fn in LOOSE:
            with open(os.path.join(self.ws, fn), "w") as f:
                f.write("int %s;\n" % fn.replace(".", "_"))

        # Groups for every directory EXCEPT target/, which stays closed and
        # is what the test presses enter on.
        groups, tabs, gid = [], [], 0
        for d in ("alpha", "bravo", "charlie"):
            gid += 1
            groups.append({"id": gid, "label": d + "/",
                           "dir_path": os.path.join(self.ws, d),
                           "active_member": os.path.join(self.ws, d, "main.c")})
            for o, fn in enumerate(LAYOUT[d], start=1):
                tabs.append({"filename": f"{d}/{fn}", "is_orphan": False,
                             "modified": False, "group": gid, "group_ordinal": o,
                             "panes": [{"x_start": 0.0, "y_start": 0.0,
                                        "x_end": 1.0, "y_end": 1.0,
                                        "filename": f"{d}/{fn}", "cursor_line": 1,
                                        "cursor_column": 1, "viewport_line": 1,
                                        "viewport_column": 1}],
                             "active_pane": 1})
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

    def send(self, d, w=0.4):
        self.child.send(d)
        self.drain(w)

    def selected(self):
        """The highlighted tree row. Rows 1-2 are the tab bar and the group
        member strip, which carry reverse video of their own."""
        for y in range(2, ROWS - 1):
            if any(self.screen.buffer[y][x].reverse for x in range(0, 30)):
                return y + 1, "".join(self.screen.buffer[y][x].data
                                      for x in range(0, 30)).strip()
        return None, None

    def text(self):
        return "\n".join(r.rstrip() for r in self.screen.display)

    def dialog(self):
        """Just the modal box. The tab bar behind it lists other groups'
        files, so scanning the whole screen would find them there."""
        rows = [r for r in self.screen.display if "\u2502" in r or "\u256d" in r]
        return "\n".join(r.rstrip() for r in rows)

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.root, ignore_errors=True)


def walk_to(s, want, limit=40):
    """Press down until the HIGHLIGHTED row reads `want`."""
    for _ in range(limit):
        _, t = s.selected()
        if t and want in t:
            return True
        s.send("\x1b[B", 0.18)
    _, t = s.selected()
    return bool(t and want in t)


def test_enter_acts_on_the_highlighted_directory(binary):
    print("\nEnter offers a group for the directory that is highlighted")
    s = Tree(binary)
    try:
        s.send("\x02", 1.5)
        found = walk_to(s, "target")
        row, label = s.selected()
        check(found, "walked the highlight onto target/", repr(label))
        if not found:
            return

        s.send("\r", 1.5)
        body = s.text()
        check("New Tab Group" in body,
              "enter on a directory offers a tab group", body[:200])
        check("target" in body,
              "and the group is for target/, the highlighted row",
              [l for l in body.split("\n") if "Name" in l][:1])
        # The files listed must be target/'s own, not another directory's.
        box = s.dialog()
        check("one.c" in box and "two.c" in box,
              "listing target/'s own files", box[:200])
        check("alpha.c" not in box and "charlie.c" not in box,
              "and nobody else's", box[:200])
    finally:
        s.close()


def test_enter_on_a_file_opens_that_file(binary):
    print("\nAnd on a file, it opens that same file")
    s = Tree(binary)
    try:
        s.send("\x02", 1.5)
        found = walk_to(s, "zeta.c")
        row, label = s.selected()
        check(found, "walked the highlight onto zeta.c", repr(label))
        if not found:
            return

        s.send("\r", 1.5)
        check("zeta.c" in s.screen.display[0],
              "zeta.c is the tab that opened", s.screen.display[0][:110])
        check("int zeta_c" in s.text(),
              "and its text is what is showing")
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_enter_acts_on_the_highlighted_directory,
               test_enter_on_a_file_opens_that_file):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_fuss_cursor: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_fuss_cursor: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
