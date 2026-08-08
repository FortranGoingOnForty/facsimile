#!/usr/bin/env python3
"""
Integration test: backing out of a directory puts you back on it.

Left arrow ascends to the parent. It used to resolve the new selection
eagerly, in the key handler:

    case ('D')  ! Left arrow - go to parent
        temp_dir = curr_dir
        curr_dir = get_parent_path(curr_dir)
        sel = find_in_parent(temp_dir, files, file_count)

`files` there is still the listing of the directory being LEFT -- the parent
is not read until `fortress_sync` runs, which happens at render time, after
the key has been handled. So it searched the child's own listing for the
child's name, never found it, and `find_in_parent` fell back to 1. Descend
into the tenth directory, back out, and the selection was on the first: you
lost your place every time you looked into something and came back.

The fix records which directory was left and lets the sync resolve it once
the parent listing exists. Same lesson as the fuss cursor bug -- derive at
the point of use, not before the data is there.

Usage: python3 test/integration_fortress_nav.py [path-to-fac-binary]
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

ROWS, COLS = 28, 110
CTRL_O = "\x0f"
UP, DOWN, RIGHT, LEFT = "\x1b[A", "\x1b[B", "\x1b[C", "\x1b[D"

# Enough directories that landing on row 1 is unmistakably wrong rather than
# a lucky coincidence.
DIRS = ["alpha", "bravo", "charlie", "delta", "echo", "foxtrot"]

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


class Editor:
    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_fn_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_fn_work_")
        for d in DIRS:
            os.makedirs(os.path.join(self.work, d))
            # A child of its own, so descending has something to show and the
            # listing we come back to is demonstrably a different one.
            with open(os.path.join(self.work, d, "inside_%s.txt" % d), "w") as f:
                f.write("%s\n" % d)
        self.target = os.path.join(self.work, "top.c")
        with open(self.target, "w") as f:
            f.writelines("int line%02d = %d;\n" % (i, i) for i in range(1, 20))

        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.work)
        self.drain(2.5)

    def drain(self, w=0.6):
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

    def text(self):
        return "\n".join(r.rstrip() for r in self.screen.display)

    def has_box(self):
        return "╭" in self.text() and "FORTRESS" in self.text()

    def box_cols(self):
        """(left, right) screen columns the window spans."""
        for y in range(ROWS):
            row = self.screen.display[y]
            if "╭" in row and "╮" in row:
                return row.index("╭"), row.index("╮")
        return None, None

    def selection(self):
        """The highlighted entry in the CURRENT (right-hand) pane.

        Both panes mark their highlight with bold+underline, and attributes
        never appear in screen.display -- comparing rendered text would call a
        moved selection 'no change'. The parent pane is the left 30% of the
        window's interior, so the current pane is everything right of that."""
        l, r = self.box_cols()
        if l is None:
            return None
        split = l + 1 + (r - l - 1) * 3 // 10
        for y in range(ROWS):
            cells = [self.screen.buffer[y][x] for x in range(split, min(r, COLS))]
            hit = "".join(c.data for c in cells if c.underscore and c.bold).strip()
            if hit:
                return hit
        return None

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def walk_to(s, name, limit=20):
    """Move the selection onto `name`, returning True if it got there."""
    for _ in range(limit):
        if s.selection() == name:
            return True
        s.send(DOWN, 0.3)
    return s.selection() == name


def wait_for_selection(s, name, limit=6.0):
    """Poll rather than sleep a fixed amount. A wrong selection is wrong
    however long you wait, so this cannot mask the defect -- it only stops a
    loaded machine from reporting a slow repaint as one."""
    end = time.time() + limit
    while time.time() < end:
        if s.selection() == name:
            return True
        s.drain(0.3)
    return s.selection() == name


def wait_for_text(s, needle, limit=6.0):
    end = time.time() + limit
    while time.time() < end:
        if needle in s.text():
            return True
        s.drain(0.3)
    return needle in s.text()


def test_left_returns_to_the_directory_you_left(binary):
    print("\nBacking out of a directory selects the one you backed out of")
    s = Editor(binary)
    try:
        s.send(CTRL_O, 1.8)
        check(s.has_box(), "the window opened", s.text()[:200])

        target = "foxtrot/"
        check(walk_to(s, target),
              "walked the selection onto a directory deep in the list",
              f"selection is {s.selection()!r}")
        if s.selection() != target:
            return

        s.send(RIGHT, 0.5)
        check(wait_for_text(s, "inside_foxtrot"),
              "Right descended into it", s.text()[:300])

        s.send(LEFT, 0.5)
        # The assertion. It used to be row 1 -- 'alpha/' -- every time.
        ok = wait_for_selection(s, target)
        check("inside_foxtrot" not in s.text(),
              "Left came back out", s.text()[:300])
        check(ok, "and the selection is on the directory just left",
              f"expected {target!r}, got {s.selection()!r}")
    finally:
        s.close()


def test_it_survives_a_second_round_trip(binary):
    print("\nAnd it still holds on the way back down and out again")
    s = Editor(binary)
    try:
        s.send(CTRL_O, 1.8)
        if not walk_to(s, "charlie/"):
            check(False, "walked onto charlie/", f"{s.selection()!r}")
            return
        s.send(RIGHT, 0.5)
        s.send(LEFT, 0.5)
        check(wait_for_selection(s, "charlie/"), "back on charlie/",
              f"{s.selection()!r}")
        # Move on, descend somewhere else, and come back: the remembered
        # directory has to be the one just left, not the first one ever left.
        if not walk_to(s, "delta/"):
            check(False, "walked onto delta/", f"{s.selection()!r}")
            return
        s.send(RIGHT, 0.5)
        s.send(LEFT, 0.5)
        check(wait_for_selection(s, "delta/"),
              "and on delta/ after the second trip, not the earlier one",
              f"{s.selection()!r}")
    finally:
        s.close()


def test_ascending_past_the_top_is_still_sane(binary):
    print("\nAscending repeatedly does not wedge or crash")
    s = Editor(binary)
    try:
        s.send(CTRL_O, 1.8)
        for _ in range(12):            # walk out to / and keep pressing
            s.send(LEFT, 0.25)
        check(s.child.isalive(), "still running after ascending past /")
        check(s.has_box(), "and the window is still up", s.text()[:200])
        check(s.selection() is not None,
              "with something selected", f"{s.selection()!r}")
    finally:
        s.close()


class BigDir(Editor):
    """A directory with more entries than fit, so a page key has somewhere to
    go. Home/End/PageUp/PageDown did nothing in either driver: Home and End
    reached handle_arrow_key as 'H' and 'F' and matched no case, and the two
    tilde-terminated page keys were swallowed by the escape parser as
    unrecognised CSI. A few hundred entries were reachable one row at a
    time."""

    COUNT = 40

    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_fb_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_fb_work_")
        for i in range(self.COUNT):
            os.makedirs(os.path.join(self.work, "d%02d" % i))
        self.target = os.path.join(self.work, "zz_top.c")
        with open(self.target, "w") as f:
            f.writelines("int line%02d = %d;\n" % (i, i) for i in range(1, 20))
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.work)
        self.drain(2.5)


def test_home_end_and_the_page_keys_move(binary):
    print("\nHome, End and the page keys move through a long listing")
    s = BigDir(binary)
    try:
        s.send(CTRL_O, 1.8)
        check(s.has_box(), "the window opened", s.text()[:200])
        first = s.selection()
        check(first == "d00/", "it starts at the top", f"{first!r}")

        s.send("\x1b[F", 0.8)                 # End
        # The last entry: directories sort before the file, so it is the file.
        check(s.selection() == "zz_top.c",
              "End reaches the bottom of the listing", f"{s.selection()!r}")

        # Move off row 1 with arrows FIRST, so Home has something to undo.
        # Reaching it from a selection that never left the top passes whether
        # or not Home does anything at all.
        s.send("\x1b[H", 0.8)                 # Home
        for _ in range(5):
            s.send(DOWN, 0.2)
        check(s.selection() == "d05/", "arrows still step one row",
              f"{s.selection()!r}")
        s.send("\x1b[H", 0.8)                 # Home
        check(s.selection() == "d00/", "Home comes back to the top",
              f"{s.selection()!r}")

        s.send("\x1b[6~", 0.8)                # PageDown
        after = s.selection()
        check(after not in ("d00/", "d01/", None),
              "PageDown moves by more than a row", f"{after!r}")

        # Likewise from the bottom, not from wherever PageDown left us.
        s.send("\x1b[F", 0.6)
        s.send("\x1b[5~", 0.8)                # PageUp
        check(s.selection() not in ("zz_top.c", None),
              "and PageUp moves back up", f"{s.selection()!r}")
    finally:
        s.close()


def test_the_page_keys_reach_the_full_screen_browser_too(binary):
    print("\nThe same keys work in the full-screen browser")
    s = BigFullScreen(binary)
    try:
        check("FORTRESS" in s.text(), "the browser is up", s.text()[:200])
        check(s.selection() == "d00/", "starts at the top", f"{s.selection()!r}")
        s.send("\x1b[F", 0.8)
        end_sel = s.selection()
        check(end_sel not in ("d00/", None),
              "End moved off the first row", f"{end_sel!r}")
        # From the bottom, so Home is not asserted against a selection that
        # never moved.
        s.send("\x1b[H", 0.8)
        check(s.selection() == "d00/", "Home came back", f"{s.selection()!r}")
        s.send("\x1b[6~", 0.8)
        check(s.selection() not in ("d00/", "d01/", None),
              "PageDown moved by more than a row", f"{s.selection()!r}")
    finally:
        s.close()


class FullScreen(Editor):
    """`fac` with no arguments, then `b` for browse, gets the BLOCKING
    full-screen browser rather than the window -- there is no editor for it to
    be a window inside. It is a second driver over the same navigation state,
    and the one with no coverage otherwise: it runs before the editor exists,
    so it is the easier of the two to break silently."""

    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_fs_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_fs_work_")
        for d in DIRS:
            os.makedirs(os.path.join(self.work, d))
            with open(os.path.join(self.work, d, "inside_%s.txt" % d), "w") as f:
                f.write("%s\n" % d)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.work)
        self.drain(2.5)
        self.send("b", 1.5)                   # welcome menu -> browse

    def box_cols(self):
        # No box: the browser owns the whole terminal, and the pane split is
        # the same 30% of it.
        return -1, COLS


class BigFullScreen(FullScreen):
    """The long listing, in the blocking driver. The page keys had to be
    taught to its own escape parser separately -- it does not go through the
    editor's input layer."""

    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_fbs_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_fbs_work_")
        for i in range(BigDir.COUNT):
            os.makedirs(os.path.join(self.work, "d%02d" % i))
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.work)
        self.drain(2.5)
        self.send("b", 1.5)                   # welcome menu -> browse


def test_the_full_screen_browser_does_it_too(binary):
    print("\nThe full-screen browser keeps your place the same way")
    s = FullScreen(binary)
    try:
        check("FORTRESS" in s.text(), "the browser is up", s.text()[:200])
        if not walk_to(s, "echo/"):
            check(False, "walked onto echo/", f"{s.selection()!r}")
            return
        check(True, "walked the selection onto a directory")
        s.send(RIGHT, 0.5)
        check(wait_for_text(s, "inside_echo"), "Right descended", s.text()[:300])
        s.send(LEFT, 0.5)
        check(wait_for_selection(s, "echo/"),
              "and Left came back onto it", f"{s.selection()!r}")
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_left_returns_to_the_directory_you_left,
               test_it_survives_a_second_round_trip,
               test_ascending_past_the_top_is_still_sane,
               test_the_full_screen_browser_does_it_too,
               test_home_end_and_the_page_keys_move,
               test_the_page_keys_reach_the_full_screen_browser_too):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_fortress_nav: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_fortress_nav: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
