#!/usr/bin/env python3
"""
Integration test: the wheel scrolls whichever pane the pointer is OVER.

Reported from a split: scrolling an inactive pane moved a little way and then
stopped, and the only way to carry on was to click into it first.

The cause was the clamp. scroll_pane_at found the right pane to move but
worked out how far it could go from the ACTIVE pane's document -- its line
count and the whole text-area height -- so a long file beside a short one
stopped at the short file's last line. Clicking in made it active, which is
why clicking "fixed" it.

The clamp now comes from the hovered pane: its own buffer, its own height,
less the row it spends on its header.

Usage: python3 test/integration_pane_scroll.py [path-to-fac-binary]
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

ROWS, COLS = 30, 120
LONG = 400          # lines in the file opened in the second pane
SHORT = 39          # lines in the file opened first, and left active

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


class Split:
    """A short file on the left and active, a long one on the right."""

    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_scr_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_scr_work_")
        with open(os.path.join(self.work, "main.c"), "w") as f:
            f.writelines(f"int a{i};\n" for i in range(1, SHORT + 1))
        with open(os.path.join(self.work, "other.c"), "w") as f:
            f.writelines(f"int marker{i} = {i};\n" for i in range(1, LONG + 1))

        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [os.path.join(self.work, "main.c")],
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

    def send(self, d, w=0.4):
        self.child.send(d)
        self.drain(w)

    def open_split(self):
        """alt-v on a tree row puts that file BESIDE this one."""
        self.send("\x02", 1.2)
        sel = None
        for _ in range(10):
            for y in range(1, ROWS - 1):
                if any(self.screen.buffer[y][x].reverse for x in range(30)):
                    sel = "".join(self.screen.buffer[y][x].data
                                  for x in range(30)).strip()
                    break
            if sel and "other.c" in sel:
                break
            self.send("\x1b[B", 0.25)
        self.send("\x1bv", 1.8)
        self.send("\x1bh", 0.6)             # leave the LEFT pane active
        return sel is not None and "other.c" in sel

    def top_line(self, gutter_x):
        """The first line number drawn in a pane, by its gutter column."""
        for y in range(2, ROWS - 2):
            # The gutter is followed by the line's text, so take the first
            # token rather than the whole slice.
            t = "".join(self.screen.buffer[y][x].data
                        for x in range(gutter_x, gutter_x + 8)).split()
            if t and t[0].isdigit():
                return int(t[0])
        return None

    def left_top(self):
        return self.top_line(0)

    def right_top(self):
        return self.top_line(56)

    def right_text(self):
        return "\n".join("".join(self.screen.buffer[y][x].data
                                 for x in range(56, COLS - 1))
                         for y in range(2, ROWS - 2))

    def wheel(self, n, col, row, up=False):
        code = 64 if up else 65
        for _ in range(n):
            self.child.send(f"\x1b[<{code};{col};{row}M")
            self.drain(0.12)

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def test_hovering_scrolls_past_the_active_file_length(binary):
    print("\nAn inactive pane scrolls on its own length, not the active one's")
    s = Split(binary)
    try:
        if not s.open_split():
            check(False, "opened other.c in a vertical split")
            return
        check(s.right_top() == 1, "the right pane starts at line 1",
              str(s.right_top()))

        s.wheel(30, 90, 10)                 # 30 ticks over the RIGHT pane
        top = s.right_top()
        check(top is not None and top > SHORT,
              f"scrolled past line {SHORT}, where the active file ends",
              f"stopped at {top}")
        check(top == 91, "and moved exactly three lines a tick", str(top))
    finally:
        s.close()


def test_it_still_stops_at_the_end(binary):
    print("\nAnd stops at the end of its own file rather than past it")
    s = Split(binary)
    try:
        if not s.open_split():
            check(False, "opened other.c in a vertical split")
            return
        s.wheel(200, 90, 10)                # far more than enough
        top = s.right_top()
        check(top is not None and top < LONG,
              "the top line is still a real line of the file", str(top))
        check(f"marker{LONG} " in s.right_text(),
              f"the last line, marker{LONG}, is on screen")

        s.wheel(10, 90, 10)                 # keep going; nothing should move
        check(s.right_top() == top, "and further ticks do not move it",
              f"{top} -> {s.right_top()}")
    finally:
        s.close()


def test_the_active_pane_is_left_alone(binary):
    print("\nScrolling one pane does not move the other")
    s = Split(binary)
    try:
        if not s.open_split():
            check(False, "opened other.c in a vertical split")
            return
        before = s.left_top()
        s.wheel(30, 90, 10)
        check(s.left_top() == before, "the left pane has not moved",
              f"{before} -> {s.left_top()}")

        s.wheel(4, 20, 10)                  # now over the LEFT pane
        check(s.left_top() != before, "but the wheel does move it when hovered",
              f"{before} -> {s.left_top()}")
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_hovering_scrolls_past_the_active_file_length,
               test_it_still_stops_at_the_end,
               test_the_active_pane_is_left_alone):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_pane_scroll: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_pane_scroll: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
