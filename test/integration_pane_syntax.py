#!/usr/bin/env python3
"""
Integration test: syntax highlighting is per-pane, not per-terminal.

Reported from a split with main.c beside a getopt source: scrolling the getopt
pane turned the WHOLE of main.c into a comment, and block comments in the
getopt pane flickered between comment and code while scrolling.

One cause. render_buffer_line_in_pane -- the line renderer used whenever there
is more than one pane -- called the tokenizer without ever seeding its
multi-line comment state. So the tokenizer carried on from whatever line it
had colourised last, which with a split is a line in the OTHER pane's file.
An open block comment there bled into an unrelated document, and nothing
established whether the top of a scrolled viewport was inside a comment.

The single-pane path had seeded all along, which is why this was only ever
wrong in panes.

Usage: python3 test/integration_pane_syntax.py [path-to-fac-binary]
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
COMMENT_FG = "brightblack"

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
    """main.c on the left, a file with a mid-file block comment on the right."""

    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_ps_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_ps_work_")

        # No block comments at all: anything comment-coloured here is bleed.
        with open(os.path.join(self.work, "main.c"), "w") as f:
            f.write("#include <stdio.h>\n")
            f.writelines(f"int v{i} = {i};\n" for i in range(1, 40))

        # A long block comment partway down, so scrolling starts inside it.
        with open(os.path.join(self.work, "getopt.c"), "w") as f:
            f.write("#include <getopt.h>\n")
            f.writelines(f"int g{i} = {i};\n" for i in range(1, 20))
            f.write("/* a long block comment\n")
            f.writelines(f" * line {i} of the comment\n" for i in range(1, 15))
            f.write(" */\n")
            f.writelines(f"int h{i} = {i};\n" for i in range(1, 30))

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

    def send(self, d, w=0.5):
        self.child.send(d)
        self.drain(w)

    def open_second_pane(self):
        """alt-v on a tree row opens that file in a vertical split.

        Clicking the row instead would open it in the CURRENT pane and
        collapse the split, which is not the arrangement under test.
        """
        self.send("\x02", 1.2)
        sel = None
        for _ in range(10):
            for y in range(1, ROWS - 1):
                if any(self.screen.buffer[y][x].reverse for x in range(30)):
                    sel = "".join(self.screen.buffer[y][x].data
                                  for x in range(30)).strip()
                    break
            if sel and "getopt.c" in sel:
                break
            self.send("\x1b[B", 0.25)
        self.send("\x1bv", 1.8)
        return sel is not None and "getopt.c" in sel

    def left_colours(self):
        out = {}
        for y in range(3, 26):
            for x in range(6, 50):
                c = self.screen.buffer[y][x]
                if c.data.strip():
                    out[c.fg] = out.get(c.fg, 0) + 1
        return out

    def right_rows(self):
        out = []
        for y in range(2, ROWS - 2):
            txt = "".join(self.screen.buffer[y][x].data
                          for x in range(62, 118)).rstrip()
            if not txt.strip():
                continue
            fg = {self.screen.buffer[y][x].fg for x in range(62, 118)
                  if self.screen.buffer[y][x].data.strip()}
            out.append((txt.strip(), fg))
        return out

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def test_scrolling_one_pane_leaves_the_other_alone(binary):
    print("\nScrolling one pane does not recolour the other")
    s = Split(binary)
    try:
        if not s.open_second_pane():
            check(False, "opened a second pane on getopt.c")
            return
        before = s.left_colours()
        check(len(before) >= 3,
              "main.c starts with real highlighting, not one flat colour",
              str(before))
        check(before.get(COMMENT_FG, 0) == 0,
              "and nothing in it is comment-coloured, since it has no comments",
              str(before))

        for _ in range(14):
            s.send("\x1b[B", 0.12)      # scroll the getopt pane into its comment

        after = s.left_colours()
        check(after == before,
              "main.c is untouched after scrolling the other pane",
              f"{before} -> {after}")
    finally:
        s.close()


def test_block_comments_stay_comments_while_scrolling(binary):
    print("\nA block comment does not flicker as its pane scrolls")
    s = Split(binary)
    try:
        if not s.open_second_pane():
            check(False, "opened a second pane on getopt.c")
            return

        wrong = []
        saw_comment = False
        saw_code = False
        for step in range(26):
            s.send("\x1b[B", 0.10)
            for txt, fg in s.right_rows():
                is_comment = txt.startswith(("/*", "*", "*/")) or "of the comment" in txt
                coloured_as_comment = fg <= {COMMENT_FG}
                if is_comment:
                    saw_comment = True
                    if not coloured_as_comment:
                        wrong.append((step, "comment drawn as code", txt))
                elif txt.startswith("int "):
                    saw_code = True
                    if coloured_as_comment:
                        wrong.append((step, "code drawn as comment", txt))

        check(saw_comment, "the scroll passed through the block comment")
        check(saw_code, "and through code either side of it")
        check(not wrong, "every line was coloured for what it is",
              f"{len(wrong)} wrong, first: {wrong[0] if wrong else ''}")
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_scrolling_one_pane_leaves_the_other_alone,
               test_block_comments_stay_comments_while_scrolling):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_pane_syntax: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_pane_syntax: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
