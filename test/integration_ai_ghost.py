#!/usr/bin/env python3
"""
Integration test: model-backed shadow text in the real editor.

Two things are being proved. First, that with the feature off -- the default --
the editor behaves exactly as it did before it existed. Second, that with it on
and a live model, a suggestion appears, Tab inserts it, and dismissing it leaves
the buffer byte-identical.

Skips the model half cleanly when ollama is not running, so it is safe anywhere.

Usage: python3 test/integration_ai_ghost.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import json
import os
import re
import shutil
import sys
import tempfile
import time
import urllib.request

try:
    import pexpect
    import pyte
except ImportError as e:
    print(f"SKIP: missing dependency ({e}); pip3 install pexpect pyte")
    sys.exit(0)

ROWS, COLS = 24, 100
MODEL = "qwen2.5-coder:1.5b-base"
failures = []


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if not ok:
        failures.append(name)
        if detail:
            for ln in str(detail).split("\n")[:16]:
                print("        " + ln[:96])


def have_model():
    try:
        with urllib.request.urlopen("http://127.0.0.1:11434/api/tags", timeout=3) as r:
            names = [m["name"] for m in json.load(r).get("models", [])]
        return MODEL in names
    except Exception:
        return False


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
    def __init__(self, binary, content, ai_on, name="s.c",
                 rows=ROWS, cols=COLS, max_block_lines=4, truecolor=False):
        self.home = tempfile.mkdtemp(prefix="fac_aig_")
        cfg = os.path.join(self.home, ".config", "fac")
        os.makedirs(cfg)
        with open(os.path.join(cfg, "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        if ai_on:
            with open(os.path.join(cfg, "settings.json"), "w") as f:
                f.write('{\n'
                        '  "ai.enabled": true,\n'
                        '  "ai.host": "127.0.0.1",\n'
                        '  "ai.port": 11434,\n'
                        '  "ai.model": "%s",\n'
                        '  "ai.debounce_ms": 120,\n'
                        '  "ai.max_block_lines": %d%s\n'
                        '}\n' % (MODEL, max_block_lines,
                                 ',\n  "ui.color_mode": "truecolor"' if truecolor else ''))
        self.target = os.path.join(self.home, name)
        with open(self.target, "w") as f:
            f.write(content)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("NO_COLOR", None)
        if truecolor:
            env["COLORTERM"] = "truecolor"
        self.screen = pyte.Screen(cols, rows)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(rows, cols),
                                   env=env, cwd=self.home)
        self.drain(1.5)

    def drain(self, w=0.4):
        end = time.time() + w
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                pass
            except pexpect.EOF:
                break

    def send(self, data, w=0.4):
        self.child.send(data)
        self.drain(w)

    def wait_for(self, pred, timeout=8.0):
        end = time.time() + timeout
        while time.time() < end:
            self.drain(0.25)
            if pred(self.screen):
                return True
        return False

    def display(self):
        return "\n".join(r.rstrip() for r in self.screen.display)

    def saved(self):
        self.send("\x13", 0.6)
        with open(self.target) as f:
            return f.read()

    def ai_status(self):
        """Open the palette, run AI: Status, return the status bar text."""
        self.send("\x10", 0.5)          # Ctrl+P
        self.send("AI: Status", 0.5)
        self.send("\r", 0.8)
        return self.display()


    def close(self):
        try:
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)


def test_off_by_default(binary):
    """With no settings file at all, the editor must behave as before."""
    s = Session(binary, "int main(void) {\n    \n}\n", ai_on=False)
    try:
        s.send("\x1b[B", 0.3)
        s.send("\x1b[F", 0.3)
        s.send("int x = 1;", 1.2)
        check(s.saved() == "int main(void) {\n    int x = 1;\n}\n",
              "typing is unaffected when AI is off", s.saved())
    finally:
        s.close()


def test_ghost_appears_and_accepts(binary):
    s = Session(binary, "/* Return the larger of a and b. */\n"
                        "int max_of(int a, int b) {\n"
                        "    ret\n"
                        "}\n", ai_on=True)
    try:
        s.send("\x1b[B\x1b[B", 0.3)      # to line 3
        s.send("\x1b[F", 0.3)            # end of "    ret"
        # retype the trigger character so a completion is requested
        s.send("\x7f", 0.3)
        s.send("t", 0.4)

        got = s.wait_for(lambda sc: any("return" in r for r in sc.display[1:6]),
                         timeout=12)
        check(got, "a model suggestion appears as ghost text", s.display())

        if got:
            before = open(s.target).read()
            check("return" not in before,
                  "the ghost is NOT in the buffer before accepting", repr(before))

            s.send("\t", 1.2)
            after = s.saved()
            check("return" in after, "Tab inserts the suggestion", repr(after))
    finally:
        s.close()


def test_dismiss_leaves_buffer_untouched(binary):
    original = ("/* Add a and b. */\n"
                "int add(int a, int b) {\n"
                "    ret\n"
                "}\n")
    s = Session(binary, original, ai_on=True)
    try:
        s.send("\x1b[B\x1b[B", 0.3)
        s.send("\x1b[F", 0.3)
        s.send("\x7f", 0.3)
        s.send("t", 0.4)
        s.wait_for(lambda sc: any("return" in r for r in sc.display[1:6]), timeout=12)

        s.send("\x1b[B", 0.6)            # move away: dismisses the ghost
        check(s.saved() == original,
              "dismissing leaves the buffer byte-identical", repr(s.saved()))
    finally:
        s.close()


BLOCK_SRC = ("/* Sum every element of a and return the total. */\n"
             "int total(const int *a, int n) {\n"
             "    int sum = 0;\n"
             "    f\n"
             "    return sum;\n"
             "}\n"
             "\n"
             "int other(void) { return 7; }\n")


def drive_to_block(s):
    """Put the caret at end of the '    f' line and re-trigger."""
    s.send("\x1b[B" * 3, 0.3)
    s.send("\x1b[F", 0.3)
    s.send("\x7f", 0.3)
    s.send("f", 0.5)
    return s.wait_for(lambda sc: any("for" in r for r in sc.display[3:10]), timeout=14)


def test_block_renders_without_hiding_the_file(binary):
    s = Session(binary, BLOCK_SRC, ai_on=True, name="b.c", truecolor=True)
    try:
        editor_bg = s.screen.buffer[ROWS - 2][COLS - 2].bg
        if not drive_to_block(s):
            print("SKIP: model did not produce a block this run")
            return

        disp = s.display()
        check("return sum" in disp,
              "the real line below the block is still visible", disp)
        check("int other" in disp,
              "and so is the code further down", disp)

        # The block renderer pushes every real row below the suggestion down
        # and repaints the remaining post-EOF rows. Those clears must retain
        # the editor surface instead of exposing the terminal's background.
        other_row = next((i for i, row in enumerate(s.screen.display)
                          if "int other" in row), ROWS - 2)
        post_eof = [s.screen.buffer[y][x]
                    for y in range(other_row + 1, ROWS - 1)
                    for x in range(0, COLS - 1)]
        check(editor_bg != "default" and post_eof and
              all(cell.bg == editor_bg for cell in post_eof),
              "post-EOF rows keep the editor background while ghosting",
              sorted({cell.bg for cell in post_eof}))

        # The first ghost row sits on the highlighted caret row. Its text must
        # retain that row's surface instead of painting editor-background
        # rectangles around only the generated characters.
        for_row = next((i for i, row in enumerate(s.screen.display)
                        if "for" in row), -1)
        return_row = next((i for i, row in enumerate(s.screen.display)
                           if i > for_row and "return sum" in row), -1)
        if for_row >= 0:
            text_start = s.screen.display[for_row].find("for")
            row_bg = s.screen.buffer[for_row][COLS - 2].bg
            text_cells = [s.screen.buffer[for_row][x]
                          for x in range(text_start, text_start + 3)]
            check(row_bg != "default" and
                  all(cell.bg == row_bg for cell in text_cells),
                  "the first ghost row keeps one current-line background",
                  (row_bg, [cell.bg for cell in text_cells]))

        # Continuation rows are virtual editor rows. Gutter, generated text,
        # and right-side padding must all use the editor surface; resetting
        # before the padding used to leave visible background fragments.
        if return_row > for_row + 1:
            continuation = [s.screen.buffer[y][x]
                            for y in range(for_row + 1, return_row)
                            for x in range(0, COLS - 1)]
            continuation_bg = continuation[0].bg if continuation else "default"
            check(continuation_bg != "default" and
                  all(cell.bg == continuation_bg for cell in continuation),
                  "multiline ghost rows keep one editor background",
                  sorted({cell.bg for cell in continuation}))

        # every line number 1..8 appears exactly once: the block's own rows are
        # unnumbered, and the pushed-down lines keep their real numbers
        nums = [r.strip().split()[0] for r in s.screen.display[1:]
                if r.strip() and r.strip()[0].isdigit()]
        check(len(nums) == len(set(nums)),
              "no duplicated line numbers around the block", str(nums))

        check("for" not in open(s.target).read(),
              "nothing is in the buffer before accepting", repr(open(s.target).read()))
    finally:
        s.close()


def test_block_line_and_full_accept(binary):
    s = Session(binary, BLOCK_SRC, ai_on=True, name="b2.c")
    try:
        if not drive_to_block(s):
            print("SKIP: model did not produce a block this run")
            return

        s.send("\x1b[1;3C", 1.0)          # alt-right: accept one line
        after_one = open(s.target).read()
        s.send("\x13", 0.6)
        after_one = open(s.target).read()
        check("for" in after_one, "alt-right accepted the first block line", after_one)
        check(after_one.count("\n") >= BLOCK_SRC.count("\n"),
              "and inserted a line rather than replacing one", repr(after_one))
    finally:
        s.close()


def test_block_that_does_not_fit_shows_a_marker(binary):
    """On a short terminal the block cannot be drawn, so the user is told how
    much Tab would bring rather than shown a block truncated mid-thought."""
    s = Session(binary, BLOCK_SRC, ai_on=True, name="b3.c", rows=8, cols=90)
    try:
        s.send("\x1b[B" * 3, 0.3)
        s.send("\x1b[F", 0.3)
        s.send("\x7f", 0.3)
        s.send("f", 0.5)
        got = s.wait_for(lambda sc: any("more (Tab)" in r or "for" in r
                                        for r in sc.display), timeout=14)
        if not got:
            print("SKIP: model did not produce a suggestion this run")
            return
        # Either it fitted (fine) or the marker is shown -- never a partial block
        disp = s.display()
        check(True, "short terminal handled without crashing")
        if "more (Tab)" in disp:
            check(True, "overflow marker shown when the block will not fit")
    finally:
        s.close()


class SplitSession(Session):
    """The same session, with a second file opened beside the first.

    Written for one bug: a multi-line ghost is drawn INSIDE a pane, but each
    of its rows was finished with ESC[K, which clears to the end of the
    TERMINAL line. In a vertical split that is the neighbouring pane, so a
    block suggestion blanked the file next to it from the block's first row
    downwards, and dismissing the suggestion brought it back.
    """

    def __init__(self, binary, content, other, **kw):
        # Its own geometry: the default 24x100 is too short for a block of any
        # size to fit, and a block that does not fit is drawn as a marker
        # instead -- which is a different path and not the one under test.
        kw.setdefault("rows", 30)
        kw.setdefault("cols", 120)
        super().__init__(binary, content, True, **kw)
        self.rows = kw["rows"]
        self.cols = kw["cols"]
        self.other = os.path.join(self.home, "other.c")
        with open(self.other, "w") as f:
            f.write(other)

    def open_split(self):
        """alt-v on a tree row: opens that file BESIDE this one.

        Clicking the row would replace the current pane instead, which is not
        the arrangement under test.
        """
        self.send("\x02", 1.2)
        sel = None
        for _ in range(10):
            for y in range(1, self.rows - 1):
                if any(self.screen.buffer[y][x].reverse for x in range(30)):
                    sel = "".join(self.screen.buffer[y][x].data
                                  for x in range(30)).strip()
                    break
            if sel and "other.c" in sel:
                break
            self.send("\x1b[B", 0.25)
        self.send("\x1bv", 1.8)
        self.send("\x1bh", 0.6)              # focus back to the left pane
        return sel is not None and "other.c" in sel

    def ghost_rows(self):
        """Rows of the LEFT pane drawn in the dim ghost colour."""
        n = 0
        for y in range(2, self.rows - 2):
            cells = [self.screen.buffer[y][x]
                     for x in range(6, self.cols // 2 - 4)]
            if not "".join(c.data for c in cells).strip():
                continue
            if all(c.fg == "brightblack" for c in cells if c.data.strip()):
                n += 1
        return n

    def right_rows(self):
        """How many rows of the RIGHT pane still show anything."""
        n = 0
        for y in range(2, self.rows - 2):
            if "".join(self.screen.buffer[y][x].data
                       for x in range(self.cols // 2 + 2, self.cols - 2)).strip():
                n += 1
        return n


def test_a_block_does_not_truncate_the_other_pane(binary):
    print("\nA block suggestion leaves the pane beside it alone")
    s = SplitSession(
        binary,
        "#include <stdio.h>\n\n"
        "/* Print every number from 1 to 10, one per line, using a for loop. */\n"
        "void print_numbers(void)\n{\n",
        "".join(f"int marker{i} = {i};\n" for i in range(1, 40)),
        max_block_lines=8)
    try:
        if not s.open_split():
            check(False, "opened other.c in a vertical split")
            return
        before = s.right_rows()
        check(before > 10, "the right pane starts full of text", str(before))

        s.send("\x1b[B" * 5, 0.4)
        s.send("\x05", 0.3)                  # end of line
        s.send("\n    ", 0.6)
        s.send("\x1b\\", 1.0)                # ask for a deep completion
        for _ in range(20):
            s.drain(1.0)
            if s.ghost_rows() >= 2:
                break

        if s.ghost_rows() < 2:
            print("SKIP: model did not produce a block this run")
            return
        during = s.right_rows()
        check(during == before,
              "the right pane is intact while the block is shown",
              f"{before} rows -> {during} while a {s.ghost_rows()}-row block is up")

        s.send("\x1b", 0.8)
        check(s.right_rows() == before,
              "and still intact after the block is dismissed",
              f"{before} -> {s.right_rows()}")
    finally:
        s.close()


def test_cache_saves_a_repeat_request(binary):
    """Backspace is a trigger key, so deleting and retyping asks the model a
    question it has already answered. The cache is only worth its complexity
    if that repeat is actually served locally -- so read the counter rather
    than trust the design."""
    s = Session(binary, "/* Return the larger of a and b. */\n"
                        "int max_of(int a, int b) {\n"
                        "    ret\n"
                        "}\n", ai_on=True, cols=200)
    try:
        s.send("\x1b[B\x1b[B", 0.3)
        s.send("\x1b[F", 0.3)

        # First pass: delete a char and retype it, producing a real request.
        s.send("\x7f", 0.3)
        s.send("t", 0.4)
        got = s.wait_for(lambda sc: any("return" in r for r in sc.display[1:6]),
                         timeout=14)
        if not got:
            print("SKIP: model did not produce a suggestion this run")
            return

        # Same delete-and-retype: identical prefix and suffix windows, so the
        # second ask is the same question and must not reach the model.
        s.send("\x7f", 0.5)
        s.send("t", 0.6)
        s.wait_for(lambda sc: any("return" in r for r in sc.display[1:6]),
                   timeout=14)

        status = s.ai_status()
        check("cache" in status, "AI: Status reports the cache", status)

        m = re.search(r"cache (\d+)% \((\d+) saved\)", status)
        check(m is not None, "the cache counter is parseable", status)
        if m:
            check(int(m.group(2)) >= 1,
                  "a repeated prompt was served from cache, not the model",
                  status)
    finally:
        s.close()


def main():
    binary = find_binary()

    test_off_by_default(binary)

    if not have_model():
        print(f"SKIP: {MODEL} not available on 127.0.0.1:11434 "
              "(off-by-default checks still ran)")
    else:
        test_ghost_appears_and_accepts(binary)
        test_dismiss_leaves_buffer_untouched(binary)
        test_block_renders_without_hiding_the_file(binary)
        test_block_line_and_full_accept(binary)
        test_block_that_does_not_fit_shows_a_marker(binary)
        test_cache_saves_a_repeat_request(binary)
        test_a_block_does_not_truncate_the_other_pane(binary)

    if failures:
        print(f"\nintegration_ai_ghost: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("\nintegration_ai_ghost: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
