#!/usr/bin/env python3
"""
Integration test: what the Tab key writes to disk.

Two properties, checked against real files rather than the buffer:

  1. A makefile recipe line begins with a literal TAB. Not a style question --
     make rejects a space-indented recipe with "missing separator", so getting
     this wrong writes a file that cannot be built. Asserted by running make.

  2. Tab advances to a tab stop, so a trailing comment typed after two lines of
     similar length lands in the same column on both. Adding a fixed four
     spaces instead put them one apart, which is the bug this covers.

Usage: python3 test/integration_indent.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import os
import shutil
import subprocess
import sys
import tempfile
import time

try:
    import pexpect
    import pyte
except ImportError as e:
    print(f"SKIP: missing dependency ({e}); pip3 install pexpect pyte")
    sys.exit(0)

ROWS, COLS = 24, 100
failures = []


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if not ok:
        failures.append(name)
        if detail:
            for ln in str(detail).split("\n")[:12]:
                print("        " + ln[:96])


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
    def __init__(self, binary, name, content):
        self.home = tempfile.mkdtemp(prefix="fac_indent_")
        cfg = os.path.join(self.home, ".config", "fac")
        os.makedirs(cfg)
        with open(os.path.join(cfg, "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.target = os.path.join(self.home, name)
        with open(self.target, "w") as f:
            f.write(content)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.home)
        self.drain(1.5)

    def drain(self, w=0.5):
        end = time.time() + w
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                pass
            except pexpect.EOF:
                break

    def send(self, data, w=0.5):
        self.child.send(data)
        self.drain(w)

    def close(self):
        try:
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)


def test_makefile_recipe_is_tab_indented(binary):
    s = Session(binary, "Makefile", "all:\n")
    try:
        s.send("\x1b[B", 0.3)          # line 2
        s.send("\t", 0.4)
        s.send("@echo BUILT_OK", 0.6)
        s.send("\x13", 0.9)            # Ctrl-S
        raw = open(s.target, "rb").read()
        check(b"\n\t@echo" in raw,
              "a makefile recipe line starts with a literal tab", repr(raw))

        # The property that actually matters: it builds.
        r = subprocess.run(["make", "-C", s.home, "all"],
                           capture_output=True, text=True)
        check("BUILT_OK" in r.stdout,
              "and make builds it rather than reporting a missing separator",
              r.stdout + r.stderr)
    finally:
        s.close()


def test_makefile_shift_tab_removes_the_tab(binary):
    s = Session(binary, "Makefile", "all:\n\t@echo hi\n")
    try:
        s.send("\x1b[B", 0.3)
        s.send("\x1b[Z", 0.5)          # shift-tab
        s.send("\x13", 0.9)
        raw = open(s.target, "rb").read()
        check(b"\n@echo hi" in raw,
              "shift-tab removes a makefile's tab rather than ignoring it",
              repr(raw))
    finally:
        s.close()


def test_tab_lands_on_a_stop(binary):
    """Two lines whose lengths differ by one. A fixed four-space indent put
    their comments one column apart; tab stops put both on column 8."""
    s = Session(binary, "a.c", "int a;\nint ab;\n")
    try:
        s.send("\x1b[F", 0.3)          # end of line 1 (col 6)
        s.send("\t", 0.4)
        s.send("// one", 0.5)
        s.send("\x1b[B\x1b[F", 0.3)    # end of line 2 (col 7)
        s.send("\t", 0.4)
        s.send("// two", 0.6)
        s.send("\x13", 0.9)
        lines = open(s.target).read().split("\n")
        c1, c2 = lines[0].index("//"), lines[1].index("//")
        check(c1 == c2, "adjacent comments line up", f"{lines[0]!r} {lines[1]!r}")
        check(c1 % 4 == 0, f"and land on a tab stop (column {c1})")
        check("\t" not in lines[0], "a C file is indented with spaces, not tabs")
    finally:
        s.close()


def test_tab_from_column_zero_is_a_full_indent(binary):
    s = Session(binary, "b.c", "x\n")
    try:
        s.send("\x1b[B", 0.3)          # empty line 2
        s.send("\t", 0.4)
        s.send("y", 0.5)
        s.send("\x13", 0.9)
        lines = open(s.target).read().split("\n")
        check(lines[1] == "    y", "tab at column 0 inserts a full indent",
              repr(lines[1]))
    finally:
        s.close()


C_BLOCK = """int main(void) {
    if (ready) {
        while (running) {
            step();
        }
    }
    return 0;
}
"""


def saved(s):
    s.send("\x13", 0.9)
    with open(s.target, "rb") as f:
        return f.read()


def line_of(raw, needle):
    """The full line containing `needle`, as bytes."""
    for ln in raw.split(b"\n"):
        if needle in ln:
            return ln
    return b""


# The request: on a blank line, one Tab goes where the line belongs rather
# than one level nearer it.
def test_tab_on_a_blank_line_jumps_to_the_block_indent(binary):
    # A blank line inside the while block, whose body sits at 12.
    body = C_BLOCK.replace("            step();\n", "            step();\n\n")
    s = Session(binary, "t.c", body)
    try:
        for _ in range(4):             # down to the blank line
            s.send("\x1b[B", 0.12)
        s.send("\t", 0.5)
        s.send("here();", 0.5)
        raw = saved(s)
        check(b"            here();" in raw,
              "one Tab reaches the block's own indent (12 columns)",
              repr(line_of(raw, b"here();")))
        check(b"    here();" in raw and b"        here" in raw,
              "and it is not merely one level", repr(line_of(raw, b"here();")))
    finally:
        s.close()


def test_further_tabs_still_go_deeper(binary):
    """Smart Tab must not become a wall at the expected indent."""
    body = C_BLOCK.replace("            step();\n", "            step();\n\n")
    s = Session(binary, "t.c", body)
    try:
        for _ in range(4):
            s.send("\x1b[B", 0.12)
        s.send("\t", 0.4)
        s.send("\t", 0.4)              # one level past the block
        s.send("deep();", 0.5)
        raw = saved(s)
        check(b"                deep();" in raw,
              "a second Tab steps one level beyond the expected indent",
              repr(line_of(raw, b"deep();")))
    finally:
        s.close()


def test_tab_never_pulls_a_line_leftwards(binary):
    """Tab is not a dedent key -- Shift-Tab is.

    A blank line already indented DEEPER than its context must not jump back.
    """
    body = C_BLOCK.replace("    return 0;\n", "                \n    return 0;\n")
    s = Session(binary, "t.c", body)
    try:
        for _ in range(6):
            s.send("\x1b[B", 0.12)
        s.send("\x05", 0.3)            # ctrl-e: end of the whitespace
        s.send("\t", 0.5)
        s.send("x();", 0.5)
        raw = saved(s)
        ln = line_of(raw, b"x();")
        indent = len(ln) - len(ln.lstrip(b" "))
        check(indent >= 16, "Tab moved right, not left", f"{indent} columns: {ln!r}")
    finally:
        s.close()


def test_a_blank_line_outside_any_block_is_unchanged(binary):
    """With nothing above it to inherit from, Tab is just a tab."""
    s = Session(binary, "t.c", "\nint x;\n")
    try:
        s.send("\t", 0.5)
        s.send("y();", 0.5)
        raw = saved(s)
        check(raw.startswith(b"    y();"),
              "at the top of a file Tab inserts one level", repr(raw[:30]))
    finally:
        s.close()


# Backspace: the indent is the unit, the way VSCode's useTabStops has it.
def test_backspace_unwinds_a_whole_indent(binary):
    s = Session(binary, "t.c", C_BLOCK)
    try:
        for _ in range(3):             # to '            step();'
            s.send("\x1b[B", 0.12)
        # End, then back over the 7 characters of 'step();', which parks the
        # caret at column 13 -- just past the indent. Deliberately not Home:
        # that is SMART home, so it lands on the first non-blank character and
        # arrowing right from there walks into the text instead of the indent.
        s.send("\x1b[F", 0.3)
        for _ in range(len("step();")):
            s.send("\x1b[D", 0.07)
        s.send("\x7f", 0.5)
        raw = saved(s)
        ln = line_of(raw, b"step();")
        indent = len(ln) - len(ln.lstrip(b" "))
        check(indent == 8, "one Backspace unwinds a whole level (12 -> 8)",
              f"{indent} columns: {ln!r}")
    finally:
        s.close()


def test_backspace_in_text_still_takes_one_character(binary):
    """The tab-stop rule must not reach into the text itself."""
    s = Session(binary, "t.c", "int abcdef;\n")
    try:
        s.send("\x05", 0.3)            # end of line
        s.send("\x7f", 0.5)
        raw = saved(s)
        check(raw.startswith(b"int abcdef\n"),
              "backspace in code deletes exactly one character", repr(raw[:20]))
    finally:
        s.close()


# The Makefile bug this round: auto-indent wrote spaces, which make rejects.
def test_makefile_auto_indent_uses_a_tab(binary):
    s = Session(binary, "Makefile", "all:\n\t@echo FIRST\n")
    try:
        s.send("\x1b[B", 0.3)
        s.send("\x05", 0.3)            # end of the recipe line
        s.send("\r", 0.5)              # Enter: auto-indent
        s.send("@echo BUILT_OK", 0.6)
        raw = saved(s)
        check(b"\n\t@echo BUILT_OK" in raw,
              "Enter's auto-indent writes a tab in a makefile", repr(raw))
        check(b"\n    @echo" not in raw,
              "and not the spaces that make rejects", repr(raw))

        r = subprocess.run(["make", "-C", s.home, "all"],
                           capture_output=True, text=True)
        check("BUILT_OK" in r.stdout,
              "so the file it wrote actually builds", r.stdout + r.stderr)
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_makefile_recipe_is_tab_indented,
               test_tab_on_a_blank_line_jumps_to_the_block_indent,
               test_further_tabs_still_go_deeper,
               test_tab_never_pulls_a_line_leftwards,
               test_a_blank_line_outside_any_block_is_unchanged,
               test_backspace_unwinds_a_whole_indent,
               test_backspace_in_text_still_takes_one_character,
               test_makefile_auto_indent_uses_a_tab,
               test_makefile_shift_tab_removes_the_tab,
               test_tab_lands_on_a_stop,
               test_tab_from_column_zero_is_a_full_indent):
        try:
            fn(binary)
        except Exception as exc:            # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    if failures:
        print(f"\nintegration_indent: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("\nintegration_indent: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
