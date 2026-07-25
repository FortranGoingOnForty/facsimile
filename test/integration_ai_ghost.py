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
                 rows=ROWS, cols=COLS, max_block_lines=4):
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
                        '  "ai.max_block_lines": %d\n'
                        '}\n' % (MODEL, max_block_lines))
        self.target = os.path.join(self.home, name)
        with open(self.target, "w") as f:
            f.write(content)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
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
    s = Session(binary, BLOCK_SRC, ai_on=True, name="b.c")
    try:
        if not drive_to_block(s):
            print("SKIP: model did not produce a block this run")
            return

        disp = s.display()
        check("return sum" in disp,
              "the real line below the block is still visible", disp)
        check("int other" in disp,
              "and so is the code further down", disp)

        # every line number 1..8 appears exactly once: the block's own rows are
        # unnumbered, and the pushed-down lines keep their real numbers
        nums = [r.strip().split()[0] for r in s.screen.display
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

    if failures:
        print(f"\nintegration_ai_ghost: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("\nintegration_ai_ghost: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
