#!/usr/bin/env python3
"""
Integration test: LSP rename does not corrupt the file.

The reported bug, reproduced before it was fixed: renaming shortly after typing
asked the language server about a document it had already been told about but
which had since changed -- document sync is debounced half a second. The server
answered with ranges for the text it last saw, and those ranges were applied to
the text as it is NOW. The result was not a failed rename but destroyed code:
seven characters of `char li` replaced by the new name, and the new name
inserted on an unrelated blank line.

Two defences, and both are tested:

  1. The rename request force-flushes the document first, so the server is
     describing the same text the edits will land on.
  2. Every edit is checked against what the buffer actually holds before
     anything is written, and the whole set is refused if any of them
     disagrees. An edit set is one operation; half-applying it leaves the file
     in a state the user cannot recognise.

Assertions are against the WHOLE FILE, not just the renamed token. The original
bug left the renamed occurrences looking fine while damaging a line elsewhere.

Usage: python3 test/integration_rename.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte, and clangd on PATH
"""

import os
import re
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

if shutil.which("clangd") is None:
    print("SKIP: clangd not on PATH; rename needs a language server")
    sys.exit(0)

ROWS, COLS = 40, 120
F2 = "\x1b[12~"
failures = []

SRC = """#include <stdio.h>

#include "utils.h"

int main()
{
    char line[MAXLEN];
    printf("%s\\n", line);
    \x20
    char line5[MAXLEN];
    printf("first %s and again %s\\n", line5, line5);
    \x20
    printf("%s\\n", line5);
    \x20
    return 0;
}
"""

HDR = "#ifndef UTILS_H\n#define UTILS_H\n#define MAXLEN 1024\n#endif\n"


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if not ok:
        failures.append(name)
        if detail:
            for ln in str(detail).split("\n")[:16]:
                print("        " + ln[:100])


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
    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_ren_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.ws = os.path.join(self.home, "proj")
        os.makedirs(self.ws)
        self.target = os.path.join(self.ws, "main.c")
        with open(self.target, "w") as f:
            f.write(SRC)
        with open(os.path.join(self.ws, "utils.h"), "w") as f:
            f.write(HDR)
        with open(os.path.join(self.ws, "compile_flags.txt"), "w") as f:
            f.write("-I.\n-std=c11\n")
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.ws)
        self.drain(2.0)
        self.drain(7.0)                 # clangd needs to index before rename works

    def drain(self, w=0.6):
        end = time.time() + w
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                pass
            except pexpect.EOF:
                break

    def send(self, d, w=0.5):
        self.child.send(d)
        self.drain(w)

    def status(self):
        return self.screen.display[-1].rstrip()

    def goto(self, line, col):
        self.send("\x1b[1;5H", 0.5)     # ctrl-home
        for _ in range(line - 1):
            self.send("\x1b[B", 0.05)
        for _ in range(col - 1):
            self.send("\x1b[C", 0.03)
        self.drain(0.4)

    def rename_to(self, new):
        self.send(F2, 0.8)
        if "ename" not in self.status():
            self.send("\x1bOQ", 0.8)    # F2 in the other encoding
        if "ename" not in self.status():
            return False
        self.send(new, 0.4)
        self.send("\r", 3.5)
        return True

    def saved(self):
        self.send("\x13", 1.2)
        with open(self.target) as f:
            return f.read()

    def close(self):
        try:
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)


def test_a_plain_rename_changes_only_the_name(binary):
    s = Session(binary)
    try:
        s.goto(10, 10)                  # 'line5' in the declaration
        if not s.rename_to("total"):
            check(False, "rename prompt did not open", s.status())
            return
        after = s.saved()

        check("line5" not in after, "every occurrence was renamed", after)
        check(after.count("total") == 4,
              "all four occurrences, declaration and three uses",
              f"count={after.count('total')}\n{after}")
        # The whole file, not just the token: the original bug damaged a line
        # elsewhere while the renamed occurrences looked correct.
        check(after == SRC.replace("line5", "total"),
              "and nothing else in the file changed at all", after)
    finally:
        s.close()


# THE regression. Type something that shifts line numbers, then rename before
# the debounced sync can tell the server about it.
def test_renaming_right_after_an_edit_is_safe(binary):
    s = Session(binary)
    try:
        # Add a line high up, shifting everything below by one.
        s.goto(7, 1)
        s.send("\x1b[F", 0.3)           # end of line
        s.child.send("\r")              # no drain: stay inside the debounce
        s.drain(0.15)

        s.goto(11, 10)                  # the declaration, now one line lower
        if not s.rename_to("total"):
            check(False, "rename prompt did not open", s.status())
            return
        after = s.saved()

        check("char total[MAXLEN];" in after,
              "the declaration survived intact rather than being mangled", after)
        check("line5" not in after, "and every occurrence was renamed", after)
        check(after.count("total") == 4, "all four of them",
              f"count={after.count('total')}\n{after}")
        check("    total\n" not in after,
              "the new name was not dropped onto a line of its own", after)

        # Compared with the blank lines removed from both sides. The Enter that
        # set this up adds a whitespace-only line -- auto-indented, so its exact
        # width is a separate behaviour and not what this test is about. What
        # matters is that NOTHING carrying code differs beyond the rename.
        def code_lines(t):
            return [ln for ln in t.split("\n") if ln.strip()]

        check(code_lines(after) == code_lines(SRC.replace("line5", "total")),
              "and no line carrying code differs beyond the rename",
              "GOT:\n" + "\n".join(code_lines(after))
              + "\nWANT:\n" + "\n".join(code_lines(SRC.replace("line5", "total"))))
    finally:
        s.close()


def test_a_refused_rename_changes_nothing(binary):
    """Whatever the reason, a rename that cannot be applied cleanly must leave
    the file byte-identical rather than half-done."""
    s = Session(binary)
    try:
        before = open(s.target).read()
        s.goto(10, 5)                   # on 'char', not an identifier to rename
        s.send(F2, 0.8)
        if "ename" in s.status():
            s.send("total", 0.4)
            s.send("\r", 2.5)
        after = s.saved()
        check(after == before,
              "renaming a non-symbol leaves the file untouched",
              "GOT:\n" + after)
    finally:
        s.close()


def test_the_renamed_file_still_compiles(binary):
    """The property that actually matters."""
    s = Session(binary)
    try:
        s.goto(10, 10)
        if not s.rename_to("total"):
            check(False, "rename prompt did not open", s.status())
            return
        s.saved()
        r = subprocess.run(["gcc", "-I", s.ws, "-std=c11", "-fsyntax-only",
                            s.target], capture_output=True, text=True)
        check(r.returncode == 0,
              "the file compiles after the rename", r.stdout + r.stderr)
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_a_plain_rename_changes_only_the_name,
               test_renaming_right_after_an_edit_is_safe,
               test_a_refused_rename_changes_nothing,
               test_the_renamed_file_still_compiles):
        try:
            fn(binary)
        except Exception as exc:                        # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    if failures:
        print(f"\nintegration_rename: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("\nintegration_rename: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
