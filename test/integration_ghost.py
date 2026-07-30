#!/usr/bin/env python3
"""
Integration test: mid-line ghost text (dynamic line) and #include headers.

The ghost suggestion used to render only with the cursor at end of line.
Now the line visually opens up mid-line: the dim suffix is drawn at the
cursor and the real tail of the line is redrawn after it, shifted right.
Dismissing the ghost shrinks the line back; Tab accepts.

Phase 1 (always runs): word-scan source, no LSP needed. Typing 'INT_'
between '(' and ')' must show the merged line on screen while the file
on disk still has only the typed text; Tab must insert the suffix.

Phase 2 (needs clangd on PATH, skipped otherwise): '#include <floa'
must ghost the header completion 'float.h>' and Tab must accept it.

Usage: python3 test/integration_ghost.py [path-to-fac-binary]
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
    def __init__(self, binary, target, home):
        env = {**os.environ, "TERM": "xterm-256color", "HOME": home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [target], dimensions=(ROWS, COLS),
                                   env=env, cwd=os.path.dirname(target))

    def drain(self, wait=0.3):
        end = time.time() + wait
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def row_with(self, text):
        for r in self.screen.display:
            if text in r:
                return r.rstrip()
        return None

    def wait_for(self, text, timeout):
        end = time.time() + timeout
        while time.time() < end:
            self.drain(0.4)
            r = self.row_with(text)
            if r is not None:
                return r
        return None

    def close(self):
        self.child.close(force=True)


def make_home():
    home = tempfile.mkdtemp(prefix="fac_ghost_home_")
    os.makedirs(os.path.join(home, ".config", "fac"))
    with open(os.path.join(home, ".config", "fac", "state.json"), "w") as f:
        f.write('{\n  "first_run_completed": true,\n'
                '  "lsp_installer_seen": true,\n  "version": "1.0"\n}\n')
    return home


failures = []


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}" + (f"  [{detail}]" if detail and not ok else ""))
    if not ok:
        failures.append(name)


def phase_wordscan(binary):
    home = make_home()
    target = os.path.join(home, "w.txt")   # .txt: no LSP server involved
    with open(target, "w") as f:
        f.write("int INT_MAXIMUM_TEST;\nx = ();\n")

    s = Session(binary, target, home)
    s.drain(1.5)

    # Cursor to line 2, between '(' and ')'
    s.child.send("\x1b[B")          # down
    s.drain(0.2)
    for _ in range(5):
        s.child.send("\x1b[C")      # right, onto the ')'
        s.drain(0.1)

    for ch in "INT_":
        s.child.send(ch)
        s.drain(0.15)
    s.drain(0.5)

    merged = s.row_with("x = (INT_MAXIMUM_TEST);")
    check(merged is not None, "mid-line ghost opens the line up",
          str(s.row_with("x = (")))

    # Any non-accept key dismisses; the line shrinks back
    s.child.send("\x1b[B")          # down
    s.drain(0.5)
    check(s.row_with("x = (INT_MAXIMUM_TEST);") is None and
          s.row_with("x = (INT_);") is not None,
          "dismissed ghost shrinks the line back", str(s.row_with("x = (")))

    # The ghost is display-only: saving now must write only the typed text
    s.child.send("\x13")            # ctrl-s
    s.drain(0.8)
    with open(target) as f:
        check("x = (INT_);" in f.read(), "ghost never contaminates the buffer")

    # Bring the ghost back with backspace (also a trigger key), then accept.
    # Cursor must first be back between '_' and ')'.
    s.child.send("\x1b[A")          # up to line 2 (desired column restores)
    s.drain(0.2)
    s.child.send("\x7f")            # backspace: deletes '_', prefix 'INT'
    s.drain(0.6)
    check(s.row_with("x = (INT_MAXIMUM_TEST);") is not None,
          "backspace re-triggers mid-line ghost", str(s.row_with("x = (")))

    s.child.send("\t")              # accept
    s.drain(0.5)
    s.child.send("\x13")            # ctrl-s save
    s.drain(0.8)
    with open(target) as f:
        content = f.read()
    check("x = (INT_MAXIMUM_TEST);" in content,
          "tab accepts suggestion into the buffer", content.splitlines()[1] if len(content.splitlines()) > 1 else content)

    s.close()
    shutil.rmtree(home, ignore_errors=True)


def phase_include(binary):
    if shutil.which("clangd") is None:
        print("skip clangd header phase (clangd not on PATH)")
        return
    home = make_home()
    target = os.path.join(home, "h.c")
    open(target, "w").close()

    s = Session(binary, target, home)
    s.drain(2.0)

    for ch in "#include <floa":
        s.child.send(ch)
        s.drain(0.15)

    # LSP response is async; poll for the ghosted header
    row = s.wait_for("#include <float.h>", timeout=12.0)
    check(row is not None, "header completion ghosted after <floa",
          str(s.row_with("#include")))

    if row is not None:
        s.child.send("\t")          # accept
        s.drain(0.5)
        s.child.send("\x13")        # ctrl-s
        s.drain(0.8)
        with open(target) as f:
            content = f.read()
        check(content.startswith("#include <float.h>"),
              "tab accepts header into the buffer",
              content.splitlines()[0] if content else "<empty>")

        # The report that started all this: an LSP suggestion between an
        # auto-closed '(' and ')' - 'int y = (INT_|)' must ghost INT_MAX
        s.child.send("\r")
        s.drain(0.2)
        for ch in "#include <limits.h>":
            s.child.send(ch)
            s.drain(0.1)
        s.child.send("\r")
        s.drain(0.3)
        for ch in "int y = (":        # '(' auto-closes; cursor lands inside
            s.child.send(ch)
            s.drain(0.1)
        for ch in "INT_":
            s.child.send(ch)
            s.drain(0.15)
        row = s.wait_for("int y = (INT_MAX)", timeout=12.0)
        check(row is not None, "LSP ghost between parens (INT_MAX)",
              str(s.row_with("int y")))
        if row is not None:
            s.child.send("\t")
            s.drain(0.5)
            s.child.send("\x13")
            s.drain(0.8)
            with open(target) as f:
                content = f.read()
            check("int y = (INT_MAX)" in content,
                  "tab accepts LSP suggestion mid-parens",
                  next((l for l in content.splitlines() if "int y" in l), "<missing>"))

    s.close()
    shutil.rmtree(home, ignore_errors=True)


def phase_include_quoted(binary):
    """A QUOTED include, which is where the closing quote doubles up.

    The phase above uses <float.h>, and '<' is not in the auto-close set, so
    it never met this: '"' auto-closes, the header suggestion carries its own
    closing quote, and accepting one on top of the other left
    #include "showme.h"" with a character to delete by hand.
    """
    if shutil.which("clangd") is None:
        print("skip quoted-header phase (clangd not on PATH)")
        return
    home = make_home()
    proj = os.path.join(home, "proj")
    os.makedirs(proj)
    with open(os.path.join(proj, "showme.h"), "w") as f:
        f.write("#ifndef SHOWME_H\n#define SHOWME_H\n#endif\n")
    with open(os.path.join(proj, "compile_flags.txt"), "w") as f:
        f.write("-I.\n-std=c11\n")
    target = os.path.join(proj, "main.c")
    open(target, "w").close()

    s = Session(binary, target, home)
    s.drain(2.0)

    for ch in '#include "showme':
        s.child.send(ch)
        s.drain(0.15)

    row = s.wait_for('#include "showme.h"', timeout=12.0)
    check(row is not None, "quoted header completion is ghosted",
          str(s.row_with("#include")))

    if row is not None:
        s.child.send("\t")
        s.drain(0.6)
        s.child.send("\x13")
        s.drain(1.0)
        with open(target) as f:
            first = f.read().split("\n")[0]
        check(first == '#include "showme.h"',
              "accepting it leaves exactly one closing quote", repr(first))
        check('""' not in first, "and no doubled quote anywhere", repr(first))

    s.close()
    shutil.rmtree(home, ignore_errors=True)


def main():
    binary = find_binary()
    phase_wordscan(binary)
    phase_include(binary)
    phase_include_quoted(binary)

    if failures:
        print(f"integration_ghost: FAILED ({len(failures)}: {', '.join(failures)})")
        sys.exit(1)
    print("integration_ghost: ALL PASSED")


if __name__ == "__main__":
    main()
