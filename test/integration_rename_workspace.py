#!/usr/bin/env python3
"""
Integration test: renaming a symbol reaches every file that uses it.

It used to reach exactly the files that happened to be open. The code walked
the whole WorkspaceEdit, but each file it could not find a tab for hit

    if (tab_idx == 0) then
        ! File not open - skip for now
        return

and was dropped without a word. Rename a function from its header and the
prototype changed while every caller went on calling a name that no longer
existed -- a rename that compiles to a broken tree, reported as success.

Members of a tab group made it worse: they are deferred until looked at and
hold no text, so even an OPEN one was skipped two checks later.

So: the files are opened (not switched to) and left modified, which is the
same shape as an edit made by hand -- undo and save stay the user's.

Needs clangd; skips cleanly without it.

Usage: python3 test/integration_rename_workspace.py [path-to-fac-binary]
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

ROWS, COLS = 32, 150
CTRL_HOME = "\x1b[1;5H"
ALT_N = "\x1bn"

failures = []

HEADER = "#ifndef U_H\n#define U_H\nchar *strdup2(const char *s);\n#endif\n"
IMPL = ('#include "u.h"\n'
        '#include <stdlib.h>\n'
        '#include <string.h>\n'
        'char *strdup2(const char *s)\n'
        '{\n'
        '    char *p = malloc(strlen(s) + 1);\n'
        '    if (p) strcpy(p, s);\n'
        '    return p;\n'
        '}\n')
CALLER = ('#include "u.h"\n'
          '#include <stdio.h>\n'
          'int main(void)\n'
          '{\n'
          '    char *a = strdup2("x");\n'
          '    char *b = strdup2("y");\n'
          '    printf("%s%s", a, b);\n'
          '    return 0;\n'
          '}\n')


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if not ok:
        failures.append(name)
        if detail:
            for ln in str(detail).split("\n")[:10]:
                print("        " + ln[:118])


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
        self.home = tempfile.mkdtemp(prefix="fac_rw_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        # macOS exposes the temporary directory as /var/... while realpath
        # resolves it to /private/var/.... Facsimile resolves opened files,
        # and Apple clangd keys its background index by the compile database's
        # spelling; mixing the aliases makes one symbol look like two projects
        # and the rename response contains only the active file.
        self.work = os.path.realpath(tempfile.mkdtemp(prefix="fac_rw_work_"))
        self.paths = {}
        for name, body in (("u.h", HEADER), ("u.c", IMPL), ("main.c", CALLER)):
            p = os.path.join(self.work, name)
            with open(p, "w") as f:
                f.write(body)
            self.paths[name] = p
        with open(os.path.join(self.work, "compile_commands.json"), "w") as f:
            f.write("[" + ",".join(
                '{"directory":"%s","command":"cc -c %s","file":"%s"}'
                % (self.work, n, self.paths[n]) for n in ("u.c", "main.c")) + "]")
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        # Open ONLY u.c. main.c is never opened, which is the whole point.
        self.child = pexpect.spawn(binary, [self.paths["u.c"]],
                                   dimensions=(ROWS, COLS), env=env, cwd=self.work)
        self.drain(7.0)

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

    def send(self, d, w=0.6):
        self.child.send(d)
        self.drain(w)

    def text(self):
        return "\n".join(r.rstrip() for r in self.screen.display)

    def status(self):
        return self.screen.display[-1].strip()

    def close(self):
        try:
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def test_rename_reaches_a_file_that_was_never_open(binary):
    print("\nRenaming the definition renames callers in unopened files")
    s = Session(binary)
    try:
        # Caret onto 'strdup2' in the definition on line 4.
        s.send(CTRL_HOME, 0.6)
        for _ in range(3):
            s.send("\x1b[B", 0.25)
        for _ in range(7):                     # past "char *"
            s.send("\x1b[C", 0.15)

        s.send(ALT_N, 1.5)
        if "ename" not in s.text():
            print(f"SKIP: rename prompt did not open (status {s.status()[:60]!r})")
            return
        check(True, "the rename prompt opened")

        for ch in "strdup3":
            s.send(ch, 0.12)
        s.send("\r", 4.0)
        s.drain(4.0)

        msg = s.status()
        check("Renamed" in msg or "renamed" in msg,
              "the rename reported success", msg[:110])
        print("        MESSAGE:", msg[:120])
        print("        TABBAR :", s.screen.display[0].rstrip()[:120])
        print("        SCREEN :", " / ".join(
            r.rstrip()[:40] for r in s.screen.display[1:6]))

        # Every touched file must be marked dirty, or save-all walks past it
        # and the rename is only in memory.
        bar = s.screen.display[0]
        check(bar.count("*") + bar.count("●") >= 3,
              "every file the rename touched is marked modified", bar[:120])

        # Ctrl-Shift-S only exists as a kitty CSI-u report: 115 is 's', and
        # the modifier is 1 + shift(1) + ctrl(4).
        s.send("\x13", 1.5)                   # ctrl-s: the active tab
        s.send("\x1b[115;6u", 1.5)            # save all: everything else
        s.drain(2.5)

        impl = open(s.paths["u.c"]).read()
        caller = open(s.paths["main.c"]).read()

        check("strdup3" in impl, "the definition was renamed on disk",
              impl[:160])
        check("strdup2" not in caller and caller.count("strdup3") == 2,
              "and BOTH calls in the never-opened file were renamed too",
              caller)
    finally:
        s.close()


def main():
    binary = find_binary()
    if not shutil.which("clangd"):
        print("SKIP: clangd not installed")
        return 0
    for fn in (test_rename_reaches_a_file_that_was_never_open,):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_rename_workspace: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_rename_workspace: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
