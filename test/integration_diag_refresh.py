#!/usr/bin/env python3
"""
Integration test: a diagnostic that stops being true stops being shown.

Diagnostics arrive from the language server on their own schedule, with no
keystroke behind them. The callback stored them and said nothing, so nothing
in the main loop knew the frame was now wrong: the message on the status bar
stayed as it was until something unrelated -- a cursor move, a save, another
keypress -- forced a redraw.

The visible form: type `tre`, Tab to complete the include to "tree.h", and
the status bar goes on reporting an error about the text you just replaced.
It reads as the server not having noticed the edit, when the server had
noticed and had already said so.

The assertion is that it clears with NO further input, because "press
another key and it fixes itself" is exactly the bug.

Needs clangd; skips cleanly without it.

Usage: python3 test/integration_diag_refresh.py [path-to-fac-binary]
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

ROWS, COLS = 32, 140
CTRL_HOME = "\x1b[1;5H"
END = "\x1b[F"

failures = []


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if not ok:
        failures.append(name)
        if detail:
            for ln in str(detail).split("\n")[:8]:
                print("        " + ln[:110])


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
    def __init__(self, binary, source):
        self.home = tempfile.mkdtemp(prefix="fac_dr_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_dr_work_")
        with open(os.path.join(self.work, "tree.h"), "w") as f:
            f.write("#ifndef TREE_H\n#define TREE_H\nint tree_size(void);\n#endif\n")
        self.target = os.path.join(self.work, "tree.c")
        with open(self.target, "w") as f:
            f.write(source)
        # Without a compilation database clangd guesses, and its guess is not
        # stable enough to assert on.
        with open(os.path.join(self.work, "compile_commands.json"), "w") as f:
            f.write('[{"directory":"%s","command":"cc -c tree.c","file":"%s"}]'
                    % (self.work, self.target))
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.work)
        self.drain(6.0)                    # clangd start plus first analysis

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

    def status(self):
        return self.screen.display[-1].strip()

    def wait_for_status(self, needle, limit=20.0):
        """Wait for the status bar to contain `needle`, sending nothing."""
        end = time.time() + limit
        while time.time() < end:
            self.drain(0.5)
            if needle in self.status():
                return True
        return False

    def wait_for_status_to_lose(self, needle, limit=20.0):
        end = time.time() + limit
        while time.time() < end:
            self.drain(0.5)
            if needle not in self.status():
                return True
        return False

    def close(self):
        try:
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def test_a_fixed_include_stops_being_reported(binary):
    print("\nCompleting an include clears the error about the old text")
    s = Session(binary, '#include "\nint main(void){return 0;}\n')
    try:
        # A hard failure, not a skip. clangd is present (main() checked) and
        # the file has a compilation database and an unterminated include, so
        # a diagnostic MUST arrive -- and it must reach the bar on its own.
        # This used to be a skip, which is how the first version of this test
        # passed against the bug: with no repaint the message never appeared
        # at all, and the test politely excused itself.
        check(s.wait_for_status("Missing terminating", 25.0),
              "the unterminated include is reported to begin with",
              f"status: {s.status()[:90]!r}")

        s.send(CTRL_HOME, 0.5)
        s.send(END, 0.5)
        for ch in "tre":
            s.send(ch, 0.8)
        s.send("\t", 1.5)                  # accept the completion

        # Nothing is sent from here on. That is the whole point: the message
        # has to go because the diagnostics changed, not because a keystroke
        # happened to repaint the bar.
        check(s.wait_for_status_to_lose("Missing terminating", 20.0),
              "and stops being reported with no further input",
              f"status still: {s.status()[:90]!r}")
    finally:
        s.close()


def test_a_new_error_is_reported_without_a_keystroke(binary):
    print("\nAnd the same in reverse: a new error appears on its own")
    s = Session(binary, "int main(void)\n{\n    return 0;\n}\n")
    try:
        check("Use of undeclared" not in s.status(),
              "a clean file reports nothing", s.status()[:90])

        s.send(CTRL_HOME, 0.5)
        s.send("\x1b[B\x1b[B", 0.5)        # line 3
        s.send(END, 0.4)
        for _ in range(2):                 # "0;" -> ""
            s.send("\x7f", 0.3)
        for ch in "zqx":                   # an identifier that cannot exist
            s.send(ch, 0.4)
        # The last keystroke is a word character, which does repaint -- but
        # clangd cannot have answered that fast. The message still has to
        # arrive on its own.
        check(s.wait_for_status("Use of undeclared", 20.0),
              "the new error appears without further input",
              f"status: {s.status()[:90]!r}")
    finally:
        s.close()


def main():
    binary = find_binary()
    if not shutil.which("clangd"):
        print("SKIP: clangd not installed")
        return 0
    for fn in (test_a_fixed_include_stops_being_reported,
               test_a_new_error_is_reported_without_a_keystroke):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_diag_refresh: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_diag_refresh: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
