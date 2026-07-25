#!/usr/bin/env python3
"""
Integration test: Ctrl-Q closes what is open before it quits.

Ctrl-Q used to quit outright from anywhere, including with the terminal
panel focused, where it was the deliberate escape hatch. It now closes the
topmost open surface and only quits once nothing is open.

The two properties worth guarding are not the cascade itself but its edges:
quitting must still reach the unsaved-changes prompt, and Ctrl-Q must never
become unable to quit. A surface whose visibility flag outlives its drawing
would silently eat presses, so the last check hammers the key and insists
the process goes away.

Usage: python3 test/integration_ctrlq.py [path-to-fac-binary]
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

ROWS, COLS = 30, 100
CONTENT = "".join(f"line{i:02d} alpha beta\n" for i in range(1, 21))

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


class Session:
    def __init__(self, binary):
        self.base = tempfile.mkdtemp(prefix="fac_ctrlq_")
        self.home = os.path.join(self.base, "home")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.target = os.path.join(self.home, "a.txt")
        with open(self.target, "w") as f:
            f.write(CONTENT)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.home, timeout=10)
        self.drain(1.6)

    def drain(self, wait=0.5):
        end = time.time() + wait
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def send(self, data, wait=0.6):
        self.child.send(data)
        self.drain(wait)

    def alive(self):
        return self.child.isalive()

    def text(self):
        return "".join(self.screen.display)

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.base, ignore_errors=True)


def main():
    binary = find_binary()

    # Nothing open: quitting is unchanged, one press and it is gone
    s = Session(binary)
    s.send("\x11", 1.5)
    check(not s.alive(), "clean buffer with nothing open quits at once")
    s.close()

    # Quitting must still reach the unsaved-changes prompt
    s = Session(binary)
    s.send("Z", 0.5)
    s.send("\x11", 1.5)
    check("Unsaved changes" in s.text() and s.alive(),
          "a dirty buffer still prompts instead of quitting",
          f"alive={s.alive()}")
    s.close()

    # Each open surface costs one press before the quit
    for key, wait, label in (("\x1bt", 2.0, "terminal panel"),
                             ("\x02", 1.5, "fuss mode")):
        s = Session(binary)
        s.send(key, wait)
        s.send("\x11", 1.2)
        check(s.alive(), f"{label}: the first ctrl-q closes it rather than quitting")
        s.send("\x11", 1.5)
        check(not s.alive(), f"{label}: the next ctrl-q quits")
        s.close()

    # With several surfaces open, ctrl-q must still always get you out
    s = Session(binary)
    s.send("\x1bt", 1.5)
    s.send("\x02", 1.0)
    presses = 0
    while s.alive() and presses < 6:
        s.send("\x11", 0.9)
        presses += 1
    check(not s.alive(),
          "repeated ctrl-q always exits, so no surface can trap the user",
          f"still alive after {presses} presses")
    s.close()

    if failures:
        print(f"integration_ctrlq: FAILED ({len(failures)}: {', '.join(failures)})")
        sys.exit(1)
    print("integration_ctrlq: ALL PASSED")


if __name__ == "__main__":
    main()
