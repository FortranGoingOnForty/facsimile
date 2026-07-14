#!/usr/bin/env python3
"""
Integration smoke test for lazy fuss-tree expansion (non-git workspaces).

Drives the real binary through a pty (pexpect) and reconstructs the screen
with a terminal emulator (pyte), since fac renders with absolute cursor
positioning. Verifies:
  - activation shows collapsed dirs ('+ sub/') without their contents
  - space expands lazily (a.txt appears, glyph flips to '- sub/')
  - space collapses again
  - '.' reveals dotfile entries (lazy nodes carry is_dotfile)

Usage: python3 test/integration_lazy_tree.py [path-to-fac-binary]
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

ROWS, COLS = 40, 120


def find_binary():
    if len(sys.argv) > 1:
        return os.path.abspath(sys.argv[1])
    here = os.path.dirname(os.path.abspath(__file__))
    root = os.path.dirname(here)
    cand = os.path.join(root, "fac")
    if os.path.exists(cand):
        return cand
    import glob
    for p in glob.glob(os.path.join(root, "build", "gfortran_*", "app", "fac")):
        return p
    print("SKIP: no fac binary found (run make first)")
    sys.exit(0)


def make_fixture():
    d = tempfile.mkdtemp(prefix="fac_lazy_it_")
    os.makedirs(os.path.join(d, "sub"))
    open(os.path.join(d, "sub", "a.txt"), "w").write("inner\n")
    open(os.path.join(d, "opened.txt"), "w").write("hello\n")
    open(os.path.join(d, ".dotfile"), "w").write("dot\n")
    # must NOT be a git repo for the lazy path
    assert subprocess.run(["git", "rev-parse"], cwd=d,
                          capture_output=True).returncode != 0
    return d


class Screen:
    def __init__(self):
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.ByteStream(self.screen)

    def pump(self, proc, seconds):
        end = time.time() + seconds
        while time.time() < end:
            try:
                self.stream.feed(proc.read_nonblocking(65536, 0.05))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def text(self):
        return "\n".join(self.screen.display)

    def wait_for(self, proc, predicate, timeout, label):
        end = time.time() + timeout
        while time.time() < end:
            self.pump(proc, 0.1)
            if predicate(self.text()):
                return True
        print(f"FAIL {label}")
        print("---- screen ----")
        for row in self.screen.display:
            if row.strip():
                print("|" + row.rstrip())
        return False


def main():
    binary = find_binary()
    fixture = make_fixture()
    failures = 0
    try:
        scr = Screen()
        p = pexpect.spawn(binary, ["opened.txt"], cwd=fixture, timeout=15,
                          encoding=None, dimensions=(ROWS, COLS))

        ok = scr.wait_for(p, lambda t: "ctrl-b:fuss" in t, 10, "first frame")
        failures += 0 if ok else 1

        # activate fuss (Ctrl-B)
        p.send(b"\x02")
        ok = scr.wait_for(p, lambda t: "+ sub/" in t, 10,
                          "collapsed '+ sub/' visible after Ctrl-B")
        failures += 0 if ok else 1
        if "a.txt" in scr.text():
            print("FAIL a.txt visible before expand (tree not lazy)")
            failures += 1
        else:
            print("ok   a.txt hidden before expand")
        if ".dotfile" in scr.text():
            print("FAIL .dotfile visible despite hide-dotfiles default")
            failures += 1
        else:
            print("ok   dotfiles hidden by default")

        # selection starts on 'sub' (only dir, dirs sort first): expand
        p.send(b" ")
        ok = scr.wait_for(p, lambda t: "a.txt" in t and "- sub/" in t, 10,
                          "space expands sub (a.txt + '- sub/')")
        failures += 0 if ok else 1

        # collapse again
        p.send(b" ")
        ok = scr.wait_for(p, lambda t: "a.txt" not in t and "+ sub/" in t, 10,
                          "space collapses sub again")
        failures += 0 if ok else 1

        # '.' reveals dotfiles
        p.send(b".")
        ok = scr.wait_for(p, lambda t: ".dotfile" in t, 10,
                          "'.' reveals dotfiles")
        failures += 0 if ok else 1

        # quit
        p.send(b"\x11")
        time.sleep(0.4)
        try:
            p.expect(pexpect.EOF, timeout=5)
        except pexpect.TIMEOUT:
            p.terminate(force=True)

        if failures == 0:
            print("integration_lazy_tree: ALL PASSED")
        else:
            print(f"integration_lazy_tree: {failures} FAILURE(S)")
        return 1 if failures else 0
    finally:
        shutil.rmtree(fixture, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
