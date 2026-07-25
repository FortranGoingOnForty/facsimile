#!/usr/bin/env python3
"""
Integration test: mouse events must never reach the document as text.

Regression test for the prompt byte-leak bug. Prompts that read raw bytes
(goto, replace, rename, save-as, the yes/no quit prompts) treated the ESC
that opens an SGR mouse report as "cancel" and exited, leaving the rest of
the report in the tty. The main loop then read `[<0;30;10M` as ordinary
printable keys and typed it into the file -- so merely nudging the mouse
while a prompt was open corrupted the buffer. The fortress navigator had
the same defect, where the digits reached type-to-jump instead.

Drives the real binary in a pty and clicks once while each prompt is open.

Usage: python3 test/integration_mouse.py [path-to-fac-binary]
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

# 20 identical-shaped lines, so any inserted junk is obvious in a diff
CONTENT = "".join(f"aaa{i:02d} alpha beta gamma\n" for i in range(1, 21))
ORIGINAL = CONTENT.splitlines()

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
    def __init__(self, binary, content, name="a.txt", args=None):
        self.home = tempfile.mkdtemp(prefix="fac_mouse_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.target = os.path.join(self.home, name)
        with open(self.target, "w") as f:
            f.write(content)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, args if args is not None else [self.target],
                                   dimensions=(ROWS, COLS), env=env, cwd=self.home,
                                   timeout=10)
        self.drain(1.5)

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

    def send(self, data, wait=0.4):
        self.child.send(data)
        self.drain(wait)

    def click(self, row, col, button=0):
        """1-based screen coords, SGR encoding."""
        self.child.send(f"\x1b[<{button};{col};{row}M")
        self.child.send(f"\x1b[<{button};{col};{row}m")
        self.drain(0.35)

    def wheel(self, row, col, up=True):
        self.child.send(f"\x1b[<{64 if up else 65};{col};{row}M")
        self.drain(0.35)

    def row_text(self, row):
        return self.screen.display[row - 1].rstrip()

    def find_row(self, needle):
        for y, r in enumerate(self.screen.display):
            if needle in r:
                return y + 1
        return None

    def save_and_read(self):
        self.child.send("\x13")
        self.drain(0.9)
        with open(self.target) as f:
            return f.read()

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)


def changed_lines(text):
    out = []
    for i, line in enumerate(text.splitlines()):
        if i < len(ORIGINAL) and line != ORIGINAL[i]:
            out.append((i + 1, line))
    return out


# Prompts that read raw bytes: key, name, a marker proving it opened, and
# whether the buffer must be dirty first. The marker assertion is what keeps
# this test honest -- without it a prompt that silently failed to open would
# report a green "no leak".
#
# F2/Alt-N rename is not listed: with no language server for a .txt file it
# only prints a status message, so there is no prompt to click into. It uses
# show_text_prompt, the same loop as save-as below, which is covered.
PROMPTS = [
    ("\x07", "ctrl-g goto", "Go to (line:col)", False),
    ("\x12", "ctrl-r replace", "Replace:", False),
    ("\x06", "ctrl-f search", "[f]:", False),
    ("\x11", "ctrl-q quit with unsaved changes", "Unsaved changes", True),
    ("\x17", "ctrl-w close tab with unsaved changes", "Unsaved changes", True),
]


def main():
    binary = find_binary()

    for seq, name, marker, needs_dirty in PROMPTS:
        s = Session(binary, CONTENT)
        row = s.find_row("aaa05")
        s.click(row, 6)
        if needs_dirty:
            s.send("Z", 0.4)          # dirty the buffer so the prompt appears
        s.send(seq, 1.2)

        opened = s.find_row(marker) is not None
        check(opened, f"{name}: prompt opened", f"no {marker!r} on screen")
        if not opened:
            s.close()
            continue

        s.click(10, 30)               # the event that used to corrupt the file
        s.wheel(10, 30)               # and a wheel notch, same encoding
        s.send("\x1b", 0.6)

        try:
            saved = s.save_and_read()
        except OSError as e:
            check(False, f"{name}: file readable after prompt", str(e))
            s.close()
            continue

        leaked = [c for c in changed_lines(saved) if "[<" in c[1]]
        check(not leaked, f"{name}: no mouse bytes typed into the buffer",
              str(leaked))
        s.close()

    # Save-As on an untitled buffer takes show_text_prompt, the same loop
    # rename uses.
    s = Session(binary, CONTENT)
    s.send("\x14", 0.8)               # ctrl-t: new untitled tab
    s.send("hello", 0.4)
    s.send("\x13", 1.2)               # ctrl-s: prompts for a filename
    check(s.find_row("Save as:") is not None, "save-as prompt opened",
          str(s.row_text(ROWS)))
    s.click(10, 30)
    s.wheel(10, 30)
    onscreen = [s.row_text(y) for y in range(1, ROWS + 1)]
    check(not any("[<" in r for r in onscreen),
          "save-as prompt: no mouse bytes echoed to the screen",
          str([r for r in onscreen if "[<" in r]))
    s.close()

    # Fortress navigator (welcome menu -> 'b' to browse). A smoke check only:
    # the navigator swallows mouse reports rather than feeding their digits to
    # type-to-jump, but the selection move was never reproduced in a pty, so
    # this asserts the safe outcome rather than a fixed symptom. The welcome
    # menu is a separate module and was never affected.
    s = Session(binary, CONTENT, args=[])
    s.drain(1.0)
    check(s.find_row("Welcome Menu") is not None, "welcome menu reached",
          str(s.row_text(2)))
    s.send("b", 1.2)                  # browse: opens the navigator
    check(s.find_row("Welcome Menu") is None, "navigator reached from menu",
          str(s.row_text(2)))
    before = [s.row_text(y) for y in range(1, ROWS + 1)]
    s.click(6, 20)
    s.click(7, 20, button=2)
    s.wheel(6, 20)
    after = [s.row_text(y) for y in range(1, ROWS + 1)]
    check(before == after,
          "navigator: clicks do not move the selection",
          f"{sum(1 for a, b in zip(before, after) if a != b)} rows changed")
    check(not any("[<" in r for r in after),
          "navigator: no mouse bytes echoed")
    s.close()

    if failures:
        print(f"integration_mouse: FAILED ({len(failures)}: {', '.join(failures)})")
        sys.exit(1)
    print("integration_mouse: ALL PASSED")


if __name__ == "__main__":
    main()
