#!/usr/bin/env python3
"""
The terminal panel's rows are not the document's.

update_viewport, the caret placement and the page-size math each derived the
document height from screen_rows - 2 and ignored the terminal panel, while the
renderer stopped drawing at last_content_row. On a 30-row terminal with a
9-row panel that is 28 rows against 19: moving the caret down did not scroll
the viewport until the caret passed row 28, so it walked off the last drawn
line and sat behind the panel, and each page key overshot by the panel height.

Reaching the state needs a mouse: every keyboard route that unfocuses the
panel also hides it, but a click in the document unfocuses it and leaves it up
(command_handler_module.f90).

Usage: python3 test/integration_termpanel_viewport.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import os
import re
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


def find_binary():
    if len(sys.argv) > 1:
        return os.path.abspath(sys.argv[1])
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    cand = os.path.join(root, "fac")
    if os.path.exists(cand):
        return cand
    import glob
    for p in glob.glob(os.path.join(root, "build", "gfortran_*", "app", "fac")):
        return p
    print("SKIP: no fac binary found (run make first)")
    sys.exit(0)


class Session:
    def __init__(self, binary, path, home):
        env = {**os.environ, "TERM": "xterm-256color", "HOME": home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.proc = pexpect.spawn(binary, [path], dimensions=(ROWS, COLS),
                                  env=env, cwd=os.path.dirname(path), timeout=20)
        self.drain(2.5)

    def drain(self, seconds=0.35):
        end = time.time() + seconds
        while time.time() < end:
            try:
                self.stream.feed(
                    self.proc.read_nonblocking(65536, 0.05).decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def send(self, data, seconds=0.06):
        self.proc.send(data)
        self.drain(seconds)

    def click(self, row, col):
        """SGR 1006 left press + release."""
        self.send(f"\x1b[<0;{col};{row}M", 0.15)
        self.send(f"\x1b[<0;{col};{row}m", 0.3)

    def caret_line(self):
        m = re.search(r"Ln (\d+)", self.screen.display[ROWS - 1])
        return int(m.group(1)) if m else None

    def drawn_lines(self):
        """{document line number: screen row} for every line the editor drew."""
        out = {}
        for y in range(2, ROWS):
            m = re.match(r"\s*(\d+)\s", self.screen.display[y - 1])
            if m:
                out[int(m.group(1))] = y
        return out

    def panel_visible(self):
        """The panel's separator row, whatever its focus state."""
        return any("terminal" in self.screen.display[y].lower() for y in range(ROWS))

    def close(self):
        try:
            self.proc.close(force=True)
        except Exception:
            pass


def run(binary):
    root = tempfile.mkdtemp(prefix="fac_tpv_")
    home = os.path.join(root, "home")
    os.makedirs(os.path.join(home, ".config", "fac"))
    with open(os.path.join(home, ".config", "fac", "state.json"), "w") as fh:
        fh.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                 ' "version": "1.0"}\n')
    path = os.path.join(home, "long.txt")
    with open(path, "w") as fh:
        for i in range(1, 201):
            fh.write(f"line {i} of the fixture\n")

    failures = 0
    s = Session(binary, path, home)
    try:
        s.send("\x1bt", 2.5)                    # alt-t: open the terminal panel
        if not s.panel_visible():
            print("SKIP: terminal panel did not open (no shell?)")
            return 0
        print("ok   terminal panel open")

        s.click(5, 20)                          # unfocus it, keep it visible
        if not s.panel_visible():
            print("FAIL panel closed on a document click; it should only unfocus")
            return 1
        print("ok   panel still visible after a document click")

        drawn = s.drawn_lines()
        rows_drawn = len(drawn)
        if rows_drawn >= ROWS - 2:
            print(f"FAIL editor drew {rows_drawn} rows over a visible panel")
            failures += 1
        else:
            print(f"ok   editor draws {rows_drawn} rows, not {ROWS - 2}")

        # Arrow down well past the shortened viewport. The caret's line must
        # stay drawn at every step -- that is the whole invariant.
        offscreen = 0
        for _ in range(45):
            s.send("\x1b[B", 0.07)
            line = s.caret_line()
            if line is not None and line not in s.drawn_lines():
                offscreen += 1
        if offscreen:
            print(f"FAIL caret's line was not drawn on {offscreen}/45 arrow-down steps")
            failures += 1
        else:
            print("ok   caret stayed on a drawn line for 45 arrow-down steps")

        # Paging must not overshoot by the panel height either.
        offscreen = 0
        for seq in ("\x1b[6~", "\x1b[6~", "\x1b[5~", "\x1b[6~", "\x1b[5~", "\x1b[5~"):
            s.send(seq, 0.35)
            line = s.caret_line()
            if line is not None and line not in s.drawn_lines():
                offscreen += 1
        if offscreen:
            print(f"FAIL caret's line was not drawn after {offscreen}/6 page keys")
            failures += 1
        else:
            print("ok   caret stayed on a drawn line across 6 page keys")

        # The caret must never be parked on a row the panel owns.
        panel_top = min((y + 1 for y in range(ROWS)
                         if "terminal" in s.screen.display[y].lower()), default=ROWS)
        if s.screen.cursor.y + 1 >= panel_top:
            print(f"FAIL caret sits on screen row {s.screen.cursor.y + 1}, "
                  f"panel starts at {panel_top}")
            failures += 1
        else:
            print(f"ok   caret on row {s.screen.cursor.y + 1}, above the panel "
                  f"at {panel_top}")
    finally:
        s.close()
        shutil.rmtree(root, ignore_errors=True)

    print("integration_termpanel_viewport: "
          + ("ALL PASSED" if failures == 0 else f"{failures} FAILURE(S)"))
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(run(find_binary()))
