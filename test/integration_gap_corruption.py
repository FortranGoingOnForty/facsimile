#!/usr/bin/env python3
"""Integration test: a long session must not smear old text over new.

The gap buffer moves its gap by shifting the text between the caret and the
gap to the far side. Moving the gap LEFT shifts that text RIGHT by the gap
size, so source and destination overlap once the gap is smaller than the
distance moved. The copy ran forward and read bytes it had already written.

The shape of the damage is what makes it so unpleasant: a ragged block of
text from earlier in the file appears in the middle of a section further
down, cutting across line boundaries, and it survives the save because the
buffer really is that shape.

Nothing shows until the gap has been eaten into, so it took a long editing
session to appear -- which is exactly how it was reported: "I had to rewrite
that section three times."

This drives the whole editor rather than the buffer alone, so the caret
arithmetic, the save path and the buffer all have to agree.

Usage: python3 test/integration_gap_corruption.py [path-to-fac-binary]
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
CTRL_HOME, CTRL_END = "\x1b[1;5H", "\x1b[1;5F"
END = "\x1b[F"
CTRL_S = "\x13"

# Every line is one repeated letter, so a byte in the wrong place is obvious.
NLINES, WIDTH = 400, 40

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


def striped():
    return "".join(chr(ord("A") + (i % 26)) * WIDTH + "\n"
                   for i in range(1, NLINES + 1))


class Session:
    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_gap_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_gap_work_")
        self.path = os.path.join(self.work, "doc.txt")
        self.original = striped()
        with open(self.path, "w") as f:
            f.write(self.original)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.path], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.work)
        self.drain(2.5)

    def drain(self, w=0.3):
        end = time.time() + w
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def send(self, d, w=0.2):
        self.child.send(d)
        self.drain(w)

    def type_bulk(self, text, chunk=8):
        """Typing is the point: every character eats one byte of the gap.

        Paced deliberately. Driving the pty faster than the editor reads
        DROPS input silently -- 64 characters per 100ms loses about half --
        and a test that lost half its keystrokes would draw the wrong
        conclusion rather than fail honestly. 8 per 50ms is comfortably
        under the threshold even with a full suite running alongside.
        caret_col() below is the check that it really all arrived."""
        for i in range(0, len(text), chunk):
            self.child.send(text[i:i + chunk])
            self.drain(0.05)

    def caret_col(self):
        import re
        m = re.search(r"Ln (\d+), Col (\d+)", self.screen.display[-1])
        return int(m.group(2)) if m else -1

    def disk(self):
        return open(self.path).read()

    def close(self):
        try:
            self.child.close(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def damaged(text):
    """Lines of the striped document that are no longer one repeated letter."""
    bad = []
    for i, line in enumerate(text.split("\n")[:NLINES], start=1):
        want = chr(ord("A") + (i % 26)) * WIDTH
        if line != want:
            bad.append((i, line, want))
    return bad


def test_a_long_session_then_an_edit_near_the_top(binary):
    print("\nTyping a lot, then editing far earlier, leaves the rest alone")
    s = Session(binary)
    try:
        check(not damaged(s.disk()), "the document starts intact")

        # Eat into the gap WITHOUT growing the buffer. The buffer allocates
        # max(8192, 2*content); at ~16KB of document that is ~16KB of gap, so
        # 2000 characters leaves ~14KB against an ~18KB jump back to the top
        # and the move overlaps. Two ways to get this wrong, both of which
        # made an earlier version pass against the bug: typing enough to
        # trigger a GROWTH (which reallocates and hands back a wide gap), and
        # typing faster than the editor reads, which silently drops input --
        # 64 characters per 100ms loses half of them. This rate registers in
        # full; verify with the status bar column if it is ever changed.
        s.send(CTRL_END, 0.5)
        s.type_bulk("z" * 2000)
        s.drain(1.5)
        check(s.caret_col() == 2001,
              "every keystroke arrived (a dropped one invalidates the rest)",
              f"caret column {s.caret_col()}, expected 2001")

        # The ordinary act of scrolling back up to fix something.
        s.send(CTRL_HOME, 0.5)
        s.send(END, 0.3)
        s.type_bulk("QQQ")
        s.drain(0.8)

        s.send(CTRL_S, 2.0)
        s.drain(1.2)

        after = s.disk()
        bad = [b for b in damaged(after) if b[0] != 1]   # line 1 is the one we typed on
        check(not bad,
              "no other line was touched by the edit near the top",
              "\n".join("line %d\n  got : %r\n  want: %r" % (n, g[:64], w[:64])
                        for n, g, w in bad[:4]))

        first = after.split("\n")[0]
        check(first.startswith("B" * WIDTH) and first.endswith("QQQ"),
              "and the line that was edited holds exactly what was typed",
              repr(first[:70]))
    finally:
        s.close()


def test_the_same_with_the_edit_in_the_middle(binary):
    print("\nAnd with the later edit landing in the middle of the document")
    s = Session(binary)
    try:
        s.send(CTRL_END, 0.5)
        s.type_bulk("z" * 2000)
        s.drain(1.5)
        check(s.caret_col() == 2001, "every keystroke arrived",
              f"caret column {s.caret_col()}, expected 2001")

        s.send(CTRL_HOME, 0.5)
        for _ in range(29):                     # line 30
            s.send("\x1b[B", 0.02)
        s.send(END, 0.3)
        s.type_bulk("MID")
        s.drain(0.8)
        s.send(CTRL_S, 2.0)
        s.drain(1.2)

        after = s.disk()
        bad = [b for b in damaged(after) if b[0] != 30]
        check(not bad,
              "every line but the edited one is untouched",
              "\n".join("line %d\n  got : %r\n  want: %r" % (n, g[:64], w[:64])
                        for n, g, w in bad[:4]))
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_a_long_session_then_an_edit_near_the_top,
               test_the_same_with_the_edit_in_the_middle):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_gap_corruption: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_gap_corruption: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
