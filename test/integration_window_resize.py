#!/usr/bin/env python3
"""A window resize invalidates the whole terminal grid.

Terminal emulators may retain or reflow alternate-screen cells while their
grid changes.  FACSIMILE must therefore erase and repaint after a resize, and
must not let the first following arrow key take the caret-only fast path.

Usage: python3 test/integration_window_resize.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import os
import re
import select
import signal
import shutil
import sys
import tempfile
import time

try:
    import pexpect
    import pyte
except ImportError as error:
    print(f"SKIP: missing dependency ({error}); pip3 install pexpect pyte")
    sys.exit(0)


INITIAL_ROWS, INITIAL_COLS = 30, 100
LARGE_ROWS, LARGE_COLS = 52, 180
FINAL_ROWS, FINAL_COLS = 24, 76
QUEUED_ROWS, QUEUED_COLS = 80, 260
CONVERGED_ROWS, CONVERGED_COLS = 26, 84
CUP = re.compile(rb"\x1b\[(\d+);(\d+)H")
failures = []


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}" +
          (f"  [{detail}]" if detail and not ok else ""))
    if not ok:
        failures.append(name)


def find_binary():
    if len(sys.argv) > 1:
        return os.path.abspath(sys.argv[1])
    here = os.path.dirname(os.path.abspath(__file__))
    candidate = os.path.join(os.path.dirname(here), "fac")
    if os.path.exists(candidate):
        return candidate
    print("SKIP: no fac binary (build with make, or pass a path)")
    sys.exit(0)


class Session:
    def __init__(self, binary):
        self.root = tempfile.mkdtemp(prefix="fac-window-resize-")
        config = os.path.join(self.root, ".config", "fac")
        os.makedirs(config)
        with open(os.path.join(config, "state.json"), "w") as state:
            state.write('{"first_run_completed": true, '
                        '"lsp_installer_seen": true, "version": "1.0"}\n')

        self.target = os.path.join(self.root, "resize.txt")
        with open(self.target, "w") as source:
            for line in range(1, 80):
                source.write(f"ROW {line:03d} " + "content " * 18 + "\n")

        env = {**os.environ, "HOME": self.root, "TERM": "xterm-256color"}
        env.pop("XDG_CONFIG_HOME", None)
        self.child = pexpect.spawn(
            binary, [self.target], dimensions=(INITIAL_ROWS, INITIAL_COLS),
            env=env, cwd=self.root, timeout=10)
        self.screen = pyte.Screen(INITIAL_COLS, INITIAL_ROWS)
        self.stream = pyte.Stream(self.screen)
        self.drain(1.5)

    def drain(self, wait=0.6):
        output = b""
        deadline = time.time() + wait
        while time.time() < deadline:
            try:
                output += self.child.read_nonblocking(65536, 0.06)
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break
        if output:
            self.stream.feed(output.decode("utf-8", "replace"))
        return output

    def resize(self, rows, cols):
        # The host grid and the child PTY change together in a real terminal.
        self.screen.resize(lines=rows, columns=cols)
        self.child.setwinsize(rows, cols)
        return self.drain(1.0)

    def resize_while_old_frame_is_queued(self):
        # Let FACSIMILE start emitting a large-grid frame without consuming
        # it. Then change the PTY and feed both frames to the already-small
        # host grid, matching a terminal host that applies monitor geometry
        # before it has presented the application's pending output.
        self.child.setwinsize(QUEUED_ROWS, QUEUED_COLS)
        deadline = time.time() + 1.0
        while time.time() < deadline:
            readable, _, _ = select.select([self.child.fileno()], [], [], 0.03)
            if readable:
                break
        self.screen.resize(lines=CONVERGED_ROWS, columns=CONVERGED_COLS)
        self.child.setwinsize(CONVERGED_ROWS, CONVERGED_COLS)
        return self.drain(1.5)

    def send(self, data, wait=0.7):
        self.child.send(data)
        return self.drain(wait)

    def snapshot(self):
        cells = []
        for row in range(self.screen.lines):
            for col in range(self.screen.columns):
                cell = self.screen.buffer[row][col]
                cells.append((cell.data, cell.fg, cell.bg, cell.bold,
                              cell.italics, cell.reverse))
        return (tuple(self.screen.display), tuple(cells),
                self.screen.cursor.y, self.screen.cursor.x)

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.root, ignore_errors=True)


def addressed_rows(output):
    return {int(match.group(1)) for match in CUP.finditer(output)}


def check_resize_transaction(output, label):
    begin = output.find(b"\x1b[?2026h")
    clear = output.find(b"\x1b[2J\x1b[H")
    end = output.rfind(b"\x1b[?2026l")
    check(clear >= 0, f"{label}: the retained grid is erased")
    check(begin >= 0 and begin < clear < end,
          f"{label}: clear and repaint are synchronized",
          f"begin={begin}, clear={clear}, end={end}")


def main():
    session = Session(find_binary())
    try:
        grow = session.resize(LARGE_ROWS, LARGE_COLS)
        shrink = session.resize(FINAL_ROWS, FINAL_COLS)

        for output, label in ((grow, "grow"), (shrink, "shrink")):
            check_resize_transaction(output, label)

        check(session.screen.lines == FINAL_ROWS and
              session.screen.columns == FINAL_COLS,
              "the final terminal geometry is active")
        check("Ln 1, Col 1" in session.screen.display[-1],
              "the status bar moved to the resized bottom row",
              session.screen.display[-1])

        first_arrow = session.send(b"\x1b[B")
        rows = addressed_rows(first_arrow)
        check(len(rows) >= FINAL_ROWS - 4,
              "the first post-resize arrow repaints the whole grid",
              f"only addressed rows {sorted(rows)}")

        after_arrow = session.snapshot()
        session.send(b"\x0c")
        check(session.snapshot() == after_arrow,
              "the post-resize frame matches an explicit full repaint")

        second_arrow = session.send(b"\x1b[B")
        fast_rows = addressed_rows(second_arrow)
        check(len(fast_rows) < 10,
              "later arrows return to the caret-only fast path",
              f"addressed {len(fast_rows)} rows")

        # Embedded terminal hosts can request a redraw with SIGWINCH even if
        # the final dimensions did not change. A size-only poll misses it.
        if hasattr(signal, "SIGWINCH"):
            os.kill(session.child.pid, signal.SIGWINCH)
            same_size = session.drain(1.0)
            check_resize_transaction(same_size, "same-size SIGWINCH")
            after_signal = session.send(b"\x1b[B")
            signal_rows = addressed_rows(after_signal)
            check(len(signal_rows) >= FINAL_ROWS - 4,
                  "the redraw event also guards the next arrow",
                  f"only addressed rows {sorted(signal_rows)}")

        queued = session.resize_while_old_frame_is_queued()
        check(queued.count(b"\x1b[2J\x1b[H") >= 2,
              "a resize during repaint erases both old and final grids",
              f"saw {queued.count(bytes([27]) + b'[2J')} clears")
        check(session.screen.lines == CONVERGED_ROWS and
              session.screen.columns == CONVERGED_COLS,
              "a queued old-size frame converges on the latest geometry")

        converged_arrow = session.send(b"\x1b[B")
        converged_rows = addressed_rows(converged_arrow)
        check(len(converged_rows) >= CONVERGED_ROWS - 4,
              "the converged resize also guards its first arrow",
              f"only addressed rows {sorted(converged_rows)}")
        converged = session.snapshot()
        session.send(b"\x0c")
        check(session.snapshot() == converged,
              "queued old-size output leaves no cells behind")
    finally:
        session.close()

    if failures:
        print(f"\nintegration_window_resize: FAILED ({len(failures)}): " +
              ", ".join(failures))
        sys.exit(1)
    print("\nintegration_window_resize: ALL PASSED")


if __name__ == "__main__":
    main()
