#!/usr/bin/env python3
"""
Integration test: the command palette.

Three defects are covered, all of which made the palette look present but
behave as decoration:

  * A space never reached the search query, because the guard tested
    len_trim(key) == 1 and a space trims to nothing. Every multi-word query
    was therefore untypeable.
  * Rows did not respond to clicks. The palette runs its own input loop, so
    the renderer's clickable-region table -- which only the main key router
    consults -- never saw the event.
  * Most commands did nothing. 23 of 38 registered ids had no case label in
    execute_palette_command and fell through to "Unknown command".

The last one is the reason this file checks a spread of commands rather than
the one that was reported: the defect was in the mapping as a whole.

Usage: python3 test/integration_palette.py [path-to-fac-binary]
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

ROWS, COLS = 30, 110
CTRL_P = "\x10"
failures = []


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if not ok:
        failures.append(name)
        if detail:
            for ln in str(detail).split("\n")[:14]:
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
    def __init__(self, binary, content="hello\nworld\nthird\n"):
        self.home = tempfile.mkdtemp(prefix="fac_palette_")
        cfg = os.path.join(self.home, ".config", "fac")
        os.makedirs(cfg)
        with open(os.path.join(cfg, "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.target = os.path.join(self.home, "a.txt")
        with open(self.target, "w") as f:
            f.write(content)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.home)
        self.drain(1.5)

    def drain(self, w=0.5):
        end = time.time() + w
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                pass
            except pexpect.EOF:
                break

    def send(self, data, w=0.6):
        self.child.send(data)
        self.drain(w)

    def click(self, row, col, w=0.9):
        """1-based screen coords, SGR encoding."""
        self.child.send(f"\x1b[<0;{col};{row}M")
        self.child.send(f"\x1b[<0;{col};{row}m")
        self.drain(w)

    def display(self):
        return "\n".join(r.rstrip() for r in self.screen.display)

    def rows_containing(self, text):
        return [i + 1 for i, r in enumerate(self.screen.display)
                if text in r and "│" in r]

    def close(self):
        try:
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)


def test_space_reaches_the_query(binary):
    s = Session(binary)
    try:
        s.send(CTRL_P, 0.8)
        s.send("close", 0.6)
        s.send(" ", 0.5)
        s.send("pane", 0.7)
        d = s.display()
        check("close pane" in d,
              "a space reaches the search query, so multi-word filtering works",
              d)
    finally:
        s.close()


def test_click_runs_the_clicked_row(binary):
    """Not the highlighted one -- clicking row two must run row two."""
    s = Session(binary)
    try:
        s.send(CTRL_P, 0.8)
        s.send("split", 0.7)
        rows = s.rows_containing("Split")
        if len(rows) < 2:
            check(False, "two split commands are listed", s.display())
            return
        s.click(rows[1], 60)          # second row: Split Horizontal
        d = s.display()
        check("Command Palette" not in d, "the palette closes on a click", d)
        check(d.count("a.txt") >= 2,
              "clicking a row runs that row rather than the highlighted one", d)
    finally:
        s.close()


def test_click_outside_dismisses(binary):
    s = Session(binary)
    try:
        s.send(CTRL_P, 0.8)
        s.click(25, 5)
        check("Command Palette" not in s.display(),
              "a click outside the box dismisses the palette", s.display())
    finally:
        s.close()


def run_command(binary, query, content="hello\nworld\nthird\n"):
    s = Session(binary, content)
    try:
        s.send(CTRL_P, 0.8)
        s.send(query, 0.6)
        s.send("\r", 1.0)
        return s.display()
    finally:
        s.close()


def test_commands_actually_fire(binary):
    """A spread across the categories that had no case label at all."""
    cases = [
        ("split vertical", lambda d: d.count("a.txt") >= 2,
         "Split Vertical opens a second pane"),
        ("goto line", lambda d: "line" in d.lower(),
         "Go to Line opens its prompt"),
        ("replace", lambda d: "replace" in d.lower(),
         "Replace opens its prompt"),
        ("delete line", lambda d: "hello" not in d,
         "Delete Line removes the caret's line"),
    ]
    for query, pred, label in cases:
        d = run_command(binary, query)
        check(pred(d), label, d)
        check("Unknown command" not in d, f"{label}: no 'Unknown command'", d)


def test_find_next_without_a_search_does_not_type_a_letter(binary):
    """'n' navigates matches only during a search; otherwise it is a letter
    and would be inserted into the document."""
    d = run_command(binary, "find next")
    check("no active search" in d.lower(),
          "Find Next with no search reports it instead of typing 'n'", d)


def main():
    binary = find_binary()
    for fn in (test_space_reaches_the_query,
               test_click_runs_the_clicked_row,
               test_click_outside_dismisses,
               test_commands_actually_fire,
               test_find_next_without_a_search_does_not_type_a_letter):
        try:
            fn(binary)
        except Exception as exc:            # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    if failures:
        print(f"\nintegration_palette: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("\nintegration_palette: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
