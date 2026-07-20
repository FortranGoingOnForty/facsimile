#!/usr/bin/env python3
"""
Integration test: the hardware caret must track the text being typed.

Regression test for the stale-session-cursor bug: restoring a workspace
whose active tab had its cursor parked deep in another file leaked that
cursor into a newly opened tab. Typing then inserted at the correct
(clamped) position while the caret was drawn rows below the text, out in
the '~' area - most visibly right after '{' auto-close + Enter, since
every Enter pushed the phantom row further down.

Drives the real binary in a pty with a deliberately poisoned restored
session (tab cursor at line 6, col 5) and asserts after every keystroke
that pyte's hardware cursor sits on the row where the text actually is.

Usage: python3 test/integration_caret.py [path-to-fac-binary]
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

ROWS, COLS = 30, 120


def find_binary():
    if len(sys.argv) > 1:
        return os.path.abspath(sys.argv[1])
    here = os.path.dirname(os.path.abspath(__file__))
    root = os.path.dirname(here)
    cand = os.path.join(root, "fac")
    if os.path.exists(cand):
        return cand
    print("SKIP: no fac binary (build with make, or pass a path)")
    sys.exit(0)


def make_sandbox_home():
    home = tempfile.mkdtemp(prefix="fac_caret_home_")
    os.makedirs(os.path.join(home, ".fac"))
    os.makedirs(os.path.join(home, ".config", "fac"))
    os.makedirs(os.path.join(home, "proj"))

    # A real file the restored tab points at, with enough lines that its
    # persisted cursor (line 6) is a legal position there
    with open(os.path.join(home, "notes.txt"), "w") as f:
        f.write("\n".join(f"note line {i}" for i in range(1, 21)) + "\n")

    # Suppress the first-run LSP installer panel
    with open(os.path.join(home, ".config", "fac", "state.json"), "w") as f:
        f.write('{\n  "first_run_completed": true,\n'
                '  "lsp_installer_seen": true,\n  "version": "1.0"\n}\n')

    # Restored session: notes.txt active with cursor at (6, 5) - the
    # poison that used to leak into the next opened file
    with open(os.path.join(home, ".fac", "workspace.json"), "w") as f:
        f.write(f"""{{
  "version": "1.0",
  "workspace_path": "{home}",
  "last_opened": "20260714",
  "tabs": [
    {{
      "filename": "notes.txt",
      "is_orphan": false,
      "modified": false,
      "panes": [
        {{
          "x_start": 0.0000,
          "y_start": 0.0000,
          "x_end": 1.0000,
          "y_end": 1.0000,
          "filename": "notes.txt",
          "cursor_line": 6,
          "cursor_column": 5,
          "viewport_line": 1,
          "viewport_column": 1
        }}
      ],
      "active_pane": 1
}}
  ],
  "active_tab": 1,
  "fuss_mode": {{
    "active": false,
    "width": 30
  }}
}}
""")
    return home


def main():
    binary = find_binary()
    home = make_sandbox_home()
    target = os.path.join(home, "proj", "fresh.c")
    open(target, "w").close()

    env = {**os.environ, "TERM": "xterm-256color", "HOME": home}
    env.pop("XDG_CONFIG_HOME", None)

    screen = pyte.Screen(COLS, ROWS)
    stream = pyte.Stream(screen)
    child = pexpect.spawn(binary, [target], dimensions=(ROWS, COLS),
                          env=env, cwd=os.path.dirname(target))

    def drain(wait=0.3):
        end = time.time() + wait
        while time.time() < end:
            try:
                stream.feed(child.read_nonblocking(65536, 0.1)
                            .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def last_text_row():
        rows = [i for i, r in enumerate(screen.display)
                if i not in (0, ROWS - 1) and r.strip()
                and not r.strip().startswith("~")]
        return max(rows) if rows else 1

    failures = []

    def check(step):
        cy = screen.cursor.y
        lt = last_text_row()
        ok = cy <= lt
        print(f"{'ok  ' if ok else 'FAIL'} {step:28s} caret row {cy}, last text row {lt}")
        if not ok:
            failures.append(step)

    drain(2.0)
    check("startup")

    # int main() <Enter> { <Enter> x - the sequence that made the phantom
    # caret walk down into the tildes (Enter advances the phantom line)
    for ch in "int main()":
        child.send(ch)
        drain(0.05)
    drain(0.3)
    check("typed 'int main()'")

    child.send("\x1b[C")  # right, over the auto-closed ')'
    drain(0.2)
    child.send("\r")
    drain(0.4)
    check("Enter")

    child.send("{")
    drain(0.3)
    check("'{' auto-close")

    child.send("\r")
    drain(0.4)
    check("Enter inside braces")

    child.send("x")
    drain(0.3)
    check("typed 'x'")

    # The caret must also sit on the exact row containing the typed 'x'
    # (renders as 'x}' since Enter moved the auto-closed brace along; a
    # ghost-text suggestion may sit between them, e.g. 'xor}')
    x_row = next((i for i, r in enumerate(screen.display)
                  if re.search(r"x\S*\}", r)), None)
    if x_row is None:
        print("FAIL typed text 'x...}' not found on screen")
        failures.append("typed text visible")
    elif screen.cursor.y != x_row:
        print(f"FAIL caret row {screen.cursor.y} but typed text is on row {x_row}")
        failures.append("caret on typed row")

    child.close(force=True)
    shutil.rmtree(home, ignore_errors=True)

    if failures:
        print(f"integration_caret: FAILED ({len(failures)}: {', '.join(failures)})")
        sys.exit(1)
    print("integration_caret: ALL PASSED")


if __name__ == "__main__":
    main()
