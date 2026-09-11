#!/usr/bin/env python3
"""Regression checks for stateful Wolf syntax rendering.

Usage: python3 test/integration_wolf_syntax.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import json
import os
import shutil
import sys
import tempfile
import time

try:
    import pexpect
    import pyte
except ImportError as exc:
    print(f"SKIP: missing dependency ({exc}); pip3 install pexpect pyte")
    sys.exit(0)

ROWS, COLS = 20, 100
failures = []


def check(condition, name, detail=""):
    print(f"{'ok  ' if condition else 'FAIL'} {name}")
    if not condition:
        failures.append(name)
        if detail:
            print(f"        {detail}")


def find_binary():
    if len(sys.argv) > 1:
        return os.path.abspath(sys.argv[1])
    candidate = os.path.join(os.path.dirname(os.path.dirname(__file__)), "fac")
    if os.path.exists(candidate):
        return candidate
    print("SKIP: no fac binary (build with make, or pass a path)")
    sys.exit(0)


class Session:
    def __init__(self, binary, content=None):
        self.root = tempfile.mkdtemp(prefix="fac_wolf_syntax_")
        self.home = os.path.join(self.root, "home")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"),
                  "w") as stream:
            json.dump({"first_run_completed": True,
                       "lsp_installer_seen": True,
                       "version": "1.0"}, stream)
        with open(os.path.join(self.home, ".config", "fac", "settings.json"),
                  "w") as stream:
            json.dump({"ui.theme": "steel", "ui.color_mode": "truecolor"},
                      stream)

        self.target = os.path.join(self.root, "top.lu")
        if content is None:
            content = ('let rows = """\n'
                       '    espresso,340\n'
                       '\n'
                       'fn later() {\n'
                       '    var n = 1\n')
        with open(self.target, "w") as stream:
            stream.write(content)

        env = {**os.environ, "HOME": self.home, "TERM": "xterm-256color",
               "COLORTERM": "truecolor"}
        for name in ("XDG_CONFIG_HOME", "FAC_SESSION", "NO_COLOR"):
            env.pop(name, None)

        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target],
                                   dimensions=(ROWS, COLS), env=env,
                                   cwd=self.root)
        self.drain(1.5)

    def drain(self, wait=0.4):
        end = time.time() + wait
        while time.time() < end:
            try:
                data = self.child.read_nonblocking(65536, 0.1)
                self.stream.feed(data.decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def send(self, data, wait=0.5):
        self.child.send(data)
        self.drain(wait)

    def cells(self, text, occurrence=1):
        seen = 0
        for y, row in enumerate(self.screen.display):
            start = 0
            while True:
                start = row.find(text, start)
                if start < 0:
                    break
                seen += 1
                if seen == occurrence:
                    return [self.screen.buffer[y][x]
                            for x in range(start, start + len(text))]
                start += len(text)
        return []

    def display(self):
        return "\n".join(row.rstrip() for row in self.screen.display)

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.root, ignore_errors=True)


def test_closing_block_string_clears_stale_interpolation(binary):
    session = Session(binary)
    try:
        # The initial unterminated block makes the brace below it look like
        # an interpolation opener. Add the missing delimiter as one editor
        # edit, matching the moment that previously left that state behind.
        session.send("\x1b[B\x1b[B", 0.4)
        session.send('\x1b[200~"""\x1b[201~', 1.0)

        # Exercise another redraw after the repair; stale lexical state used
        # to survive every subsequent frame.
        session.send("\x1b[B\x1b[A", 0.8)

        rows = session.cells("rows")
        later = session.cells("later")
        literal = session.cells("espresso,340")
        opening = session.cells('"""', 1)
        closing = session.cells('"""', 2)
        check(rows and later and
              all(cell.fg == rows[0].fg for cell in later),
              "code after a repaired triple-quoted string is plain code",
              session.display())
        check(literal and opening and closing and
              all(cell.fg == literal[0].fg
                  for cell in opening + closing),
              "both Wolf triple quotes use the standard string color",
              session.display())
    finally:
        session.close()


def test_interpolation_expression_uses_code_colors(binary):
    hole = '{if count > 2 { count * 3 } else { 0 }}'
    content = (f'let label = "value {hole} tail"\n'
               'fn outside() {\n'
               '    var n = 7\n'
               '}\n')
    session = Session(binary, content)
    try:
        hole_cells = session.cells(hole)
        literal_cells = session.cells('"value ')
        outside_keyword = session.cells('fn outside')
        outside_number = session.cells('n = 7')
        detail = session.display()

        if_pos = hole.index('if')
        else_pos = hole.index('else')
        number_positions = [hole.index('2'), hole.index('3'), hole.index('0')]
        operator_positions = [hole.index('>'), hole.index('*')]

        check(hole_cells and literal_cells and
              hole_cells[0].fg == hole_cells[-1].fg and
              hole_cells[0].fg != literal_cells[0].fg,
              "Wolf interpolation braces retain their accent color",
              detail)
        check(hole_cells and outside_keyword and
              all(hole_cells[if_pos + offset].fg == outside_keyword[offset].fg
                  for offset in range(2)) and
              all(hole_cells[else_pos + offset].fg == outside_keyword[0].fg
                  for offset in range(4)),
              "Wolf keywords inside interpolation use normal code colors",
              detail)
        check(hole_cells and outside_number and
              all(hole_cells[pos].fg == outside_number[-1].fg
                  for pos in number_positions),
              "Wolf numbers inside interpolation use normal code colors",
              detail)
        check(hole_cells and
              hole_cells[operator_positions[0]].fg ==
              hole_cells[operator_positions[1]].fg and
              hole_cells[operator_positions[0]].fg != hole_cells[0].fg,
              "Wolf operators inside interpolation use their code color",
              detail)
    finally:
        session.close()


def main():
    binary = find_binary()
    test_closing_block_string_clears_stale_interpolation(binary)
    test_interpolation_expression_uses_code_colors(binary)
    if failures:
        print(f"integration_wolf_syntax: FAILED ({len(failures)}): " +
              ", ".join(failures))
        return 1
    print("integration_wolf_syntax: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
