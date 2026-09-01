#!/usr/bin/env python3
"""Terminal regression checks for the LSP references surface.

Usage: python3 test/integration_references_panel.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte, and clangd on PATH
"""

import codecs
import json
import os
import re
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

if shutil.which("clangd") is None:
    print("SKIP: clangd not on PATH; references need a language server")
    sys.exit(0)

ROWS, COLS = 32, 120
SOURCE = """static int square(int value)
{
    return value * value;
}

int main(void)
{
    int total = square(1);
    total += square(2);
    total += square(3);
    total += square(4);
    total += square(5);
    return total;
}
"""
failures = []


def check(condition, name, detail=""):
    print(f"{'ok  ' if condition else 'FAIL'} {name}")
    if not condition:
        failures.append(name)
        if detail:
            for line in str(detail).splitlines()[:10]:
                print("        " + line[:110])


def find_binary():
    if len(sys.argv) > 1:
        return os.path.abspath(sys.argv[1])
    candidate = os.path.join(os.path.dirname(os.path.dirname(__file__)), "fac")
    if os.path.exists(candidate):
        return candidate
    print("SKIP: no fac binary")
    sys.exit(0)


class Session:
    def __init__(self, binary):
        self.root = tempfile.mkdtemp(prefix="fac_refs_")
        self.home = os.path.join(self.root, "home")
        self.work = os.path.join(self.root, "project")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        os.makedirs(self.work)
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            json.dump({"first_run_completed": True, "lsp_installer_seen": True,
                       "version": "1.0"}, f)
        with open(os.path.join(self.home, ".config", "fac", "settings.json"), "w") as f:
            json.dump({"ui.theme": "steel", "ui.color_mode": "truecolor",
                       "ui.icons": "unicode"}, f)

        self.target = os.path.join(self.work, "main.c")
        with open(self.target, "w") as f:
            f.write(SOURCE)
        with open(os.path.join(self.work, "compile_commands.json"), "w") as f:
            json.dump([{"directory": self.work, "command": "cc -c main.c",
                        "file": self.target}], f)

        env = {**os.environ, "HOME": self.home, "TERM": "xterm-256color",
               "COLORTERM": "truecolor"}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        env.pop("NO_COLOR", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.decoder = codecs.getincrementaldecoder("utf-8")("replace")
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   cwd=self.work, env=env, timeout=15)
        self.drain(7.0)

    def drain(self, wait=0.5):
        end = time.time() + wait
        while time.time() < end:
            try:
                data = self.child.read_nonblocking(65536, 0.1)
                self.stream.feed(self.decoder.decode(data))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def send(self, data, wait=0.6):
        self.child.send(data)
        self.drain(wait)

    def goto_symbol(self):
        self.send("\x1b[1;5H", 0.3)
        for _ in range(11):
            self.send("\x1b[C", 0.03)
        self.drain(0.4)

    def open_references(self):
        self.goto_symbol()
        self.send("\x1br", 2.0)

    def close(self):
        try:
            self.child.close(force=True)
        finally:
            shutil.rmtree(self.root, ignore_errors=True)


def main():
    session = Session(find_binary())
    try:
        session.open_references()
        display = session.screen.display
        panel_col = COLS - (COLS * 40 // 100)

        check("References" not in display[0] and "References" in display[1],
              "the panel begins below the tab strip",
              "\n".join(display[:4]))
        check(re.search(r"\d+ results?", display[1][panel_col:]) is not None,
              "the title band carries a right-aligned result count",
              display[1][panel_col:])
        check("LOCATIONS" in display[2][panel_col:],
              "a section label separates the title from the results",
              "\n".join(display[1:6]))
        check(not display[3][panel_col:].strip(),
              "the section heading has a breathing row before the list",
              "\n".join(display[1:6]))

        result_rows = [row for row in range(4, ROWS - 2)
                       if "main.c" in display[row][panel_col:]]
        check(len(result_rows) >= 2,
              "reference locations render in the results area",
              "\n".join(display[1:10]))
        if result_rows:
            selected = [session.screen.buffer[result_rows[0]][col]
                        for col in range(panel_col, COLS)]
            check(all(cell.reverse for cell in selected),
                  "the selected location is a full-width reverse-video row")

        footer = display[ROWS - 2][panel_col:]
        check("Navigate" in footer and "Enter" in footer and "Esc" in footer,
              "navigation help lives in the fixed footer", footer)
        check(re.search(r"1/\d+", footer) is not None,
              "the footer reports selection progress", footer)

        session.send("j", 0.8)
        moved_footer = session.screen.display[ROWS - 2][panel_col:]
        check(re.search(r"2/\d+", moved_footer) is not None,
              "j still advances the selected reference", moved_footer)

        session.send("\x1b", 0.8)
        check("References" not in "\n".join(session.screen.display),
              "Esc closes the panel and restores the editor")
        with open(session.target) as f:
            check(f.read() == SOURCE,
                  "opening and navigating references does not edit the file")
    finally:
        session.close()

    print()
    if failures:
        print(f"integration_references_panel: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_references_panel: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
