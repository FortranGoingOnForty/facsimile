#!/usr/bin/env python3
"""Visual regression checks for semantic terminal styling.

This uses pyte cells rather than screenshots so color, reverse-video, and
stale-character failures remain deterministic across fonts and terminals.

Usage: python3 test/integration_theme_render.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import codecs
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time

try:
    import pexpect
    import pyte
except ImportError as exc:
    print(f"SKIP: missing dependency ({exc}); pip3 install pexpect pyte")
    sys.exit(0)

ROWS, COLS = 28, 100
failures = []


def check(condition, name, detail=""):
    print(f"{'ok  ' if condition else 'FAIL'} {name}" +
          (f"  [{detail}]" if detail and not condition else ""))
    if not condition:
        failures.append(name)


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
        self.root = tempfile.mkdtemp(prefix="fac_theme_render_")
        self.home = os.path.join(self.root, "home")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            json.dump({"first_run_completed": True, "lsp_installer_seen": True,
                       "version": "1.0"}, f)
        with open(os.path.join(self.home, ".config", "fac", "settings.json"), "w") as f:
            json.dump({"ui.theme": "steel", "ui.color_mode": "truecolor",
                       "ui.icons": "unicode", "ui.shadows": True}, f)
        self.target = os.path.join(self.home, "a.f90")
        with open(self.target, "w") as f:
            f.write("function alpha()\ninteger :: value\nend function alpha\n")
        subprocess.run(["git", "init", "-q", "-b", "feature/modern-terminal-ui"],
                       cwd=self.home, check=True)

        env = {**os.environ, "HOME": self.home, "TERM": "xterm-256color",
               "COLORTERM": "truecolor"}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        env.pop("NO_COLOR", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.decoder = codecs.getincrementaldecoder("utf-8")("replace")
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   cwd=self.home, env=env, timeout=10)
        self.drain(1.5)

    def drain(self, wait=0.4):
        end = time.time() + wait
        while time.time() < end:
            try:
                data = self.child.read_nonblocking(65536, 0.1)
                self.stream.feed(self.decoder.decode(data))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def send(self, data, wait=0.5):
        self.child.send(data)
        self.drain(wait)

    def cells(self, text, row=1):
        line = self.screen.display[row - 1]
        start = line.find(text)
        if start < 0:
            return []
        return [self.screen.buffer[row - 1][x]
                for x in range(start, start + len(text))]

    def text(self):
        return "\n".join(self.screen.display)

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.root, ignore_errors=True)


def main():
    s = Session(find_binary())
    try:
        active = s.cells("1 a.f90")
        check(active and all(cell.reverse for cell in active),
              "the active tab carries reverse-video semantics")
        check(active and all(cell.fg != "default" and cell.bg != "default"
                             for cell in active),
              "the active tab receives truecolor foreground and background")
        keyword = s.cells("function", 2)
        check(keyword and all(cell.fg != "default" for cell in keyword),
              "syntax tokens receive the theme foreground")
        check(keyword and all(cell.bg != "default" for cell in keyword),
              "syntax tokens retain the current-line background")
        plain = s.cells("value", 3)
        check(plain and all(cell.bg != "default" for cell in plain),
              "plain source text receives the editor background")
        check(s.screen.buffer[2][90].bg == plain[0].bg,
              "the editor background continues through trailing cells")
        check(s.screen.buffer[2][5].bg == plain[0].bg,
              "the gutter separator has no terminal-background gap")

        s.send("\x02", 0.8)  # ctrl-b: Fuss open
        tree_width = COLS * 30 // 100
        branch_row = s.screen.display[1]
        check("…" in branch_row[:tree_width],
              "a long Fuss branch name is ellipsized")
        check(branch_row[tree_width] == "│",
              "the branch label stops before the Fuss separator", branch_row[:40])
        check("…" not in branch_row[tree_width + 1:tree_width + 7],
              "the branch ellipsis does not leak into the line-number gutter",
              branch_row[:40])
        repo_start = branch_row.index("home", 0, tree_width)
        branch_end = branch_row.index("…", repo_start, tree_width)
        header_cells = [s.screen.buffer[1][col]
                        for col in range(repo_start, branch_end + 1)]
        check(header_cells and header_cells[0].bg != "default" and
              all(cell.bg == header_cells[0].bg for cell in header_cells),
              "the Fuss header keeps one continuous panel surface")
        s.send("\x02", 0.8)  # close Fuss

        s.send("\x14", 0.8)  # ctrl-t
        inactive = s.cells("1 a.f90")
        active_runs = []
        for x in range(COLS):
            cell = s.screen.buffer[0][x]
            if cell.reverse and cell.data.strip():
                active_runs.append(cell)
        check(inactive and not any(cell.reverse for cell in inactive),
              "inactive tabs use quiet text without selection video")
        check(active_runs and all(cell.bg != "default" for cell in active_runs),
              "the newly active tab retains a filled highlight")
        inactive_fill = inactive[0].fg if inactive and inactive[0].reverse else (
            inactive[0].bg if inactive else None)
        active_fill = active_runs[0].fg if active_runs and active_runs[0].reverse else (
            active_runs[0].bg if active_runs else None)
        check(inactive and active_runs and inactive_fill != active_fill,
              "active and inactive tabs have distinct fills")

        s.send("\x17", 0.6)  # close untitled tab
        clean_bars = []
        for _ in range(6):
            s.send("\x02", 0.45)  # ctrl-b: Fuss open/close
            clean_bars.append(s.screen.display[0].rstrip())
        check(all("o" not in bar for bar in clean_bars),
              "repeated Fuss toggles leave no phantom edge characters",
              repr(clean_bars[-2:]))
        check("1 a.f90" in clean_bars[-1],
              "the tab label survives repeated Fuss redraws", clean_bars[-1])

        s.send("\x10", 0.7)  # ctrl-p
        s.send("Preferences: Color Theme", 0.8)
        s.send("\r", 1.0)
        check("Color Theme" in s.text(), "the live theme picker opens")
        picker_cells = s.cells("Color Theme", next(
            (i + 1 for i, row in enumerate(s.screen.display) if "Color Theme" in row), 1))
        check(picker_cells and any(cell.bg != "default" for cell in picker_cells),
              "the picker uses themed panel chrome")
        before = [(cell.fg, cell.bg, cell.reverse) for cell in picker_cells]
        s.send("\x1b[B", 0.6)
        row = next((i + 1 for i, value in enumerate(s.screen.display)
                    if "Color Theme" in value), 1)
        preview_cells = s.cells("Color Theme", row)
        after = [(cell.fg, cell.bg, cell.reverse) for cell in preview_cells]
        check(before != after, "moving in the picker previews another theme")
        s.send("\x1b", 0.8)
        check("Color Theme" not in s.text(), "escape restores the editor")

        s.send("\x1bt", 1.5)  # alt-t: integrated terminal
        terminal_row = next((i for i, row in enumerate(s.screen.display)
                             if "TERMINAL" in row), -1)
        check(terminal_row >= 0, "the integrated terminal opens")
        check(terminal_row >= 0 and
              s.screen.buffer[terminal_row][COLS - 1].data == "─",
              "the terminal border reaches the final screen column")
        terminal_surface = s.screen.buffer[ROWS - 2][COLS // 2]
        check(terminal_surface.bg == "000000",
              "the integrated terminal uses a stark black surface",
              terminal_surface.bg)
    finally:
        s.close()

    if failures:
        print(f"integration_theme_render: FAILED ({len(failures)}: " +
              ", ".join(failures) + ")")
        return 1
    print("integration_theme_render: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
