#!/usr/bin/env python3
"""
A pane keeps its own file when a sibling pane closes.

Panes in one tab need not hold the same file: alt-v/alt-s on a file-tree row
splits a *different* file into the current tab. Closing a pane then went
through two bugs at once -- the handler reloaded the working buffer from the
tab-level buffer (the closed pane's text), and the main loop's write-back
guard compared pane *indices*, which closing the first of two leaves at 1.
The survivor kept its name and filled with the closed pane's text, and the
next Ctrl-S wrote that text over the survivor's file on disk.

Usage: python3 test/integration_pane_close_identity.py [path-to-fac-binary]
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
    def __init__(self, binary, path, home, cwd):
        env = {**os.environ, "TERM": "xterm-256color", "HOME": home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.proc = pexpect.spawn(binary, [path], dimensions=(ROWS, COLS),
                                  env=env, cwd=cwd, timeout=20)
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

    def status(self):
        return self.screen.display[ROWS - 1].strip()

    def in_tree(self):
        return any("│" in self.screen.display[y] for y in range(1, ROWS - 1))

    def tree_selection(self):
        """Label of the reverse-video row in the tree pane, skipping the
        single-glyph git status markers which are also drawn reversed."""
        for y in range(1, ROWS - 1):
            row = self.screen.buffer[y]
            cells = "".join(row[x].data if row[x].reverse else "" for x in range(30)).strip()
            if len(cells) > 1 and cells not in ("✗", "↑"):
                return cells
        return None

    def first_text_line(self):
        """Text of the topmost drawn document line, gutter stripped."""
        for y in range(2, ROWS):
            line = self.screen.display[y - 1]
            if "│" in line:
                line = line.split("│", 1)[1]
            m = re.match(r"\s*\d+\s(.*)$", line)
            if m and m.group(1).strip():
                return " ".join(m.group(1).split())
        return ""

    def close(self):
        try:
            self.proc.close(force=True)
        except Exception:
            pass


def run(binary):
    root = tempfile.mkdtemp(prefix="fac_pci_")
    home = os.path.join(root, "home")
    os.makedirs(os.path.join(home, ".config", "fac"))
    with open(os.path.join(home, ".config", "fac", "state.json"), "w") as fh:
        fh.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                 ' "version": "1.0"}\n')
    work = os.path.join(home, "w")
    os.makedirs(work)
    original = {}
    for name, tag in (("alpha.txt", "AAA"), ("beta.txt", "BBB")):
        text = "".join(f"{tag} line {i} of {name}\n" for i in range(1, 201))
        with open(os.path.join(work, name), "w") as fh:
            fh.write(text)
        original[name] = text

    failures = 0
    s = Session(binary, os.path.join(work, "alpha.txt"), home, work)
    try:
        # Split beta.txt into alpha.txt's tab, from the file tree.
        s.send("\x02", 1.2)
        for _ in range(20):
            sel = s.tree_selection()
            if sel and sel.split()[0] == "beta.txt":
                break
            s.send("\x1b[B", 0.2)
        else:
            print("SKIP: could not select beta.txt in the tree")
            return 0
        s.send("\x1bv", 2.0)                     # alt-v: open in a vertical split
        if s.in_tree():
            s.send("\x02", 0.8)
        if "beta.txt" not in s.status():
            print(f"SKIP: alt-v did not open beta.txt in a split (status: {s.status()[:60]})")
            return 0
        print("ok   beta.txt split into alpha.txt's tab")

        s.send("\x1bh", 0.9)                     # alt-h: focus the first pane
        if "alpha.txt" not in s.status():
            print(f"SKIP: alt-h did not focus the alpha.txt pane (status: {s.status()[:60]})")
            return 0
        print("ok   first pane (alpha.txt) focused")

        s.send("\x1bq", 1.5)                     # alt-q: close it

        if "beta.txt" not in s.status():
            print(f"FAIL survivor should be beta.txt; status: {s.status()[:70]}")
            failures += 1
        else:
            print("ok   survivor names beta.txt")

        text = s.first_text_line()
        if not text.startswith("BBB"):
            print(f"FAIL survivor shows {text!r}, which is not beta.txt's text")
            failures += 1
        else:
            print("ok   survivor shows beta.txt's text")

        # The decisive one: saving must not write the closed pane's text over
        # the surviving pane's file. Split once first -- that resyncs the
        # editor's filename from the pane (beta.txt) while the working buffer
        # still holds the closed pane's text, which is what turns the in-memory
        # mixup into data loss on disk.
        s.send("\x1bv", 1.5)
        s.send("\x13", 1.5)
        for name in ("alpha.txt", "beta.txt"):
            with open(os.path.join(work, name)) as fh:
                now = fh.read()
            if now != original[name]:
                first = now.split("\n", 1)[0]
                print(f"FAIL Ctrl-S changed {name} on disk; it now starts {first!r}")
                failures += 1
            else:
                print(f"ok   {name} unchanged on disk after Ctrl-S")
    finally:
        s.close()
        shutil.rmtree(root, ignore_errors=True)

    print("integration_pane_close_identity: "
          + ("ALL PASSED" if failures == 0 else f"{failures} FAILURE(S)"))
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(run(find_binary()))
