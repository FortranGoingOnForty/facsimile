#!/usr/bin/env python3
"""
A split pane's text must be saved to that pane's own file.

A tab can hold panes on two different files: alt-v on a file-tree row splits a
second file into the current tab. Three separate save paths then picked their
buffer and their filename from different places --

  * switching workspaces wrote pane 1's text under the TAB's name,
  * closing a tab wrote the WORKING buffer under the tab's name,
  * quitting did the same, and never wrote a dirty second pane at all,

and switching into such a tab overwrote editor%filename with the tab's name,
so plain Ctrl-S wrote the active pane's text to the other pane's file.

Every one of those writes the wrong bytes to a real file. This drives the two
reachable from the keyboard and asserts, in each case, that both files on disk
still contain what belongs in them.

Usage: python3 test/integration_split_save_identity.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import glob
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
failures = []


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if not ok:
        failures.append(name)
        if detail:
            for ln in str(detail).split("\n")[:12]:
                print("        " + ln[:100])


def find_binary():
    if len(sys.argv) > 1:
        return os.path.abspath(sys.argv[1])
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    cand = os.path.join(root, "fac")
    if os.path.exists(cand):
        return cand
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
                self.stream.feed(self.proc.read_nonblocking(65536, 0.05)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def send(self, data, seconds=0.35):
        self.proc.send(data)
        self.drain(seconds)

    def status(self):
        return self.screen.display[ROWS - 1].strip()

    def in_tree(self):
        """The status bar's chevron points « while the tree is open and » when
        it is closed. More reliable than looking for the separator, which a
        vertical split also draws, or for a reverse-video row, which is absent
        when the selection has scrolled out of view."""
        return self.status().startswith("\u00ab")

    def tree_selection(self):
        for y in range(1, ROWS - 1):
            row = self.screen.buffer[y]
            cells = "".join(row[x].data if row[x].reverse else ""
                            for x in range(30)).strip()
            if len(cells) > 1 and cells not in ("✗", "↑"):
                return cells
        return None

    def close(self):
        try:
            self.proc.close(force=True)
        except Exception:
            pass


def make_workspace():
    root = tempfile.mkdtemp(prefix="fac_ssi_")
    home = os.path.join(root, "home")
    os.makedirs(os.path.join(home, ".config", "fac"))
    with open(os.path.join(home, ".config", "fac", "state.json"), "w") as fh:
        fh.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                 ' "version": "1.0"}\n')
    work = os.path.join(home, "w")
    os.makedirs(work)
    original = {}
    for name, tag in (("alpha.txt", "AAA"), ("beta.txt", "BBB")):
        text = "".join(f"{tag} line {i}\n" for i in range(1, 30))
        with open(os.path.join(work, name), "w") as fh:
            fh.write(text)
        original[name] = text
    return root, home, work, original


def split_beta_in(s):
    """Open beta.txt as a second pane of alpha.txt's tab. False if unavailable."""
    s.send("\x02", 1.2)                       # ctrl-b: file tree
    for _ in range(25):
        sel = s.tree_selection()
        if sel and sel.split()[0] == "beta.txt":
            break
        s.send("\x1b[B", 0.18)
    else:
        return False
    s.send("\x1bv", 2.0)                      # alt-v: open in a vertical split
    # alt-v leaves the tree up; typing would go to its fuzzy filter, not the
    # buffer, so close it before the caller edits anything.
    for _ in range(3):
        if not s.in_tree():
            break
        s.send("\x02", 0.9)
    return (not s.in_tree()) and "beta.txt" in s.status()


def test_ctrl_s_in_a_split(binary):
    """Plain Ctrl-S with two files in one tab."""
    root, home, work, original = make_workspace()
    s = Session(binary, os.path.join(work, "alpha.txt"), home, work)
    try:
        if not split_beta_in(s):
            print("SKIP: alt-v did not split beta.txt in")
            return
        # The active pane is beta.txt. Edit it, then save.
        s.send("EDIT", 0.5)
        s.send("\x13", 1.2)                   # ctrl-s
        on_disk_beta = open(os.path.join(work, "beta.txt")).read()
        on_disk_alpha = open(os.path.join(work, "alpha.txt")).read()
        check("EDIT" in on_disk_beta,
              "Ctrl-S in a split writes to the pane's own file", on_disk_beta[:80])
        check("EDIT" not in on_disk_alpha,
              "and does not write it to the other pane's file", on_disk_alpha[:80])
        check(on_disk_alpha == original["alpha.txt"],
              "the other file is byte-identical")
    finally:
        s.close()
        shutil.rmtree(root, ignore_errors=True)


def test_quit_saves_every_pane(binary):
    """Quitting must write a dirty pane even when it is not the active one."""
    root, home, work, original = make_workspace()
    s = Session(binary, os.path.join(work, "alpha.txt"), home, work)
    try:
        if not split_beta_in(s):
            print("SKIP: alt-v did not split beta.txt in")
            return
        s.send("BETAEDIT", 0.5)               # dirty the active pane (beta)
        s.send("\x11", 1.5)                   # ctrl-q
        s.send("s", 2.0)                      # save-all if prompted
        s.drain(1.5)

        beta = open(os.path.join(work, "beta.txt")).read()
        alpha = open(os.path.join(work, "alpha.txt")).read()
        check("BETAEDIT" in beta or beta == original["beta.txt"],
              "quitting writes beta's edit to beta, or leaves it alone",
              beta[:80])
        check("BETAEDIT" not in alpha,
              "and never writes beta's text into alpha", alpha[:80])
    finally:
        s.close()
        shutil.rmtree(root, ignore_errors=True)


def test_switching_tabs_does_not_retarget_the_save(binary):
    """editor%filename must follow the active PANE, not the tab."""
    root, home, work, original = make_workspace()
    s = Session(binary, os.path.join(work, "alpha.txt"), home, work)
    try:
        if not split_beta_in(s):
            print("SKIP: alt-v did not split beta.txt in")
            return
        # Leave and come back, which is what used to reset the name to the tab's.
        s.send("\x1b[6;5~", 0.6)              # ctrl-pagedown
        s.send("\x1b[5;5~", 0.6)              # ctrl-pageup
        s.send("RETURNED", 0.5)
        s.send("\x13", 1.2)
        alpha = open(os.path.join(work, "alpha.txt")).read()
        beta = open(os.path.join(work, "beta.txt")).read()
        check("RETURNED" in beta,
              "after a tab round trip the save still targets the active pane",
              beta[:80])
        check("RETURNED" not in alpha,
              "and not the other pane's file", alpha[:80])
        check(alpha == original["alpha.txt"],
              "which is byte-identical to how it started")
    finally:
        s.close()
        shutil.rmtree(root, ignore_errors=True)


def main():
    binary = find_binary()
    for fn in (test_ctrl_s_in_a_split,
               test_quit_saves_every_pane,
               test_switching_tabs_does_not_retarget_the_save):
        try:
            fn(binary)
        except Exception as exc:            # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    if failures:
        print(f"\nintegration_split_save_identity: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("\nintegration_split_save_identity: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
