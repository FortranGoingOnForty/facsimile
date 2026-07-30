#!/usr/bin/env python3
"""
Integration test: what the file tree shows, in a git repo and out of one.

The tree in a git repo used to be built from `git ls-files`, so a directory
node existed only as a path COMPONENT of some file git had named. A hidden
directory that was gitignored, or simply empty, therefore had no node at all --
and the '.' toggle could not reveal something that was never built. Hidden
FILES worked, because a dotfile is itself an entry in that list, which is what
made the behaviour look arbitrary.

Both modes now read directories and ask git only about status and ignores.

The second thing fixed here: '.' meant opposite things in the two modes. Outside
a repo dotfiles started hidden and '.' revealed them; inside one they started
visible and '.' hid them.

Usage: python3 test/integration_tree_hidden.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte, and git
"""

import os
import shutil
import subprocess
import sys
import tempfile
import time

try:
    import pexpect
    import pyte
except ImportError as e:
    print(f"SKIP: missing dependency ({e}); pip3 install pexpect pyte")
    sys.exit(0)

if shutil.which("git") is None:
    print("SKIP: git not on PATH")
    sys.exit(0)

ROWS, COLS = 34, 110
failures = []


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if not ok:
        failures.append(name)
        if detail:
            for ln in str(detail).split("\n")[:16]:
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


def sh(*a, cwd):
    subprocess.run(a, cwd=cwd, capture_output=True)


def git_init(d):
    sh("git", "init", "-q", cwd=d)
    sh("git", "config", "user.email", "t@t", cwd=d)
    sh("git", "config", "user.name", "t", cwd=d)


class Tree:
    """Open a workspace with the file tree showing."""

    def __init__(self, binary, workdir):
        self.home = tempfile.mkdtemp(prefix="fac_th_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [os.path.join(workdir, "visible.txt")],
                                   dimensions=(ROWS, COLS), env=env, cwd=workdir)
        self.drain(2.0)
        self.child.send("\x02")            # ctrl-b
        self.drain(1.5)

    def drain(self, w=0.6):
        end = time.time() + w
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                pass
            except pexpect.EOF:
                break

    def text(self):
        return "\n".join(self.screen.display)

    def toggle_hidden(self):
        self.child.send(".")
        self.drain(1.2)

    def close(self):
        try:
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)


def fixture(kind):
    """A workspace with a hidden directory in one of several states."""
    d = tempfile.mkdtemp(prefix="fac_thf_")
    open(os.path.join(d, "visible.txt"), "w").write("hi\n")
    open(os.path.join(d, ".hiddenfile"), "w").write("hi\n")
    os.makedirs(os.path.join(d, ".docs"), exist_ok=True)
    if "empty" not in kind:
        open(os.path.join(d, ".docs", "note.md"), "w").write("note\n")

    if kind.startswith("git"):
        git_init(d)
        if kind == "git-ignored":
            open(os.path.join(d, ".gitignore"), "w").write(".docs/\n")
        sh("git", "add", "-A", cwd=d)
        sh("git", "commit", "-qm", "x", cwd=d)
    return d


# The reported bug. Every one of these used to fail in a git repo.
def test_a_hidden_directory_can_always_be_revealed(binary):
    for kind in ("no-git", "no-git-empty", "git-tracked", "git-ignored",
                 "git-empty-dir"):
        d = fixture(kind)
        t = Tree(binary, d)
        try:
            check(".docs" not in t.text(),
                  f"[{kind}] the hidden directory starts hidden", t.text())
            t.toggle_hidden()
            check(".docs" in t.text(),
                  f"[{kind}] and '.' reveals it", t.text())
            check(".hiddenfile" in t.text(),
                  f"[{kind}] along with hidden files", t.text())
        finally:
            t.close()
            shutil.rmtree(d, ignore_errors=True)


# The toggle used to mean opposite things in the two modes.
def test_the_toggle_reads_the_same_way_in_both_modes(binary):
    for kind in ("no-git", "git-tracked"):
        d = fixture(kind)
        t = Tree(binary, d)
        try:
            check(".hiddenfile" not in t.text(),
                  f"[{kind}] dotfiles start hidden", t.text())
            t.toggle_hidden()
            check(".hiddenfile" in t.text(),
                  f"[{kind}] the first '.' reveals", t.text())
            t.toggle_hidden()
            check(".hiddenfile" not in t.text(),
                  f"[{kind}] and the second hides again", t.text())
        finally:
            t.close()
            shutil.rmtree(d, ignore_errors=True)


def git_project():
    """A repo with nested changes and ignored build output."""
    d = tempfile.mkdtemp(prefix="fac_thg_")
    os.makedirs(os.path.join(d, "src", "deep"), exist_ok=True)
    os.makedirs(os.path.join(d, "build"), exist_ok=True)
    open(os.path.join(d, "visible.txt"), "w").write("readme\n")
    open(os.path.join(d, "src", "tracked.c"), "w").write("int a;\n")
    open(os.path.join(d, "src", "deep", "buried.c"), "w").write("int b;\n")
    open(os.path.join(d, ".gitignore"), "w").write("build/\n")
    open(os.path.join(d, "build", "artifact.o"), "w").write("junk\n")
    git_init(d)
    sh("git", "add", "-A", cwd=d)
    sh("git", "commit", "-qm", "init", cwd=d)
    # a change buried two levels down, and an untracked file
    open(os.path.join(d, "src", "deep", "buried.c"), "a").write("int c;\n")
    open(os.path.join(d, "src", "fresh.c"), "w").write("new\n")
    return d


# Reading directories must not cost the things git was being used for.
def test_git_status_and_ignores_still_work(binary):
    d = git_project()
    t = Tree(binary, d)
    try:
        body = t.text()
        check("artifact.o" not in body,
              "ignored build output stays out of the way", body)
        check("fresh.c" in body, "an untracked file is listed", body)
        check(any(m in body for m in ("✗", "↑")),
              "and git status markers are drawn", body)

        # collapse_tree_smart used to open the folders holding changes by
        # walking the whole tree. The lazy tree walks git's dirty paths
        # instead, which has to reach the same places.
        check("buried.c" in body,
              "a change two directories down is opened up to", body)

        t.toggle_hidden()
        check("build" in t.text(),
              "and '.' reveals the ignored directory", t.text())
    finally:
        t.close()
        shutil.rmtree(d, ignore_errors=True)


def main():
    binary = find_binary()
    for fn in (test_a_hidden_directory_can_always_be_revealed,
               test_the_toggle_reads_the_same_way_in_both_modes,
               test_git_status_and_ignores_still_work):
        try:
            fn(binary)
        except Exception as exc:                        # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    if failures:
        print(f"\nintegration_tree_hidden: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("\nintegration_tree_hidden: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
