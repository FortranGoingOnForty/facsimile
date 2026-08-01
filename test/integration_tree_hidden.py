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


def grey_rows(t):
    """(text, is_grey) per row of the TREE pane, stopping at the separator."""
    out = []
    for y in range(ROWS):
        row = t.screen.buffer[y]
        cols = []
        for x in range(COLS):
            if row[x].data == "│":
                break
            cols.append(x)
        txt = "".join(row[x].data for x in cols).strip()
        if not txt or txt.startswith(("esc/", ".:hide")):
            continue
        fg = {row[x].fg for x in cols if row[x].data.strip()}
        out.append((txt, "brightblack" in fg))
    return out


# Reported: ignored directories were grey only AFTER being expanded once.
# The grey came from all_children_hidden, which is computed when a directory
# is scanned -- so it arrived as a reward for opening the thing it was meant
# to warn about.
def test_an_ignored_directory_is_grey_before_it_is_opened(binary):
    d = tempfile.mkdtemp(prefix="fac_thi_")
    os.makedirs(os.path.join(d, ".docs", "sprints", "00-scaffolding"))
    open(os.path.join(d, "visible.txt"), "w").write("hi\n")
    # '.docs/*' ignores the CONTENTS of .docs without ignoring .docs itself,
    # which is the shape that showed the bug.
    open(os.path.join(d, ".gitignore"), "w").write(".docs/*\n")
    open(os.path.join(d, ".docs", "sprints", "00-scaffolding", "s00.md"), "w").write("# s\n")
    git_init(d)
    sh("git", "add", "-A", cwd=d)
    sh("git", "commit", "-qm", "x", cwd=d)

    t = Tree(binary, d)
    try:
        t.toggle_hidden()
        rows = dict(grey_rows(t))
        docs = [k for k in rows if ".docs" in k]
        check(bool(docs), "the hidden directory is listed", str(list(rows)))

        # Open .docs, then look at sprints/ WITHOUT opening that.
        t.child.send(" ")
        t.drain(1.2)
        rows = grey_rows(t)
        sprint = [(txt, grey) for txt, grey in rows if "sprints" in txt]
        check(bool(sprint), "sprints/ appears once .docs is opened", str(rows))
        if sprint:
            txt, grey = sprint[0]
            check(txt.startswith("+"),
                  "and is still collapsed, never having been expanded", txt)
            check(grey, "yet is already drawn grey", txt)
    finally:
        t.close()
        shutil.rmtree(d, ignore_errors=True)


def selected(t):
    """The highlighted tree row.

    From row 2: the tab bar draws the ACTIVE TAB in reverse video too, and
    scanning from the top matches that on every call.
    """
    for y in range(2, ROWS - 1):
        row = t.screen.buffer[y]
        cells = "".join(row[x].data if row[x].reverse else "" for x in range(40))
        if cells.strip():
            return cells.strip()
    return None


def select(t, name, limit=25):
    """Arrow down until `name` is selected. Up/down move between SIBLINGS."""
    for _ in range(limit):
        sel = selected(t)
        if sel and name in sel:
            return True
        t.child.send("\x1b[B")
        t.drain(0.2)
    sel = selected(t)
    return bool(sel and name in sel)


def sprint_fixture():
    """The reported shape: .docs/sprints/00-.../ with '.docs/*' ignored."""
    d = tempfile.mkdtemp(prefix="fac_thm_")
    os.makedirs(os.path.join(d, ".docs", "sprints", "00-scaffolding"))
    open(os.path.join(d, "visible.txt"), "w").write("hi\n")
    open(os.path.join(d, ".gitignore"), "w").write(".docs/*\n")
    open(os.path.join(d, ".docs", "sprints", "00-scaffolding", "s00.md"), "w").write("# s\n")
    git_init(d)
    sh("git", "add", "-A", cwd=d)
    sh("git", "commit", "-qm", "x", cwd=d)
    return d


# Closing the panel used to free the whole tree, and reopening rebuilt it via
# init_tree_state -- which is intent(out), so it reset the dotfile toggle as
# well as every open directory.
def test_the_tree_remembers_across_close_and_reopen(binary):
    d = sprint_fixture()
    t = Tree(binary, d)
    try:
        t.toggle_hidden()
        if not select(t, ".docs"):
            check(False, "setup: could not select .docs", t.text())
            return
        t.child.send("\x1b[C")            # right: descend into .docs
        t.drain(1.2)
        t.child.send("\x1b[C")            # right: descend into sprints/
        t.drain(1.2)
        check("00-scaffolding" in t.text(),
              "sprints/ is expanded before closing", t.text())

        t.child.send("\x02")              # close
        t.drain(1.0)
        t.child.send("\x02")              # and reopen
        t.drain(1.5)

        check("00-scaffolding" in t.text(),
              "the open directory is still open after reopening", t.text())
        check(".docs" in t.text(),
              "and hidden entries are still shown", t.text())
    finally:
        t.close()
        shutil.rmtree(d, ignore_errors=True)


# No stored list of open directories: the tabs are already persisted, so the
# folders holding them come back on their own.
def test_a_restart_reveals_the_folders_holding_open_files(binary):
    d = sprint_fixture()
    home = tempfile.mkdtemp(prefix="fac_thr_")
    os.makedirs(os.path.join(home, ".config", "fac"))
    with open(os.path.join(home, ".config", "fac", "state.json"), "w") as f:
        f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                ' "version": "1.0"}\n')
    env = {**os.environ, "TERM": "xterm-256color", "HOME": home}
    env.pop("XDG_CONFIG_HOME", None)
    buried = os.path.join(d, ".docs", "sprints", "00-scaffolding", "s00.md")

    def run(args, wait=2.2):
        scr = pyte.Screen(COLS, ROWS)
        st = pyte.Stream(scr)
        ch = pexpect.spawn(binary, args, dimensions=(ROWS, COLS), env=env, cwd=d)

        def drain(w=0.6):
            end = time.time() + w
            while time.time() < end:
                try:
                    st.feed(ch.read_nonblocking(65536, 0.1).decode("utf-8", "replace"))
                except pexpect.TIMEOUT:
                    pass
                except pexpect.EOF:
                    break
        drain(wait)
        return ch, scr, drain

    try:
        # Open the workspace, walk to the buried file through the hidden
        # directories, and open it. Passing the file as a second argument does
        # not put fac in workspace mode, and then nothing is ever saved.
        ch, scr, drain = run([d])
        ch.send("\x02"); drain(1.5)        # tree
        ch.send("."); drain(1.2)           # reveal hidden

        class Shim:                        # so select()/selected() can be reused
            pass
        sh_t = Shim(); sh_t.screen = scr; sh_t.child = ch; sh_t.drain = drain

        if not select(sh_t, ".docs"):
            print("SKIP: could not reach .docs in the tree")
            return
        for _ in range(3):                 # into .docs, sprints, 00-scaffolding
            ch.send("\x1b[C"); drain(1.0)
        if not select(sh_t, "s00.md"):
            print("SKIP: could not reach the buried file")
            return
        ch.send("\r"); drain(1.5)          # open it (leaves fuss mode)
        # Until it exits: ctrl-q closes one surface per press, and after
        # working in the tree there is more than one thing to close. This is
        # long-standing behaviour, not something this change introduced -- the
        # same two presses are needed on the commit before it.
        for _ in range(4):
            ch.send("\x11"); drain(1.8)
            if not ch.isalive():
                break
        try:
            ch.terminate(force=True)
        except Exception:
            pass

        state = os.path.join(d, ".fac", "workspace.json")
        check(os.path.exists(state), "quitting wrote the workspace file", state)
        if not os.path.exists(state):
            return

        # Reopen: the tab comes back, and the tree should open the folders
        # holding it even though they are hidden AND gitignored.
        ch, scr, drain = run([d])
        ch.send("\x02")
        drain(1.8)
        body = "\n".join(scr.display)
        check("00-scaffolding" in body,
              "the folders holding a restored tab are opened up to", body)
        check("sprints" in body,
              "including the hidden ones on the way", body)
        try:
            ch.terminate(force=True)
        except Exception:
            pass
    finally:
        shutil.rmtree(home, ignore_errors=True)
        shutil.rmtree(d, ignore_errors=True)


def nested_project():
    """ch4/ with several subdirectories and ch5/ after it, as reported."""
    d = tempfile.mkdtemp(prefix="fac_nav_")
    layout = {"ch1": [], "ch4": ["printaf", "rpn", "rpn2"], "ch5": ["showme"]}
    for top, subs in layout.items():
        os.makedirs(os.path.join(d, top), exist_ok=True)
        with open(os.path.join(d, top, "top.c"), "w") as f:
            f.write("int x;\n")
        for sub in subs:
            os.makedirs(os.path.join(d, top, sub), exist_ok=True)
            with open(os.path.join(d, top, sub, "main.c"), "w") as f:
                f.write("int y;\n")
    with open(os.path.join(d, "visible.txt"), "w") as f:
        f.write("hi\n")
    return d


def walk_down(t, n):
    """Selections visited by pressing Down n times."""
    seen = []
    for _ in range(n):
        seen.append(selected(t))
        t.child.send("\x1b[B")
        t.drain(0.28)
    seen.append(selected(t))
    return seen


# Reported: the cursor stopped for no visible reason in a deep tree, and only
# recovered after traversing up and out. Down searched for the next SIBLING
# and did nothing when there was not one, so the last child of any expanded
# directory was a dead end.
def test_down_leaves_the_last_child_of_a_directory(binary):
    d = nested_project()
    t = Tree(binary, d)
    try:
        if not select(t, "ch4"):
            check(False, "could not reach ch4/", t.text())
            return
        t.child.send("\x1b[C")             # right: into ch4/
        t.drain(1.0)

        seen = walk_down(t, 6)
        # Trailing repeats are the BOTTOM of the tree, where staying put is
        # right. Only a repeat with different rows still to come is a stall.
        trimmed = list(seen)
        while len(trimmed) > 1 and trimmed[-1] == trimmed[-2]:
            trimmed.pop()
        stuck = [i for i in range(1, len(trimmed)) if trimmed[i] == trimmed[i - 1]]
        check(not stuck,
              "Down never stops on the same row twice inside ch4/",
              " -> ".join(str(x) for x in seen))
        check(any(s and "ch5" in s for s in seen),
              "and walking down out of ch4/ reaches ch5/",
              " -> ".join(str(x) for x in seen))
    finally:
        t.close()
        shutil.rmtree(d, ignore_errors=True)


def test_up_climbs_back_out_the_same_way(binary):
    d = nested_project()
    t = Tree(binary, d)
    try:
        if not select(t, "ch4"):
            check(False, "could not reach ch4/", t.text())
            return
        t.child.send("\x1b[C")
        t.drain(1.0)
        for _ in range(4):                 # down into the subtree
            t.child.send("\x1b[B")
            t.drain(0.28)
        before = selected(t)

        seen = []
        for _ in range(5):
            t.child.send("\x1b[A")
            t.drain(0.28)
            seen.append(selected(t))
        trimmed = list(seen)
        while len(trimmed) > 1 and trimmed[-1] == trimmed[-2]:
            trimmed.pop()
        stuck = [i for i in range(1, len(trimmed)) if trimmed[i] == trimmed[i - 1]]
        check(not stuck, "Up moves every press too",
              f"from {before}: " + " -> ".join(str(x) for x in seen))
        check(any(s and "ch4" in s for s in seen),
              "and climbs back out to ch4/", " -> ".join(str(x) for x in seen))
    finally:
        t.close()
        shutil.rmtree(d, ignore_errors=True)


def test_the_ends_of_the_list_hold(binary):
    """Moving past either end must stay put rather than wrap or run off."""
    d = nested_project()
    t = Tree(binary, d)
    try:
        for _ in range(40):                # far past the bottom
            t.child.send("\x1b[B")
            t.drain(0.06)
        t.drain(0.5)
        last = selected(t)
        check(last is not None, "still on a real row at the bottom", str(last))

        for _ in range(60):                # and far past the top
            t.child.send("\x1b[A")
            t.drain(0.05)
        t.drain(0.5)
        first = selected(t)
        check(first is not None, "and at the top", str(first))
        check(first != last, "the two ends are different rows",
              f"{first!r} vs {last!r}")
    finally:
        t.close()
        shutil.rmtree(d, ignore_errors=True)


def rows(t):
    """Visible tree rows, with '>' marking the selected one."""
    out = []
    for y in range(1, ROWS - 1):
        row = t.screen.buffer[y]
        text = "".join(row[x].data for x in range(34)).rstrip()
        if not text.strip():
            continue
        if text.strip().startswith(("esc/", ".:hide")):
            continue
        mark = ">" if any(row[x].reverse for x in range(34)) else " "
        out.append(mark + text)
    return out


def test_left_collapses_an_open_directory(binary):
    """Reported: Left on an expanded directory did nothing.

    It only ever climbed to the parent, and a top-level directory has no
    parent ROW to climb to -- so the key did nothing at all, which is also
    what made the state afterwards confusing. Collapsing when the thing under
    the cursor is open is what every tree does.
    """
    d = nested_project()
    t = Tree(binary, d)
    try:
        if not select(t, "ch4"):
            check(False, "could not reach ch4/", t.text())
            return
        t.child.send(" ")                 # expand
        t.drain(0.8)
        opened = [r for r in rows(t) if "ch4" in r]
        check(any("-" in r for r in opened), "ch4/ is expanded", str(opened))

        t.child.send("\x1b[D")            # LEFT
        t.drain(0.8)
        after = [r for r in rows(t) if "ch4" in r]
        check(any("+" in r for r in after), "Left collapsed it", str(after))
        check(any(r.startswith(">") and "ch4" in r for r in rows(t)),
              "and the selection stayed on it", str(rows(t)))

        # Space must still work afterwards -- the reported symptom was that it
        # stopped collapsing once Left had been pressed.
        t.child.send(" ")
        t.drain(0.8)
        check(any("-" in r for r in rows(t) if "ch4" in r),
              "space still expands after a Left", str(rows(t)))
        t.child.send(" ")
        t.drain(0.8)
        check(any("+" in r for r in rows(t) if "ch4" in r),
              "and still collapses", str(rows(t)))
    finally:
        t.close()
        shutil.rmtree(d, ignore_errors=True)


def test_left_still_climbs_out_of_a_directory(binary):
    """The other half of the key: from inside, Left goes to the parent."""
    d = nested_project()
    t = Tree(binary, d)
    try:
        if not select(t, "ch4"):
            check(False, "could not reach ch4/", t.text())
            return
        t.child.send("\x1b[C")            # right: into ch4/
        t.drain(1.0)
        t.child.send("\x1b[B")            # down to a child
        t.drain(0.5)
        here = selected(t)
        check(here is not None and "ch4" not in here,
              "sitting on a child of ch4/", str(here))

        t.child.send("\x1b[D")            # LEFT
        t.drain(0.8)
        check(selected(t) is not None and "ch4" in selected(t),
              "Left climbs to the parent", str(selected(t)))
    finally:
        t.close()
        shutil.rmtree(d, ignore_errors=True)


def main():
    binary = find_binary()
    for fn in (test_left_collapses_an_open_directory,
               test_left_still_climbs_out_of_a_directory,
               test_a_hidden_directory_can_always_be_revealed,
               test_the_toggle_reads_the_same_way_in_both_modes,
               test_git_status_and_ignores_still_work,
               test_an_ignored_directory_is_grey_before_it_is_opened,
               test_the_tree_remembers_across_close_and_reopen,
               test_a_restart_reveals_the_folders_holding_open_files,
               test_down_leaves_the_last_child_of_a_directory,
               test_up_climbs_back_out_the_same_way,
               test_the_ends_of_the_list_hold):
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
