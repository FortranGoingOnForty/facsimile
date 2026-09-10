#!/usr/bin/env python3
"""
Integration test: `fac` run in the integrated terminal opens in THIS session.

Typing `fac foo.c` at the panel's prompt used to start a whole second editor
inside a pane of the first one -- a nested session with its own tab bar,
status line and keybindings, competing for the same keystrokes. It now hands
the path to the editor that owns the terminal, the way `code` does, and exits.

A directory is the interesting case, because it means two different things
depending on where you type it. From a normal terminal `fac dir/` opens a new
workspace, and that is unchanged. From the panel there is already a workspace,
so it opens the directory as a TAB GROUP in it.

The load-bearing assertions are the negative ones. "The file opened" is also
true of a nested editor that opened it in its own inner session, so each case
additionally checks that the outer editor's own tab bar changed and that the
shell got its prompt back.

Usage: python3 test/integration_session_ipc.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import os
import re
import shutil
import sys
import tempfile
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

try:
    import pexpect
    import pyte
except ImportError as e:
    print(f"SKIP: missing dependency ({e}); pip3 install pexpect pyte")
    sys.exit(0)

ROWS, COLS = 32, 100
ALT_T = "\x1bt"

failures = []


def check(name, cond, detail=""):
    if cond:
        print(f"  PASS  {name}")
    else:
        print(f"  FAIL  {name}{('  -- ' + detail) if detail else ''}")
        failures.append(name)


def find_binary(argv):
    if len(argv) > 1:
        return os.path.abspath(argv[1])
    here = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    for c in (os.path.join(here, "fac"), os.path.join(here, "build", "fac")):
        if os.path.exists(c):
            return c
    print("SKIP: no fac binary found; run make first")
    sys.exit(0)


class Session:
    """An editor open on a workspace, with a terminal panel to type into."""

    def __init__(self, binary):
        self.binary = binary
        self.home = tempfile.mkdtemp(prefix="fac_ipc_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')

        self.work = tempfile.mkdtemp(prefix="fac_ipc_work_")
        for name in ("opened.c", "later.c"):
            with open(os.path.join(self.work, name), "w") as f:
                f.write("int %s;\n" % name.split(".")[0])
        os.makedirs(os.path.join(self.work, "chapter"))
        for name in ("one.c", "two.c", "three.c"):
            with open(os.path.join(self.work, "chapter", name), "w") as f:
                f.write("int %s;\n" % name.split(".")[0])
        os.makedirs(os.path.join(self.work, "chapter", "nested"))
        with open(os.path.join(self.work, "chapter", "nested", "deep.c"), "w") as f:
            f.write("int deep;\n")
        os.makedirs(os.path.join(self.work, "chapter2"))
        with open(os.path.join(self.work, "chapter2", "outside.c"), "w") as f:
            f.write("int outside;\n")

        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home,
               "PS1": "$ ", "SHELL": "/bin/sh"}
        env.pop("XDG_CONFIG_HOME", None)
        # Must not leak in from whatever ran this test, or the editor under
        # test would think IT is a client and forward its own argument away.
        env.pop("FAC_SESSION", None)

        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [os.path.join(self.work, "opened.c")],
                                   dimensions=(ROWS, COLS), env=env,
                                   cwd=self.work)
        self.drain(3.0)

    def drain(self, w=0.5):
        end = time.time() + w
        while time.time() < end:
            try:
                data = self.child.read_nonblocking(65536, 0.1)
                self.stream.feed(data.decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                pass
            except pexpect.EOF:
                break

    def send(self, data, w=0.6):
        self.child.send(data)
        self.drain(w)

    def run_in_panel(self, cmdline, w=3.0):
        self.send(cmdline + "\r", w)

    def display(self):
        return list(self.screen.display)

    def tab_bar(self):
        return self.screen.display[0]

    def panel_open(self):
        # Case-insensitive on purpose: the separator reads " TERMINAL " while
        # the panel has focus and " terminal " while it does not, and this
        # feature deliberately takes focus away from a panel that stays open.
        return any("terminal" in r.lower() for r in self.screen.display)

    def panel_focused(self):
        return any(" TERMINAL " in r for r in self.screen.display)

    def text(self):
        return "\n".join(self.screen.display)

    def find(self, text):
        for y, row in enumerate(self.screen.display):
            col = row.find(text)
            if col >= 0:
                return y + 1, col + 1
        return None

    def click(self, row, col, button=0, w=0.8):
        self.child.send(f"\x1b[<{button};{col};{row}M")
        self.drain(0.2)
        self.child.send(f"\x1b[<{button};{col};{row}m")
        self.drain(w)

    def group_count(self):
        match = re.search(r"chapter/ \((\d+)\)", self.tab_bar())
        return int(match.group(1)) if match else 0

    def make_chapter_group(self):
        """Create a two-member group through the same IPC route under test."""
        self.send(ALT_T, 1.2)
        self.run_in_panel(f"{self.binary} chapter", 2.0)
        if "New Tab Group" not in self.text():
            return False
        for name in ("one.c", "two.c"):
            hit = self.find("[ ] " + name)
            if not hit:
                return False
            self.click(hit[0], hit[1] + 2)
        self.send("\r", 2.0)
        return self.group_count() == 2

    def close(self):
        try:
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def spool_dirs():
    """Session spool directories currently on disk."""
    root = os.environ.get("TMPDIR", "/tmp")
    try:
        return {d for d in os.listdir(root) if d.startswith("fac-session-")}
    except OSError:
        return set()


def test_a_file_opens_in_this_session(binary):
    print("\nA file typed at the panel prompt opens as a tab here")
    s = Session(binary)
    try:
        s.send(ALT_T, 1.5)
        check("the terminal panel is open", s.panel_open())

        before = s.tab_bar()
        check("later.c is not open yet", "later.c" not in before,
              repr(before))

        s.run_in_panel(f"{binary} later.c")
        after = s.tab_bar()

        check("the outer tab bar now lists later.c", "later.c" in after,
              repr(after))
        # The client says so on its way out. Without this, a nested editor
        # that happened to open the file would satisfy the assertion above.
        check("the shell reported forwarding it",
              "Opening in facsimile" in s.text(), repr(s.text()[-400:]))
        check("no second editor took over the panel",
              s.panel_open(), "the panel lost its separator bar")
        check("focus moved to the file that was opened",
              not s.panel_focused(),
              "still typing at the shell while looking at a new file")
    finally:
        s.close()


def test_a_directory_opens_as_a_tab_group(binary):
    print("\nA directory typed at the panel prompt opens the group dialog")
    s = Session(binary)
    try:
        s.send(ALT_T, 1.5)
        check("the terminal panel is open", s.panel_open())

        s.run_in_panel(f"{binary} chapter")
        body = s.text()

        check("the group dialog appeared", "New Tab Group" in body,
              repr(body[:400]))
        check("it is the chapter directory", "chapter" in body)
        check("the panel is still open underneath", s.panel_open())
        # The one that matters. The request arrives while the user is typing
        # in the shell, so without handing the keyboard over the dialog gets
        # no keys at all and Esc goes to the shell instead -- a modal on
        # screen that cannot be answered or dismissed.
        check("but the keyboard has left the panel", not s.panel_focused(),
              "the panel still holds focus, so the dialog is unreachable")

        # Proof of the above: Esc must reach the DIALOG.
        s.send("\x1b", 1.2)
        check("esc dismissed the dialog", "New Tab Group" not in s.text())
        check("and did not close the panel with it", s.panel_open())
        check("the original file is still open", "opened.c" in s.tab_bar(),
              repr(s.tab_bar()))
        check("the shell reported forwarding a group",
              "Opening group in facsimile" in s.text(),
              repr(s.text()[-400:]))
    finally:
        s.close()


def test_matching_files_join_their_tab_group(binary):
    print("\nA file under a tab-group root joins that group")
    s = Session(binary)
    try:
        check("the chapter group was created", s.make_chapter_group(),
              s.tab_bar())

        # The completed dialog lands inside the group. Alt-T re-focuses the
        # still-visible terminal, where a relative path is resolved by the
        # short-lived client before it reaches the editor.
        s.send(ALT_T, 0.8)
        s.run_in_panel(f"{binary} chapter/three.c")
        check("a direct child joins the group", s.group_count() == 3,
              s.tab_bar())
        check("the new member is visible on the group row",
              "three.c" in s.screen.display[1],
              repr(s.screen.display[1]))

        s.send(ALT_T, 0.8)
        s.run_in_panel(f"{binary} chapter/nested/deep.c")
        check("a nested child joins the same group", s.group_count() == 4,
              s.tab_bar())
        check("the nested member is visible on the group row",
              "deep.c" in s.screen.display[1],
              repr(s.screen.display[1]))
    finally:
        s.close()


def test_an_adjacent_directory_stays_outside_the_group(binary):
    print("\nA similarly named adjacent directory does not match the group")
    s = Session(binary)
    try:
        check("the chapter group was created", s.make_chapter_group(),
              s.tab_bar())
        s.send(ALT_T, 0.8)
        s.run_in_panel(f"{binary} chapter2/outside.c")

        check("the group still has only its original members",
              s.group_count() == 2, s.tab_bar())
        check("the file opened as a loose tab", "outside.c" in s.tab_bar(),
              repr(s.tab_bar()))
    finally:
        s.close()


def test_a_normal_terminal_still_opens_a_workspace(binary):
    print("\nOutside the panel a directory still opens a whole workspace")
    home = tempfile.mkdtemp(prefix="fac_ipc_home_")
    os.makedirs(os.path.join(home, ".config", "fac"))
    with open(os.path.join(home, ".config", "fac", "state.json"), "w") as f:
        f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                ' "version": "1.0"}\n')
    work = tempfile.mkdtemp(prefix="fac_ipc_work_")
    os.makedirs(os.path.join(work, "chapter"))
    with open(os.path.join(work, "chapter", "one.c"), "w") as f:
        f.write("int one;\n")

    env = {**os.environ, "TERM": "xterm-256color", "HOME": home}
    env.pop("XDG_CONFIG_HOME", None)
    env.pop("FAC_SESSION", None)

    screen = pyte.Screen(COLS, ROWS)
    stream = pyte.Stream(screen)
    child = pexpect.spawn(binary, [os.path.join(work, "chapter")],
                          dimensions=(ROWS, COLS), env=env, cwd=work)
    try:
        end = time.time() + 3.0
        while time.time() < end:
            try:
                stream.feed(child.read_nonblocking(65536, 0.1)
                            .decode("utf-8", "replace"))
            except Exception:
                time.sleep(0.05)
        body = "\n".join(screen.display)
        check("it opened an editor, not a group dialog",
              "New Tab Group" not in body, repr(body[:400]))
        check("and it did not try to forward anywhere",
              "Opening group in facsimile" not in body)
    finally:
        try:
            child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(home, ignore_errors=True)
        shutil.rmtree(work, ignore_errors=True)


def test_the_spool_is_cleaned_up(binary):
    print("\nThe session spool does not outlive the session")
    before = spool_dirs()
    s = Session(binary)
    try:
        s.send(ALT_T, 1.5)
        during = spool_dirs()
        check("a spool exists while the session runs",
              len(during - before) >= 1,
              f"before={len(before)} during={len(during)}")
    finally:
        s.close()
    # Quitting is a SIGKILL here, so the directory may survive; what must not
    # happen is an unbounded pile-up under a name we cannot attribute.
    leaked = spool_dirs() - before
    for d in leaked:
        shutil.rmtree(os.path.join(os.environ.get("TMPDIR", "/tmp"), d),
                      ignore_errors=True)
    check("at most one spool per session", len(leaked) <= 1,
          f"leaked={sorted(leaked)}")


def main():
    binary = find_binary(sys.argv)
    print(f"Testing {binary}")
    test_a_file_opens_in_this_session(binary)
    test_a_directory_opens_as_a_tab_group(binary)
    test_matching_files_join_their_tab_group(binary)
    test_an_adjacent_directory_stays_outside_the_group(binary)
    test_a_normal_terminal_still_opens_a_workspace(binary)
    test_the_spool_is_cleaned_up(binary)

    print()
    if failures:
        print(f"FAILED: {len(failures)}")
        for f in failures:
            print(f"  - {f}")
        return 1
    print("All session-IPC tests passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
