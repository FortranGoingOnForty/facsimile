#!/usr/bin/env python3
"""
Integration test: Escape closes the integrated terminal panel, but only from
a bare shell prompt.

The distinction matters because Escape is meaningful to the shell (vi-mode,
dismissing a completion) and to anything full-screen running inside it. The
panel decides by watching where the prompt left the caret: output that moves
to a new line marks the prompt, and character echo deliberately does not — so
a caret sitting right of the mark means the user has text on the line.

Usage: python3 test/integration_terminal_panel.py [path-to-fac-binary]
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

ROWS, COLS = 30, 100
ALT_T = "\x1bt"          # toggles the terminal panel
ESC = "\x1b"
BACKSPACE = "\x7f"

failures = []


def check(ok, name, screen=None):
    print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if not ok:
        failures.append(name)
        if screen is not None:
            for r in screen.display[-12:]:
                if r.strip():
                    print("        " + r.rstrip()[:90])


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
    def __init__(self, binary, wait=3.0):
        self.home = tempfile.mkdtemp(prefix="fac_term_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.target = os.path.join(self.home, "t.txt")
        with open(self.target, "w") as f:
            f.write("editor text\n")
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home,
               "PS1": "$ ", "SHELL": "/bin/sh"}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.home)
        self.drain(wait)

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

    def panel_open(self):
        # The panel draws a separator bar labelled TERMINAL whenever visible
        return any("TERMINAL" in r for r in self.screen.display)

    def saved_file(self):
        with open(self.target) as f:
            return f.read()

    def display(self):
        return "\n".join(r.rstrip() for r in self.screen.display)

    def close(self):
        try:
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)


def test_escape_closes_on_bare_prompt(binary):
    s = Session(binary)
    try:
        s.send(ALT_T, 2.0)
        opened = s.panel_open()
        s.send(ESC, 1.0)
        check(opened and not s.panel_open(),
              "escape on a bare prompt closes the panel", s.screen)
    finally:
        s.close()


def test_escape_kept_by_shell_when_line_has_text(binary):
    s = Session(binary)
    try:
        s.send(ALT_T, 2.0)
        s.send("echo hi", 0.9)
        s.send(ESC, 1.0)
        check(s.panel_open(),
              "escape with text typed leaves the panel open", s.screen)
    finally:
        s.close()


def test_escape_closes_after_the_line_is_cleared(binary):
    s = Session(binary)
    try:
        s.send(ALT_T, 2.0)
        s.send("echo hi", 0.9)
        s.send(BACKSPACE * 7, 0.9)
        s.send(ESC, 1.0)
        check(not s.panel_open(),
              "escape closes again once the line is cleared", s.screen)
    finally:
        s.close()


def test_escape_closes_on_the_prompt_after_a_command(binary):
    s = Session(binary)
    try:
        s.send(ALT_T, 2.0)
        s.send("echo hello\r", 1.6)
        s.send(ESC, 1.0)
        check(not s.panel_open(),
              "escape on the prompt after running a command closes", s.screen)
    finally:
        s.close()


def test_escape_belongs_to_a_fullscreen_program(binary):
    """A program on the alternate screen owns Escape outright."""
    s = Session(binary)
    try:
        s.send(ALT_T, 2.0)
        s.send("printf '\\033[?1049h'; sleep 6\r", 2.0)
        s.send(ESC, 1.0)
        check(s.panel_open(),
              "escape inside a full-screen program leaves the panel open",
              s.screen)
    finally:
        s.close()


# --- A dead shell must not turn the panel into a passthrough -----------------
#
# The child can exit while the panel is still on screen: `exit`, Ctrl-D, or an
# idle timeout such as bash's TMOUT. The panel looks identical afterwards, so
# every keystroke the user aims at the shell was being executed by the editor
# instead -- typing text straight into the file, silently.

def test_dead_shell_does_not_leak_keys_into_the_document(binary):
    s = Session(binary)
    try:
        s.send(ALT_T, 2.0)
        s.send("exit\r", 1.5)
        check("PROCESS EXITED" in s.display(),
              "a shell that exits says so instead of looking alive", s.screen)
        s.send("XXXX", 1.0)
        check("[modified]" not in s.display(),
              "typing at a dead shell does not modify the document", s.screen)
        check(s.saved_file() == "editor text\n",
              "the file on disk is untouched", repr(s.saved_file()))
    finally:
        s.close()


def test_enter_respawns_a_dead_shell(binary):
    s = Session(binary)
    try:
        s.send(ALT_T, 2.0)
        s.send("exit\r", 1.5)
        s.send("\r", 2.0)
        s.send("echo REVIVED\r", 1.5)
        check("REVIVED" in s.display() and "PROCESS EXITED" not in s.display(),
              "Enter replaces a dead shell with a working one", s.screen)
    finally:
        s.close()


def test_escape_closes_a_dead_panel(binary):
    s = Session(binary)
    try:
        s.send(ALT_T, 2.0)
        s.send("exit\r", 1.5)
        s.send(ESC, 1.2)
        check(not s.panel_open() and "PROCESS EXITED" not in s.display(),
              "escape closes the panel once the shell has exited", s.screen)
    finally:
        s.close()


def test_unmapped_chord_does_not_reach_the_editor(binary):
    """A live shell that does not consume a key must not hand it to the editor:
    ctrl-shift-k would kill a line of the file while the user is looking at a
    shell prompt."""
    s = Session(binary)
    try:
        s.send(ALT_T, 2.0)
        s.send("echo alive\r", 1.2)
        s.send("\x1b[107;6u", 1.0)          # ctrl-shift-k, unmapped in the panel
        check("[modified]" not in s.display(),
              "an unmapped chord does not edit the document", s.screen)
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_escape_closes_on_bare_prompt,
               test_escape_kept_by_shell_when_line_has_text,
               test_escape_closes_after_the_line_is_cleared,
               test_escape_closes_on_the_prompt_after_a_command,
               test_escape_belongs_to_a_fullscreen_program,
               test_dead_shell_does_not_leak_keys_into_the_document,
               test_enter_respawns_a_dead_shell,
               test_escape_closes_a_dead_panel,
               test_unmapped_chord_does_not_reach_the_editor):
        try:
            fn(binary)
        except Exception as exc:            # noqa: BLE001 - report and keep going
            check(False, f"{fn.__name__}: {exc!r}")

    if failures:
        print(f"\nintegration_terminal_panel: FAILED ({len(failures)}: "
              + ", ".join(failures) + ")")
        return 1
    print("\nintegration_terminal_panel: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
