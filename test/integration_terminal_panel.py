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

import codecs
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


def check(ok, name, detail=None):
    """Report a check. `detail` may be a pyte Screen or any printable value.

    It used to require a Screen and call .display on it, so passing a string
    raised AttributeError -- but only when the check FAILED, since the detail
    is untouched on success. That turned a legible assertion failure into an
    exception, and it hid in CI while every local run was green."""
    print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if not ok:
        failures.append(name)
        if detail is None:
            return
        lines = getattr(detail, "display", None)
        if lines is None:
            lines = str(detail).split("\n")
        else:
            lines = list(lines)[-12:]
        for r in lines[:12]:
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
        self.decoder = codecs.getincrementaldecoder("utf-8")("replace")
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.home)
        self.drain(wait)

    def drain(self, w=0.5):
        end = time.time() + w
        while time.time() < end:
            try:
                data = self.child.read_nonblocking(65536, 0.1)
                self.stream.feed(self.decoder.decode(data))
            except pexpect.TIMEOUT:
                pass
            except pexpect.EOF:
                break

    def send(self, data, w=0.6):
        self.child.send(data)
        self.drain(w)

    def drag(self, row, col_from, col_to, release_row=None):
        """SGR press, motion, release. release_row defaults to the drag row;
        pass a different one to end the drag outside the panel."""
        self.child.send(f"\x1b[<0;{col_from};{row}M")
        self.drain(0.3)
        for c in range(col_from + 1, col_to + 1):
            self.child.send(f"\x1b[<32;{c};{row}M")
        self.drain(0.4)
        rr = row if release_row is None else release_row
        self.child.send(f"\x1b[<0;{col_to};{rr}m")
        self.drain(0.9)

    def output_row(self, text):
        for i, r in enumerate(self.screen.display):
            if r.strip().startswith(text):
                return i + 1
        return 0

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


# --- Copying text out of the terminal ---------------------------------------
#
# The copy fires on mouse release. The router only forwarded mouse events whose
# row fell inside the panel, so a drag that ended above it -- which is what
# selecting the last few lines looks like -- never delivered its release. The
# selection stayed drawn, so it read as "copy is broken" rather than "the
# release went somewhere else".

def _select_and_paste_back(binary, release_row_delta):
    """Select terminal text, then paste into the document and return the file."""
    s = Session(binary)
    try:
        s.send(ALT_T, 2.0)
        s.send("echo SELECTMEPLEASE\r", 1.2)
        r = s.output_row("SELECTMEPLEASE")
        if r == 0:
            return None
        s.drag(r, 1, 16, release_row=r + release_row_delta)
        s.send(ALT_T, 0.8)          # close the panel
        s.send("\x16", 1.0)         # ctrl-v into the document
        s.send("\x13", 1.0)         # ctrl-s
        return s.saved_file()
    finally:
        s.close()


def test_selection_copies_on_release(binary):
    got = _select_and_paste_back(binary, 0)
    check(got is not None and "SELECTMEPLEASE" in got,
          "a terminal selection reaches the clipboard", repr(got))


def test_selection_copies_when_the_drag_ends_outside(binary):
    got = _select_and_paste_back(binary, -12)
    check(got is not None and "SELECTMEPLEASE" in got,
          "and still copies when the drag ends above the panel", repr(got))


def test_ctrl_shift_c_copies_from_the_keyboard(binary):
    """Ctrl-C is SIGINT to the shell, so the clipboard uses ctrl-shift-c.
    Before this there was no keyboard route to the terminal's text at all."""
    s = Session(binary)
    try:
        s.send(ALT_T, 2.0)
        s.send("echo KEYBOARDCOPY\r", 1.2)
        r = s.output_row("KEYBOARDCOPY")
        if r == 0:
            check(False, "terminal produced output to select")
            return
        # press and drag, but do not release: the keyboard does the copying
        s.child.send(f"\x1b[<0;1;{r}M")
        s.drain(0.3)
        for c in range(2, 14):
            s.child.send(f"\x1b[<32;{c};{r}M")
        s.drain(0.4)
        s.send("\x1b[99;6u", 0.9)          # ctrl-shift-c
        check("Copied" in s.display(),
              "ctrl-shift-c copies the selection and says how much", s.display())
    finally:
        s.close()


def test_ctrl_shift_c_with_no_selection_reports_it(binary):
    s = Session(binary)
    try:
        s.send(ALT_T, 2.0)
        s.send("\x1b[99;6u", 0.9)
        check("Nothing selected" in s.display(),
              "ctrl-shift-c with nothing selected says so rather than staying silent",
              s.display())
    finally:
        s.close()


def test_wheel_scrolls_and_typing_still_works(binary):
    s = Session(binary)
    try:
        s.send(ALT_T, 2.0)
        s.send("for i in 1 2 3 4 5 6 7 8 9; do echo LINE$i; done\r", 1.6)
        prow = 0
        for i, r in enumerate(s.screen.display):
            if "TERMINAL" in r or "terminal" in r:
                prow = i + 1
                break
        s.child.send(f"\x1b[<64;10;{prow + 3}M")
        s.drain(0.7)
        d = s.display()
        check("SCROLLBACK" in d or "LINE1" in d,
              "the wheel over the panel scrolls its scrollback", d)
        s.send("echo AFTERSCROLL\r", 1.2)
        check("AFTERSCROLL" in s.display(),
              "and typing after a scroll still reaches the shell", s.display())
    finally:
        s.close()


def test_ctrl_l_clear_keeps_visible_commands_in_scrollback(binary):
    """Fish implements Ctrl-L as CSI H followed by CSI 2 J. The erase must
    start a fresh viewport without throwing away commands which have not yet
    scrolled off the live grid."""
    s = Session(binary)
    try:
        s.send(ALT_T, 2.0)
        s.send("printf 'KEEP_ALPHA\\nKEEP_BETA\\nKEEP_GAMMA\\n'\r", 1.5)
        before = s.display()
        check("KEEP_ALPHA" in before and "KEEP_GAMMA" in before,
              "the pre-clear command is visible in the live terminal", before)

        # Reproduce the escape sequence emitted by fish's Ctrl-L directly.
        # This is deterministic under the /bin/sh used by the test harness.
        s.send("printf '\\033[H\\033[2J'\r", 1.2)
        prow = 0
        for i, row in enumerate(s.screen.display):
            if "TERMINAL" in row or "terminal" in row:
                prow = i + 1
                break
        for _ in range(3):
            s.child.send(f"\x1b[<64;10;{prow + 3}M")
            s.drain(0.4)

        history = s.display()
        check("SCROLLBACK" in history and "KEEP_ALPHA" in history and
              "KEEP_GAMMA" in history,
              "Ctrl-L preserves the cleared viewport in scrollback", history)
    finally:
        s.close()


# --- Non-ASCII output ---------------------------------------------------------
#
# The grid stored one byte per cell, and the parser put a space in place of any
# UTF-8 lead byte and dropped the continuations. Every non-ASCII character the
# shell printed therefore came out blank: Nerd Font icons from an ls alias, box
# drawing, accented filenames, CJK. Cells now hold codepoints.

# The text under test, as real characters. Written to a file from Python and
# cat'd, rather than built with `printf '\uXXXX'` -- Ubuntu's /bin/sh is dash
# and the escape is not portable, which made this pass locally and fail in CI.
UNICODE_TEXT = "\ue5ff ICON \u2502 caf\u00e9 \u4f60\u597d END"


def write_unicode_file(s):
    path = os.path.join(s.home, "uni.txt")
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(UNICODE_TEXT + "\n")
    return path


def test_non_ascii_renders(binary):
    s = Session(binary)
    try:
        path = write_unicode_file(s)
        s.send(ALT_T, 2.0)
        s.send(f"cat {path}\r", 1.5)
        row = ""
        for r in s.screen.display:
            if "ICON" in r and "printf" not in r:
                row = r.rstrip()
                break
        check("\ue5ff" in row, "a Nerd Font glyph renders instead of a blank", s.screen)
        check("\u2502" in row, "box drawing renders", s.screen)
        check("caf\u00e9" in row, "accented latin renders", s.screen)
        check("\u4f60\u597d" in row, "double-width CJK renders", s.screen)
        check("END" in row, "text after the wide characters is not lost", s.screen)
    finally:
        s.close()


def test_non_ascii_copies_as_bytes(binary):
    """A three-byte glyph must come out of the clipboard as three bytes, and
    the trailing half of a double-width one as nothing."""
    s = Session(binary)
    try:
        path = write_unicode_file(s)
        s.send(ALT_T, 2.0)
        s.send(f"cat {path}\r", 1.5)
        r = s.output_row("\ue5ff")
        if r == 0:
            check(False, "found the unicode output row", s.screen)
            return
        s.drag(r, 1, 30)
        s.send(ALT_T, 0.8)
        s.send("\x16", 1.0)
        s.send("\x13", 1.0)
        got = s.saved_file()
        check("\ue5ff" in got, "the icon survives a copy", s.screen)
        check("\u4f60\u597d" in got, "and so does the CJK pair", s.screen)
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
               test_unmapped_chord_does_not_reach_the_editor,
               test_selection_copies_on_release,
               test_selection_copies_when_the_drag_ends_outside,
               test_ctrl_shift_c_copies_from_the_keyboard,
               test_ctrl_shift_c_with_no_selection_reports_it,
               test_wheel_scrolls_and_typing_still_works,
               test_ctrl_l_clear_keeps_visible_commands_in_scrollback,
               test_non_ascii_renders,
               test_non_ascii_copies_as_bytes):
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
