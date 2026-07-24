#!/usr/bin/env python3
"""
Integration test: ctrl-/ toggle-comment, the ctrl-? help rebind, and the
CSI-u key path that makes the two chords distinguishable.

ctrl-/ and ctrl-shift-/ are the same byte (0x1F) in the legacy terminal
encoding, so fac negotiates the kitty keyboard protocol at startup and parses
CSI <codepoint>;<mods> u key events. That parsing needs a lookahead over the
CSI parameters, and every legacy CSI sequence shares the same prefix -- so
this also re-checks the ordinary digit-parameter sequences (delete, pageup,
F5, shift+arrow, bracketed paste) that now travel through the pushback path.

Usage: python3 test/integration_comment.py [path-to-fac-binary]
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

ROWS, COLS = 24, 100

failures = []


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}" + (f"  [{detail!r}]" if detail and not ok else ""))
    if not ok:
        failures.append(name)


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
    def __init__(self, binary, content, name="c.py"):
        self.home = tempfile.mkdtemp(prefix="fac_comment_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.target = os.path.join(self.home, name)
        with open(self.target, "w") as f:
            f.write(content)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.raw = b""
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.home)
        self.drain(1.5)

    def drain(self, wait=0.3):
        end = time.time() + wait
        while time.time() < end:
            try:
                data = self.child.read_nonblocking(65536, 0.1)
                self.raw += data
                self.stream.feed(data.decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                pass
            except pexpect.EOF:
                break

    def send(self, data, wait=0.4):
        self.child.send(data)
        self.drain(wait)

    def saved_text(self):
        self.send("\x13", 0.6)          # ctrl-s
        with open(self.target) as f:
            return f.read()

    def display(self):
        return "\n".join(self.screen.display)

    def close(self):
        try:
            self.child.send("\x11")     # ctrl-q
            self.drain(0.5)
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)


def test_protocol_negotiated(binary):
    s = Session(binary, "a = 1\n")
    try:
        check(b"\x1b[>1u" in s.raw, "startup pushes kitty keyboard flags",
              s.raw[:160])
        s.child.send("\x11")            # ctrl-q
        s.drain(1.2)
        check(b"\x1b[<u" in s.raw, "exit pops kitty keyboard flags", s.raw[-160:])
    finally:
        s.close()


def test_legacy_byte_toggles_comment(binary):
    """0x1F is all a terminal without the protocol can send for ctrl-/."""
    s = Session(binary, "a = 1\nb = 2\n")
    try:
        s.send("\x1f", 0.5)
        text = s.saved_text()
        check(text == "# a = 1\nb = 2\n", "legacy 0x1F comments the line", text)
        s.send("\x1f", 0.5)
        text = s.saved_text()
        check(text == "a = 1\nb = 2\n", "legacy 0x1F uncomments it again", text)
    finally:
        s.close()


def test_csi_u_ctrl_slash(binary):
    s = Session(binary, "int a;\n", name="c.c")
    try:
        s.send("\x1b[47;5u", 0.5)
        text = s.saved_text()
        check(text == "// int a;\n", "CSI 47;5u comments with the C token", text)
    finally:
        s.close()


def test_csi_u_ctrl_shift_slash_is_help(binary):
    s = Session(binary, "a = 1\n")
    try:
        s.send("\x1b[47;6u", 0.9)
        check("FACSIMILE HELP" in s.display(), "CSI 47;6u opens help",
              s.display()[:200])
        s.send("q", 0.5)
        check(s.saved_text() == "a = 1\n", "help did not touch the buffer")
    finally:
        s.close()


def test_help_hint_and_f1(binary):
    s = Session(binary, "a = 1\n")
    try:
        check("ctrl-?:help" in s.display(), "status bar advertises ctrl-?",
              s.display()[-300:])
        s.send("\x1bOP", 0.9)           # F1
        check("FACSIMILE HELP" in s.display(), "F1 opens help",
              s.display()[:200])
    finally:
        s.close()


def test_selection_indent_baseline(binary):
    """A partial selection comments whole lines, at the shallowest indent."""
    s = Session(binary, "def f():\n    aaa = 1\n        bbb = 2\n")
    try:
        s.send("\x1b[B", 0.3)                        # down to line 2
        s.send("\x1b[C" * 6, 0.3)                    # into the middle of it
        s.send("\x1b[1;2B", 0.3)                     # shift+down: mid line 3
        s.send("\x1f", 0.5)
        check(s.saved_text() == "def f():\n    # aaa = 1\n    #     bbb = 2\n",
              "partial selection comments whole lines at one indent",
              s.saved_text())
    finally:
        s.close()


def test_legacy_csi_sequences_still_parse(binary):
    """Every digit-parameter CSI sequence now goes through the CSI-u
    lookahead first. These must come out the other side unchanged."""
    s = Session(binary, "abcd\nefgh\nijkl\n")
    try:
        s.send("\x1b[3~", 0.4)                       # delete
        check(s.saved_text() == "bcd\nefgh\nijkl\n",
              "ESC[3~ still deletes forward", s.saved_text())

        s.send("\x1b[B\x1b[4~", 0.4)                 # down, then End
        s.send("X", 0.4)
        check(s.saved_text() == "bcd\nefghX\nijkl\n",
              "ESC[4~ still moves to end of line", s.saved_text())

        s.send("\x1b[1;2H", 0.4)                     # shift+home: select to col 1
        s.send("Z", 0.4)
        check(s.saved_text() == "bcd\nZ\nijkl\n",
              "ESC[1;2H still selects to line start", s.saved_text())

        s.send("\x1b[200~pasted\x1b[201~", 0.5)      # bracketed paste
        check(s.saved_text() == "bcd\nZpasted\nijkl\n",
              "ESC[200~ bracketed paste still arrives whole", s.saved_text())

        # pageup lands on line 1, keeping the column where it can
        s.send("\x1b[5~", 0.4)
        s.send("Q", 0.4)
        check(s.saved_text() == "bcdQ\nZpasted\nijkl\n",
              "ESC[5~ still pages up to line 1", s.saved_text())
    finally:
        s.close()


def test_csi_u_chords_reach_their_commands(binary):
    """Under the protocol every ctrl chord arrives as CSI u, so spot-check
    that the translation actually reaches the same commands."""
    s = Session(binary, "a = 1\n")
    try:
        s.send("\x1b[47;5u", 0.5)                    # ctrl+/
        check(s.saved_text() == "# a = 1\n", "commented before undo")
        s.send("\x1b[122;5u", 0.5)                   # ctrl+z
        check(s.saved_text() == "a = 1\n", "CSI 122;5u reaches undo",
              s.saved_text())

        s.send("\x1b[27u", 0.4)                      # escape
        check("FACSIMILE HELP" not in s.display(), "CSI 27u is just escape")

        s.send("\x1b[1;2H", 0.3)                     # shift+home, then collapse
        s.send("\x1b[27u", 0.3)                      # escape clears the selection
        s.send("\x1b[105;5u", 0.4)                   # ctrl+i -> tab, as before
        check(s.saved_text() == "    a = 1\n",
              "CSI 105;5u keeps ctrl+i meaning tab", s.saved_text())
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_protocol_negotiated,
               test_legacy_byte_toggles_comment,
               test_csi_u_ctrl_slash,
               test_csi_u_ctrl_shift_slash_is_help,
               test_help_hint_and_f1,
               test_selection_indent_baseline,
               test_legacy_csi_sequences_still_parse,
               test_csi_u_chords_reach_their_commands):
        try:
            fn(binary)
        except Exception as exc:            # noqa: BLE001 - report and keep going
            check(False, fn.__name__, repr(exc))

    if failures:
        print(f"\nFAILED: {len(failures)} check(s): " + ", ".join(failures))
        return 1
    print("\nAll comment/help integration checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
