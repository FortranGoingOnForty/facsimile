#!/usr/bin/env python3
"""
Integration test: a chord's letter case is incidental.

Reported: ctrl+S did not save, and other binds were eaten with caps lock on.

Two causes. A terminal using the kitty protocol may report the SHIFTED
codepoint, so the chord was named 'ctrl-S' and matched nothing -- the letter's
case is not something the user asked for. And Ctrl+Shift+S, which a hand
reaching for Ctrl+S with Shift still down produces, was advertised by the
palette as Save All and delegated to a case that did not exist, so both the
menu entry and the chord did nothing at all.

The part that must NOT change: chords that genuinely differ by Shift still do.
Shift lives in the prefix, so ctrl-shift-z stays redo and never becomes undo.

Usage: python3 test/integration_keycase.py [path-to-fac-binary]
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

ROWS, COLS = 24, 90
failures = []

# kitty CSI-u: ESC [ <codepoint> ; <mods> u, mods = 1 + bits
# (shift 1, alt 2, ctrl 4)
CTRL = 5
CTRL_SHIFT = 6


def csi_u(cp, mods):
    return "\x1b[%d;%du" % (ord(cp) if isinstance(cp, str) else cp, mods)


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if not ok:
        failures.append(name)
        if detail:
            for ln in str(detail).split("\n")[:10]:
                print("        " + ln[:96])


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
    def __init__(self, binary, content="original\n"):
        self.home = tempfile.mkdtemp(prefix="fac_kc_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.target = os.path.join(self.home, "t.txt")
        with open(self.target, "w") as f:
            f.write(content)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.home)
        self.drain(1.8)

    def drain(self, w=0.5):
        end = time.time() + w
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                pass
            except pexpect.EOF:
                break

    def send(self, d, w=0.5):
        self.child.send(d)
        self.drain(w)

    def disk(self):
        with open(self.target) as f:
            return f.read()

    def text(self):
        return "\n".join(self.screen.display)

    def close(self):
        try:
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)


def test_ctrl_s_saves_when_the_letter_arrives_uppercase(binary):
    """Caps lock, or a terminal reporting the shifted codepoint."""
    s = Session(binary)
    try:
        s.send("EDIT", 0.6)
        check(s.disk() == "original\n", "not yet written", repr(s.disk()))
        s.send(csi_u('S', CTRL), 1.2)
        check(s.disk() == "EDIToriginal\n",
              "ctrl-S with an uppercase letter still saves", repr(s.disk()))
    finally:
        s.close()


def test_ctrl_shift_s_saves(binary):
    """The palette has advertised this for ages against a case that never
    existed."""
    s = Session(binary)
    try:
        s.send("EDIT", 0.6)
        s.send(csi_u('s', CTRL_SHIFT), 1.2)
        check(s.disk() == "EDIToriginal\n",
              "ctrl+shift+s saves rather than doing nothing", repr(s.disk()))
    finally:
        s.close()


def test_save_all_from_the_palette_works(binary):
    s = Session(binary)
    try:
        s.send("EDIT", 0.6)
        s.send("\x10", 0.8)                 # ctrl-p
        s.send("Save All", 0.6)
        s.send("\r", 1.4)
        check(s.disk() == "EDIToriginal\n",
              "the Save All palette entry writes the file", repr(s.disk()))
    finally:
        s.close()


# The distinction that must survive: shift is in the prefix, not the letter.
def test_ctrl_shift_z_is_still_redo(binary):
    s = Session(binary)
    try:
        s.send("abc", 0.6)
        s.send("\x1a", 0.7)                 # ctrl-z: undo
        after_undo = s.text()
        check("abc" not in after_undo, "ctrl-z undid the typing", after_undo[:200])

        s.send(csi_u('z', CTRL_SHIFT), 0.8)  # ctrl-shift-z: redo
        check("abc" in s.text(),
              "ctrl+shift+z redoes rather than undoing again", s.text()[:200])
    finally:
        s.close()


def test_an_uppercase_redo_chord_still_redoes(binary):
    """ctrl-shift-Z must normalise to ctrl-shift-z, not to ctrl-z."""
    s = Session(binary)
    try:
        s.send("abc", 0.6)
        s.send("\x1a", 0.7)
        check("abc" not in s.text(), "undone", s.text()[:120])
        s.send(csi_u('Z', CTRL_SHIFT), 0.8)
        check("abc" in s.text(),
              "an uppercase ctrl+shift+Z is still redo, never undo",
              s.text()[:200])
    finally:
        s.close()


def test_plain_ctrl_s_is_unaffected(binary):
    s = Session(binary)
    try:
        s.send("EDIT", 0.6)
        s.send("\x13", 1.2)                 # the ordinary control byte
        check(s.disk() == "EDIToriginal\n",
              "the ordinary ctrl-s byte still saves", repr(s.disk()))
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_ctrl_s_saves_when_the_letter_arrives_uppercase,
               test_ctrl_shift_s_saves,
               test_save_all_from_the_palette_works,
               test_ctrl_shift_z_is_still_redo,
               test_an_uppercase_redo_chord_still_redoes,
               test_plain_ctrl_s_is_unaffected):
        try:
            fn(binary)
        except Exception as exc:                        # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    if failures:
        print(f"\nintegration_keycase: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("\nintegration_keycase: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
