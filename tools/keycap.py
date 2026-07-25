#!/usr/bin/env python3
"""Show exactly what your terminal sends for a key, in the mode fac uses.

fac negotiates the kitty keyboard protocol at startup (CSI > 1 u), which
changes what some terminals send. A key can therefore behave differently
inside fac than in a plain raw-mode reader -- so this pushes the same flags
before reading, and pops them on exit.

    python3 tools/keycap.py            # same mode as fac
    python3 tools/keycap.py --legacy   # without the protocol, for comparison

Press keys; Ctrl-C or 'q' on its own quits.
"""
import argparse
import os
import sys
import termios
import tty

CSI = "\x1b["


def describe(seq: bytes) -> str:
    """Best-effort reading of a sequence, mirroring fac's input handler."""
    if not seq:
        return ""
    if seq[0] != 0x1B:
        b = seq[0]
        if b == 0:
            return "ctrl-space"
        if b == 9:
            return "tab"
        if b in (10, 13):
            return "enter"
        if b == 8:
            return "ctrl-h"
        if b == 31:
            return "ctrl-/ (also what ctrl-shift-/ sends without the protocol)"
        if b == 127:
            return "backspace"
        if b < 27:
            return "ctrl-%s" % chr(ord("a") + b - 1)
        return "text %r" % seq.decode("utf-8", "replace")

    s = seq.decode("latin-1")
    if len(s) == 1:
        return "esc"
    if s.startswith("\x1bO") and len(s) == 3:
        return {"P": "f1", "Q": "f2", "R": "f3", "S": "f4"}.get(s[2], "SS3 %r" % s[2])
    if s.startswith(CSI):
        body = s[2:]
        if body.endswith("u"):
            params = body[:-1]
            code = params.split(";")[0].split(":")[0]
            mods = ""
            if ";" in params:
                mods = " modifiers=%s" % params.split(";")[1].split(":")[0]
            try:
                n = int(code)
            except ValueError:
                return "CSI-u, unparsable (%r)" % params
            named = {27: "escape", 13: "enter", 9: "tab", 127: "backspace",
                     32: "space"}
            if n in named:
                return "CSI-u %s%s" % (named[n], mods)
            if 57364 <= n <= 57375:
                return "CSI-u F%d (kitty functional key)%s" % (n - 57363, mods)
            if 57376 <= n <= 57398:
                return "CSI-u F%d (kitty functional key)%s" % (n - 57363, mods)
            if 57344 <= n <= 57454:
                return "CSI-u private-use key %d (keypad/media/modifier)%s" % (n, mods)
            if 32 <= n <= 126:
                return "CSI-u %r%s" % (chr(n), mods)
            return "CSI-u codepoint %d%s" % (n, mods)
        if body.endswith("~"):
            num = body[:-1].split(";")[0]
            legacy = {"1": "home", "2": "insert", "3": "delete", "4": "end",
                      "5": "pageup", "6": "pagedown", "11": "f1", "12": "f2",
                      "13": "f3", "14": "f4", "15": "f5", "17": "f6",
                      "18": "f7", "19": "f8", "20": "f9", "21": "f10",
                      "23": "f11", "24": "f12"}
            return "CSI ~ %s" % legacy.get(num, "unknown(%s)" % num)
        if body and body[-1] in "ABCDHF":
            return "CSI %s" % {"A": "up", "B": "down", "C": "right",
                               "D": "left", "H": "home", "F": "end"}[body[-1]]
    return "unrecognised"


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--legacy", action="store_true",
                    help="do not negotiate the kitty keyboard protocol")
    args = ap.parse_args()

    if not sys.stdin.isatty():
        print("Run this in a terminal, not through a pipe.")
        return 2

    fd = sys.stdin.fileno()
    saved = termios.tcgetattr(fd)
    mode = "legacy (no protocol)" if args.legacy else "kitty protocol on (as fac runs)"
    print("Mode: %s" % mode)
    print("Press keys. Ctrl-C to quit.\n")
    try:
        tty.setraw(fd)
        if not args.legacy:
            os.write(1, (CSI + ">1u").encode())
        while True:
            first = os.read(fd, 1)
            if not first:
                break
            seq = first
            if first == b"\x1b":
                # Drain the rest of the sequence
                import select
                while select.select([fd], [], [], 0.02)[0]:
                    more = os.read(fd, 1)
                    if not more:
                        break
                    seq += more
            hexed = " ".join("%02x" % b for b in seq)
            printable = seq.decode("latin-1").replace("\x1b", "ESC")
            os.write(1, ("  %-28s %-24s %s\r\n"
                         % (hexed, repr(printable), describe(seq))).encode())
            if seq == b"\x03":
                break
    finally:
        if not args.legacy:
            os.write(1, (CSI + "<u").encode())
        termios.tcsetattr(fd, termios.TCSADRAIN, saved)
    print()
    return 0


if __name__ == "__main__":
    sys.exit(main())
