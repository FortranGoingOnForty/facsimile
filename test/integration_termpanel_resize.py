#!/usr/bin/env python3
"""
Integration test: the terminal panel can be resized.

The panel's height used to be a fixed fraction recomputed from a constant, so
there was nothing to change. It is now a stored ratio, adjustable from the
keyboard while the panel has focus.

The load-bearing assertion here is not that the panel grew -- it is that the
chord produced NO TEXT IN THE SHELL. Every key that reaches the focused panel
is forwarded to the pty, and the block that handles them deliberately swallows
what it does not recognise, so a resize chord wired in the wrong place fails
silently or, worse, arrives at the prompt as an escape sequence.

Usage: python3 test/integration_termpanel_resize.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

try:
    import pexpect                                    # noqa: F401
    import pyte                                       # noqa: F401
except ImportError as e:
    print(f"SKIP: missing dependency ({e}); pip3 install pexpect pyte")
    sys.exit(0)

from integration_terminal_panel import (              # noqa: E402
    Session, check, find_binary, failures, ROWS, ALT_T,
)

GROW = "\x1b[1;6A"      # ctrl+shift+up   (modifier 6 = 1 + shift + ctrl)
SHRINK = "\x1b[1;6B"    # ctrl+shift+down
# ctrl+shift+m is indistinguishable from Ctrl-M (i.e. Enter) in a legacy
# terminal, so it only exists under the kitty keyboard protocol, which the
# editor negotiates with CSI >1u. Same footing as ctrl+shift+c/v.
MAXIMIZE = "\x1b[109;6u"


def sep_row(s):
    """1-based screen row of the panel's separator bar, or 0."""
    for i, r in enumerate(s.screen.display, 1):
        if "TERMINAL" in r or "terminal " in r:
            if "-" in r:
                return i
    return 0


def height(s):
    """Rows the panel occupies, separator included."""
    r = sep_row(s)
    return 0 if r == 0 else ROWS - r + 1


def open_panel(s):
    if not s.panel_open():
        s.send(ALT_T, 1.5)
    return s.panel_open()


def shell_text(s):
    """Everything drawn inside the panel, below the separator bar."""
    r = sep_row(s)
    if r == 0:
        return ""
    return "\n".join(x.rstrip() for x in s.screen.display[r:])


def test_the_panel_starts_at_a_third(binary):
    s = Session(binary)
    try:
        if not open_panel(s):
            check(False, "panel did not open")
            return
        h = height(s)
        check(h > 0, "the panel has a measurable height", h)
        # A third of 30 rows, give or take the clamps and rounding.
        check(6 <= h <= 14, "and it is roughly a third of the screen", h)
    finally:
        s.close()


def test_growing_and_shrinking(binary):
    s = Session(binary)
    try:
        if not open_panel(s):
            check(False, "panel did not open")
            return
        start = height(s)

        s.send(GROW, 0.9)
        grown = height(s)
        check(grown == start + 1, "ctrl+shift+up adds exactly one row",
              f"{start} -> {grown}")

        s.send(GROW, 0.9)
        s.send(GROW, 0.9)
        check(height(s) == start + 3, "and keeps adding one per press",
              f"{start} -> {height(s)}")

        s.send(SHRINK, 0.9)
        check(height(s) == start + 2, "ctrl+shift+down takes one back",
              f"{height(s)}")
    finally:
        s.close()


# THE assertion. A chord wired anywhere but the focused-panel block either
# vanishes or reaches the shell as text.
def test_the_chord_never_reaches_the_shell(binary):
    s = Session(binary)
    try:
        if not open_panel(s):
            check(False, "panel did not open")
            return
        before = shell_text(s)

        for _ in range(3):
            s.send(GROW, 0.7)
        for _ in range(3):
            s.send(SHRINK, 0.7)

        after = shell_text(s)
        # An escape sequence that reached the shell shows up as literal text:
        # the bracket form, or the shell complaining about it.
        for junk in ("[1;6A", "[1;6B", "1;6A", "1;6B", "6A", "6B"):
            check(junk not in after,
                  f"no {junk!r} was typed into the shell", after[:400])
        check("command not found" not in after,
              "and the shell did not try to run it", after[:400])
        check(before.count("$") <= after.count("$") + 1,
              "the prompt is still there", after[:400])
    finally:
        s.close()


def test_the_document_gives_up_rows(binary):
    """Growing the panel must actually take rows from the editor, not overlap."""
    s = Session(binary)
    try:
        if not open_panel(s):
            check(False, "panel did not open")
            return
        first = sep_row(s)
        s.send(GROW, 0.9)
        s.send(GROW, 0.9)
        second = sep_row(s)
        check(second == first - 2,
              "the separator moved up, so the editor area shrank",
              f"{first} -> {second}")
        check(second > 1, "and the editor still has rows", second)
    finally:
        s.close()


def test_it_stops_at_the_limit(binary):
    """Holding grow must not eat the document or wrap around."""
    s = Session(binary)
    try:
        if not open_panel(s):
            check(False, "panel did not open")
            return
        for _ in range(ROWS + 5):
            s.send(GROW, 0.16)
        r = sep_row(s)
        check(r >= 2, "the panel never takes the whole screen", r)
        check(height(s) <= ROWS - 2, "leaving room for the editor", height(s))

        for _ in range(ROWS + 5):
            s.send(SHRINK, 0.16)
        check(height(s) >= 2,
              "and shrinking stops while the panel is still usable", height(s))
        check(s.panel_open(), "the panel is still open, not shrunk away")
    finally:
        s.close()


def test_maximize_and_restore(binary):
    s = Session(binary)
    try:
        if not open_panel(s):
            check(False, "panel did not open")
            return
        start = height(s)

        s.send(MAXIMIZE, 1.0)
        big = height(s)
        check(big > start, "ctrl+shift+m makes the panel much taller",
              f"{start} -> {big}")
        check(sep_row(s) >= 2, "but the editor keeps at least a row",
              sep_row(s))

        s.send(MAXIMIZE, 1.0)
        check(height(s) == start, "and pressing it again restores the old size",
              f"{big} -> {height(s)} (was {start})")
    finally:
        s.close()


def test_a_manual_resize_ends_maximized(binary):
    """After maximising, a nudge should adjust from where it is.

    If the restore point survived a manual resize, the next ctrl+shift+m would
    jump somewhere the user never chose.
    """
    s = Session(binary)
    try:
        if not open_panel(s):
            check(False, "panel did not open")
            return
        s.send(MAXIMIZE, 1.0)
        big = height(s)
        s.send(SHRINK, 0.9)
        check(height(s) == big - 1, "shrink works while maximized",
              f"{big} -> {height(s)}")

        after = height(s)
        s.send(MAXIMIZE, 1.0)
        check(height(s) > after,
              "and ctrl+shift+m now maximizes again rather than restoring",
              f"{after} -> {height(s)}")
    finally:
        s.close()


def test_pane_navigation_is_untouched(binary):
    """ctrl+shift+up is only a resize while the terminal has focus.

    With the panel closed -- or open but unfocused -- it must still be
    navigate-to-pane-above, which is what it has always been.
    """
    s = Session(binary)
    try:
        check(not s.panel_open(), "starting with the panel closed")
        before = s.display()
        s.send(GROW, 0.8)
        check(not s.panel_open(),
              "ctrl+shift+up with no panel does not conjure one")
        # Nothing should have been typed into the document either.
        check("[1;6A" not in s.display() and "6A" not in s.display(),
              "and nothing was inserted into the document", s.display()[:300])
        check(s.saved_file() == "editor text\n",
              "the file on disk is untouched", s.saved_file())
        del before
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_the_panel_starts_at_a_third,
               test_growing_and_shrinking,
               test_the_chord_never_reaches_the_shell,
               test_the_document_gives_up_rows,
               test_it_stops_at_the_limit,
               test_maximize_and_restore,
               test_a_manual_resize_ends_maximized,
               test_pane_navigation_is_untouched):
        try:
            fn(binary)
        except Exception as exc:                       # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    if failures:
        print(f"\nintegration_termpanel_resize: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("\nintegration_termpanel_resize: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
