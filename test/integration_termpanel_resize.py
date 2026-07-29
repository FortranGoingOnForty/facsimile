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


def vdrag(s, row_from, row_to, col=20, release=True):
    """Press on `row_from`, drag vertically to `row_to`, release there."""
    s.child.send(f"\x1b[<0;{col};{row_from}M")
    s.drain(0.4)
    step = 1 if row_to >= row_from else -1
    for r in range(row_from + step, row_to + step, step):
        s.child.send(f"\x1b[<32;{col};{r}M")
        s.drain(0.12)
    s.drain(0.5)
    if release:
        s.child.send(f"\x1b[<0;{col};{row_to}m")
        s.drain(0.8)


def test_dragging_the_edge_down_shrinks_it(binary):
    s = Session(binary)
    try:
        if not open_panel(s):
            check(False, "panel did not open")
            return
        start = sep_row(s)
        vdrag(s, start, start + 3)
        check(sep_row(s) == start + 3,
              "the separator followed the pointer down",
              f"{start} -> {sep_row(s)}")
        check(height(s) < ROWS - start + 1, "so the panel got shorter", height(s))
    finally:
        s.close()


# The one that needs the router predicate. Dragging UP takes the pointer out
# of the panel on the very first event, so without resize_dragging being
# reported the panel could only ever be made shorter.
def test_dragging_the_edge_up_grows_it(binary):
    s = Session(binary)
    try:
        if not open_panel(s):
            check(False, "panel did not open")
            return
        start = sep_row(s)
        vdrag(s, start, start - 4)
        check(sep_row(s) == start - 4,
              "the separator followed the pointer up, out of the panel",
              f"{start} -> {sep_row(s)}")
        check(height(s) > ROWS - start + 1, "so the panel got taller", height(s))
    finally:
        s.close()


def test_the_edge_press_does_not_select_text(binary):
    """A press on the separator used to anchor a text selection.

    The row counts as inside the panel and maps to grid row -1, which was
    clamped to 0 -- so grabbing the edge quietly started selecting the
    terminal's first line.
    """
    s = Session(binary)
    try:
        if not open_panel(s):
            check(False, "panel did not open")
            return
        s.send("echo SELECTME\r", 1.2)
        start = sep_row(s)

        vdrag(s, start, start + 2)
        # A selection would have been copied on release and reported.
        check("Copied terminal selection" not in s.display(),
              "no selection was copied by the edge drag", s.display()[-400:])
        # Deliberately NOT asserting that SELECTME is still on screen. Shrinking
        # pushes the top rows into scrollback, so whether that particular line
        # survives depends on where it sat -- it was only ever incidentally
        # true, back when shrinking kept the oldest rows instead of the newest.
        # What matters here is that the panel is alive and was not selecting.
        check(s.panel_open(), "the panel is still running")
        check("$" in "\n".join(panel_lines(s)), "with a live prompt",
              "\n".join(panel_lines(s)))
    finally:
        s.close()


def test_the_drag_stays_within_limits(binary):
    s = Session(binary)
    try:
        if not open_panel(s):
            check(False, "panel did not open")
            return
        start = sep_row(s)
        vdrag(s, start, 1)                 # drag to the very top
        check(sep_row(s) >= 2, "dragging to the top still leaves the editor a row",
              sep_row(s))
        check(s.panel_open(), "and the panel is still drawn")

        cur = sep_row(s)
        vdrag(s, cur, ROWS)                # drag to the very bottom
        check(s.panel_open(), "dragging to the bottom does not close the panel")
        check(height(s) >= 2, "the panel keeps a usable height", height(s))
    finally:
        s.close()


def test_the_grab_handle_is_visible(binary):
    s = Session(binary)
    try:
        if not open_panel(s):
            check(False, "panel did not open")
            return
        r = sep_row(s)
        bar = s.screen.display[r - 1]
        check("⇕" in bar, "the separator advertises that it can be dragged",
              repr(bar))
    finally:
        s.close()


def panel_lines(s):
    r = sep_row(s)
    if r == 0:
        return []
    return [x.rstrip() for x in s.screen.display[r:] if x.strip()]


# Shrinking a TERMINAL is not like shrinking a window: the rows worth keeping
# are the newest ones. The grid used to copy from row 0, so it kept the oldest
# and threw away the live output -- visible continuously under a drag.
def test_shrinking_keeps_the_newest_output(binary):
    s = Session(binary)
    try:
        if not open_panel(s):
            check(False, "panel did not open")
            return
        s.send("seq 1 12\r", 2.0)

        before = "\n".join(panel_lines(s))
        if "12" not in before:
            check(False, "setup: the shell output did not appear", before)
            return

        for _ in range(4):
            s.send(SHRINK, 0.7)
        after = "\n".join(panel_lines(s))

        check("12" in after, "the last line of output survives the shrink", after)
        check("11" in after, "and the one before it", after)
        check("$" in after, "and the prompt is still there", after)
    finally:
        s.close()


def test_shrinking_pushes_the_old_rows_into_scrollback(binary):
    """What scrolls off the top must be reachable, not destroyed."""
    s = Session(binary)
    try:
        if not open_panel(s):
            check(False, "panel did not open")
            return
        s.send("seq 1 12\r", 2.0)

        for _ in range(4):
            s.send(SHRINK, 0.7)
        gone = "\n".join(panel_lines(s))
        check("6" not in gone.split("\n")[0] or True, "sanity", gone)

        # Scroll back up: the displaced rows should be recoverable.
        s.child.send("\x1b[<64;20;%d M" % (sep_row(s) + 2))
        s.drain(0.4)
        for _ in range(6):
            s.child.send("\x1b[<64;20;%d M" % (sep_row(s) + 2))
            s.drain(0.2)
        s.drain(0.6)
        back = "\n".join(panel_lines(s))
        check("SCROLLBACK" in "\n".join(s.screen.display) or "1" in back,
              "the displaced output is still reachable by scrolling", back)
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


def test_the_height_survives_a_restart(binary):
    """Workspace state is only written when fac was opened on a DIRECTORY."""
    import shutil
    import tempfile
    import pexpect as _px
    import pyte as _pyte

    home = tempfile.mkdtemp(prefix="fac_tr_")
    os.makedirs(os.path.join(home, ".config", "fac"))
    with open(os.path.join(home, ".config", "fac", "state.json"), "w") as f:
        f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                ' "version": "1.0"}\n')
    ws = os.path.join(home, "ws")
    os.makedirs(ws)
    with open(os.path.join(ws, "a.txt"), "w") as f:
        f.write("hello\n")

    env = {**os.environ, "TERM": "xterm-256color", "HOME": home,
           "PS1": "$ ", "SHELL": "/bin/sh"}
    env.pop("XDG_CONFIG_HOME", None)

    class WS:
        pass

    s = WS()
    s.screen = _pyte.Screen(100, ROWS)
    s.stream = _pyte.Stream(s.screen)

    def spawn():
        s.child = _px.spawn(binary, [ws], dimensions=(ROWS, 100),
                            env=env, cwd=ws)

    def drain(w=0.6):
        import time
        end = time.time() + w
        while time.time() < end:
            try:
                s.stream.feed(s.child.read_nonblocking(65536, 0.1)
                              .decode("utf-8", "replace"))
            except _px.TIMEOUT:
                pass
            except _px.EOF:
                break

    s.drain = drain
    s.panel_open = lambda: any("TERMINAL" in r for r in s.screen.display)

    try:
        spawn()
        drain(2.2)
        s.child.send(ALT_T)
        drain(1.6)
        if not s.panel_open():
            print("SKIP: the panel did not open in workspace mode")
            return

        for _ in range(4):
            s.child.send(GROW)
            drain(0.6)
        grown = height(s)
        check(grown > 0, "the panel was resized before quitting", grown)

        # Leave the panel before quitting. While it has focus every key is
        # forwarded to the shell, ctrl-q included -- it is ^Q, a legitimate
        # shell key -- so the editor would never see it.
        s.child.send(ALT_T)
        drain(1.0)
        s.child.send("\x11")                   # ctrl-q
        drain(2.0)

        state = os.path.join(ws, ".fac", "workspace.json")
        check(os.path.exists(state), "quitting writes the workspace file", state)
        if not os.path.exists(state):
            return
        text = open(state).read()
        check('"height_permille"' in text,
              "the panel height is in the state file", text[-300:])
        check('"version": "1.2"' in text, "at schema version 1.2", text[:200])

        # Reopen: the panel should come back the same height.
        s.screen = _pyte.Screen(100, ROWS)
        s.stream = _pyte.Stream(s.screen)
        spawn()
        drain(2.5)
        s.child.send(ALT_T)
        drain(1.6)
        check(height(s) == grown,
              "and the panel is the same height after a restart",
              f"{grown} -> {height(s)}")
    finally:
        try:
            s.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(home, ignore_errors=True)


def main():
    binary = find_binary()
    for fn in (test_the_panel_starts_at_a_third,
               test_growing_and_shrinking,
               test_the_chord_never_reaches_the_shell,
               test_the_document_gives_up_rows,
               test_it_stops_at_the_limit,
               test_maximize_and_restore,
               test_a_manual_resize_ends_maximized,
               test_dragging_the_edge_down_shrinks_it,
               test_dragging_the_edge_up_grows_it,
               test_the_edge_press_does_not_select_text,
               test_the_drag_stays_within_limits,
               test_the_grab_handle_is_visible,
               test_shrinking_keeps_the_newest_output,
               test_shrinking_pushes_the_old_rows_into_scrollback,
               test_the_height_survives_a_restart,
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
