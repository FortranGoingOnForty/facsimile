#!/usr/bin/env python3
"""
Integration test: multi-digit tab jumps.

alt-N jumps to tab N at once, and a further digit typed within a short window
extends the number -- alt-1 then 5 reaches tab 15. The jump happens on the
FIRST digit rather than waiting to see whether more follow, because waiting
would put half a second of lag on the overwhelmingly common single-digit case;
superseding a jump that already happened costs nothing.

If the extended number does not exist, its last digit is retried as a fresh
single-digit jump. Alt-2 then Alt-1 therefore tries tab 21 and falls back to
tab 1 rather than leaving the user stranded on tab 2.

When the tab the first digit landed on belongs to a group, the next digit picks
a member of that group instead. A group entry carries no number on the tab bar,
so there is nothing for a digit to extend towards.

The assertion that matters most is the one about the window EXPIRING: once it
has, a digit must go back to being ordinary text. If it did not, every digit
typed shortly after a tab switch would be swallowed.

Usage: python3 test/integration_tabjump.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import codecs
import json
import os
import re
import shutil
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

try:
    import pexpect                                     # noqa: F401
    import pyte                                        # noqa: F401
except ImportError as e:
    print(f"SKIP: missing dependency ({e}); pip3 install pexpect pyte")
    sys.exit(0)

from integration_tabgroups import (                     # noqa: E402
    COLS, ROWS, Session, check, find_binary, failures, group_some_and_leave,
)

N_TABS = 16


def showing(s):
    """Which file is on screen, read from the DOCUMENT.

    Deliberately not the status bar: a status message replaces the path there,
    and this feature sets one, so reading the path would report nothing exactly
    when it matters.
    """
    for ln in s.screen.display:
        m = re.search(r"(file\d+\.txt) line \d+", ln)
        if m:
            return m.group(1)
    return None


def make_more_files(s, n):
    s.names = ["file%02d.txt" % i for i in range(1, n + 1)]
    for name in s.names:
        p = os.path.join(s.ws, name)
        if not os.path.exists(p):
            with open(p, "w") as f:
                f.write("".join("%s line %d\n" % (name, i) for i in range(1, 40)))


def open_tabs(s, n, leave_tree_open=False):
    """Open files 2..n as tabs through the file tree."""
    for name in s.names[1:n]:
        if not s.in_tree():
            s.send("\x02", 0.7)
        if not s.in_tree():
            return False
        for _ in range(3 * len(s.names) + 8):
            sel = s.tree_selection()
            if sel and name in sel:
                break
            s.send("\x1b[B", 0.08)
        s.send("\r", 0.5)
        # Opening a file deliberately leaves Fuss active. Losing the status
        # row here used to make the active panel look closed and caused the
        # next iteration to toggle the actual state in the wrong direction.
        if not s.in_tree():
            return False
    if not leave_tree_open and s.in_tree():
        s.send("\x02", 0.6)
    return True


def alt(s, digit, wait=0.25):
    s.send("\x1b" + str(digit), wait)


def ctrl(s, digit, wait=0.25):
    """Ctrl+digit as a kitty CSI-u key event."""
    s.send(f"\x1b[{ord(str(digit))};5u", wait)


def grouped_session(binary, n_groups=6):
    """Restore interleaved loose tabs and groups with deliberately odd ids."""
    s = Session(binary)
    s.child.terminate(force=True)
    shutil.rmtree(s.ws)
    os.makedirs(s.ws)

    labels = ("alpha", "bravo", "charlie", "delta", "echo", "foxtrot")
    ids = (41, 3, 99, 7, 2, 80)
    groups, tabs = [], []

    def tab(path, gid, ordinal):
        return {"filename": path, "is_orphan": False, "modified": False,
                "group": gid, "group_ordinal": ordinal,
                "panes": [{"x_start": 0.0, "y_start": 0.0, "x_end": 1.0,
                           "y_end": 1.0, "filename": path, "cursor_line": 1,
                           "cursor_column": 1, "viewport_line": 1,
                           "viewport_column": 1}],
                "active_pane": 1}

    for position, (label, gid) in enumerate(zip(labels, ids), start=1):
        if position > n_groups:
            break
        directory = os.path.join(s.ws, label)
        os.makedirs(directory)
        active_name = f"file{position:02d}.txt"
        other_name = f"member{position:02d}.txt"
        for name in (active_name, other_name):
            with open(os.path.join(directory, name), "w") as f:
                f.write("".join(f"{name} line {line}\n" for line in range(1, 30)))
        active_rel = f"{label}/{active_name}"
        other_rel = f"{label}/{other_name}"
        groups.append({"id": gid, "label": label + "/", "dir_path": directory,
                       "active_member": os.path.join(directory, active_name)})
        tabs.extend((tab(active_rel, gid, 1), tab(other_rel, gid, 2)))

        # Loose tabs are intentionally interleaved between group entries. They
        # must not consume a Ctrl+number ordinal.
        loose_name = f"loose{position:02d}.txt"
        with open(os.path.join(s.ws, loose_name), "w") as f:
            f.write(f"{loose_name} line 1\n")
        tabs.append(tab(loose_name, 0, 0))

    doc = {"version": "1.2", "workspace_path": s.ws,
           "last_opened": "20260910", "tab_groups": groups, "tabs": tabs,
           "active_tab": 1, "fuss_mode": False}
    os.makedirs(os.path.join(s.ws, ".fac"))
    with open(os.path.join(s.ws, ".fac", "workspace.json"), "w") as f:
        json.dump(doc, f, indent=2)

    env = {**os.environ, "TERM": "xterm-256color", "HOME": s.home}
    env.pop("XDG_CONFIG_HOME", None)
    env.pop("FAC_SESSION", None)
    s.screen = pyte.Screen(COLS, ROWS)
    s.stream = pyte.Stream(s.screen)
    s.decoder = codecs.getincrementaldecoder("utf-8")("replace")
    s.child = pexpect.spawn(binary, [s.ws], dimensions=(ROWS, COLS),
                            env=env, cwd=s.ws)
    s.drain(2.5)
    return s


def many_tabs(binary, n=N_TABS):
    s = Session(binary)
    make_more_files(s, n)
    if not open_tabs(s, n):
        s.close()
        return None
    return s


def test_a_single_digit_still_jumps(binary):
    s = many_tabs(binary)
    if s is None:
        check(False, "setup: could not open tabs")
        return
    try:
        alt(s, 1, 0.9)
        check(showing(s) == "file01.txt", "alt-1 goes to tab 1", showing(s))
        alt(s, 7, 0.9)
        check(showing(s) == "file07.txt", "alt-7 goes to tab 7", showing(s))
    finally:
        s.close()


def test_ctrl_digits_jump_to_groups_in_bar_order(binary):
    s = grouped_session(binary)
    try:
        ctrl(s, 2, 0.8)
        check(showing(s) == "file02.txt",
              "ctrl-2 enters the second group from the left", showing(s))
        ctrl(s, 5, 0.8)
        check(showing(s) == "file05.txt",
              "loose tabs do not consume group ordinals", showing(s))
    finally:
        s.close()


def test_ctrl_group_jump_falls_back_and_rearms(binary):
    s = grouped_session(binary)
    try:
        ctrl(s, 2)
        ctrl(s, 6, 0.2)                    # group 26 is absent; group 6 exists
        check(showing(s) == "file06.txt",
              "ctrl-2 then ctrl-6 falls back from group 26 to group 6",
              showing(s))
        check("Group 6" in s.status() and "another digit" in s.status(),
              "the fallback group starts a fresh extension window", s.status())
    finally:
        s.close()


def test_two_digits_reach_a_higher_tab(binary):
    s = many_tabs(binary)
    if s is None:
        check(False, "setup: could not open tabs")
        return
    try:
        alt(s, 1)
        s.child.send("5")
        s.drain(0.9)
        check(showing(s) == "file15.txt", "alt-1 then 5 reaches tab 15", showing(s))

        alt(s, 1)
        s.child.send("2")
        s.drain(0.9)
        check(showing(s) == "file12.txt", "and alt-1 then 2 reaches tab 12",
              showing(s))
    finally:
        s.close()


# THE assertion. Without the deadline, every digit typed shortly after a tab
# switch would be eaten instead of typed.
def test_after_the_window_a_digit_is_just_text(binary):
    s = many_tabs(binary)
    if s is None:
        check(False, "setup: could not open tabs")
        return
    try:
        alt(s, 1, 0.9)                  # let the window lapse
        s.drain(0.8)
        before = showing(s)
        s.child.send("5")
        s.drain(0.8)
        check(showing(s) == before,
              "a digit typed after the window does not change tab",
              f"{before} -> {showing(s)}")
        check(re.search(r"5file01\.txt", "\n".join(s.screen.display)) is not None,
              "it was typed into the document instead",
              "\n".join(s.screen.display[:6]))
    finally:
        s.close()


def test_a_non_digit_ends_the_window(binary):
    s = many_tabs(binary)
    if s is None:
        check(False, "setup: could not open tabs")
        return
    try:
        alt(s, 1)
        s.child.send("x")               # not a digit: ends it
        s.drain(0.5)
        s.child.send("5")               # so this is text, not a continuation
        s.drain(0.7)
        check(showing(s) == "file01.txt",
              "a letter closes the window, so the next digit is text",
              showing(s))
        check("x5" in "\n".join(s.screen.display),
              "and both characters were typed", "\n".join(s.screen.display[:6]))
    finally:
        s.close()


def test_an_out_of_range_number_keeps_the_first_jump(binary):
    s = many_tabs(binary)
    if s is None:
        check(False, "setup: could not open tabs")
        return
    try:
        alt(s, 9)
        s.child.send("9")               # tab 99 does not exist
        s.drain(0.9)
        check(showing(s) == "file09.txt",
              "an impossible number leaves the first jump standing", showing(s))
        check("9" not in "".join(s.screen.display[1:3]).replace("file09", ""),
              "and the digit was not typed into the document",
              "\n".join(s.screen.display[:4]))
    finally:
        s.close()


def test_an_invalid_composite_falls_back_to_its_last_digit(binary):
    # Twelve tabs are enough to prove both fallback and re-arming while
    # keeping setup below the crowded-tab tree toggle edge exercised by the
    # separate overflow suites.
    s = many_tabs(binary, n=12)
    if s is None:
        check(False, "setup: could not open tabs")
        return
    try:
        alt(s, 2)
        s.send("\x1b1", 0.2)            # keep Alt held: tab 21 is absent
        check(showing(s) == "file01.txt",
              "alt-2 then alt-1 falls back from tab 21 to tab 1", showing(s))
        check("another digit" in s.status(),
              "the fallback digit starts a fresh extension window", s.status())

        s.child.send("2")
        s.drain(0.9)
        check(showing(s) == "file12.txt",
              "a digit after the fallback can still extend to tab 12", showing(s))
    finally:
        s.close()


def test_a_crowded_tab_bar_does_not_scroll_fuss_off_its_rows(binary):
    s = Session(binary)
    # The larger visible tree produces enough UTF-8 output to exercise PTY
    # chunk boundaries while tab 12 makes the strip itself crowded.
    make_more_files(s, 16)
    try:
        opened = open_tabs(s, 12, leave_tree_open=True)
        check(opened, "opening tab 12 keeps the Fuss chevron on the status row",
              s.status())
        check("WORKSPACE" in s.row(2)[:30],
              "the crowded tab strip stays above the Fuss body", s.row(1) + "\n" + s.row(2))
        check("file12.txt" in s.row(1),
              "the active crowded tab remains on row one", s.row(1))
    finally:
        s.close()


def test_the_pending_window_is_announced(binary):
    s = many_tabs(binary)
    if s is None:
        check(False, "setup: could not open tabs")
        return
    try:
        alt(s, 1, 0.2)                  # read inside the window
        check("another digit" in s.status(),
              "the status bar says a digit would extend the jump", s.status())
        s.drain(0.9)                    # let it lapse
        check("another digit" not in s.status(),
              "and stops saying so once the window closes", s.status())
    finally:
        s.close()


# The group half of the request.
def test_a_digit_picks_a_group_member(binary):
    s = Session(binary)
    try:
        if not group_some_and_leave(s):
            check(False, "setup: could not form the group")
            return
        # Tabs 1-4 are the group's members; file05 is outside it.
        for digit, want in ((3, "file03.txt"), (2, "file02.txt"),
                            (4, "file04.txt"), (1, "file01.txt")):
            alt(s, 1)
            s.child.send(str(digit))
            s.drain(0.9)
            check(showing(s) == want,
                  f"alt-1 then {digit} reaches group member {digit}",
                  showing(s))
    finally:
        s.close()


def test_the_group_window_says_how_many_members(binary):
    s = Session(binary)
    try:
        if not group_some_and_leave(s):
            check(False, "setup: could not form the group")
            return
        alt(s, 1, 0.2)
        check("member" in s.status(),
              "the hint says a digit picks a member", s.status())
        check("1-4" in s.status(), "and how many there are", s.status())
    finally:
        s.close()


def test_too_high_a_member_says_so(binary):
    s = Session(binary)
    try:
        if not group_some_and_leave(s):
            check(False, "setup: could not form the group")
            return
        alt(s, 1)
        s.child.send("9")               # the group has 4
        s.drain(0.5)
        check("4 members" in s.status(),
              "asking for member 9 of 4 reports the count", s.status())
        s.drain(0.6)
        check(showing(s) == "file01.txt",
              "and stays on the member the first digit reached", showing(s))
    finally:
        s.close()


# Reported: holding the modifier down for the second digit jumped to tab 1 and
# then to tab 5, instead of tab 15. Only a BARE digit was accepted as a
# continuation, so ctrl-5 fell through to the main dispatch's own jump case.
def test_the_modifier_may_stay_held_between_digits(binary):
    s = many_tabs(binary)
    if s is None:
        check(False, "setup: could not open tabs")
        return
    try:
        alt(s, 1)
        s.send("\x1b5", 0.9)               # alt-5, without letting go
        check(showing(s) == "file15.txt",
              "alt-1 then alt-5 reaches tab 15", showing(s))

        alt(s, 1)
        s.send("\x1b2", 0.9)
        check(showing(s) == "file12.txt",
              "and alt-1 then alt-2 reaches tab 12", showing(s))

        # Bare digits must go on working; this added a form, it did not
        # replace one.
        alt(s, 1)
        s.child.send("4")
        s.drain(0.9)
        check(showing(s) == "file14.txt",
              "a bare digit still continues the jump", showing(s))
    finally:
        s.close()


def test_a_modified_digit_after_the_window_is_its_own_jump(binary):
    """The window must not swallow modified digits forever."""
    s = many_tabs(binary)
    if s is None:
        check(False, "setup: could not open tabs")
        return
    try:
        alt(s, 1, 0.9)
        s.drain(0.8)                       # let the window lapse
        alt(s, 5, 0.9)
        check(showing(s) == "file05.txt",
              "alt-5 on its own still goes to tab 5", showing(s))
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_a_single_digit_still_jumps,
               test_ctrl_digits_jump_to_groups_in_bar_order,
               test_ctrl_group_jump_falls_back_and_rearms,
               test_the_modifier_may_stay_held_between_digits,
               test_a_modified_digit_after_the_window_is_its_own_jump,
               test_two_digits_reach_a_higher_tab,
               test_after_the_window_a_digit_is_just_text,
               test_a_non_digit_ends_the_window,
               test_an_out_of_range_number_keeps_the_first_jump,
               test_an_invalid_composite_falls_back_to_its_last_digit,
               test_a_crowded_tab_bar_does_not_scroll_fuss_off_its_rows,
               test_the_pending_window_is_announced,
               test_a_digit_picks_a_group_member,
               test_the_group_window_says_how_many_members,
               test_too_high_a_member_says_so):
        try:
            fn(binary)
        except Exception as exc:                        # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    if failures:
        print(f"\nintegration_tabjump: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("\nintegration_tabjump: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
