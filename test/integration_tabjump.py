#!/usr/bin/env python3
"""
Integration test: multi-digit tab jumps.

alt-N jumps to tab N at once, and a further digit typed within a short window
extends the number -- alt-1 then 5 reaches tab 15. The jump happens on the
FIRST digit rather than waiting to see whether more follow, because waiting
would put half a second of lag on the overwhelmingly common single-digit case;
superseding a jump that already happened costs nothing.

When the tab the first digit landed on belongs to a group, the next digit picks
a member of that group instead. A group entry carries no number on the tab bar,
so there is nothing for a digit to extend towards.

The assertion that matters most is the one about the window EXPIRING: once it
has, a digit must go back to being ordinary text. If it did not, every digit
typed shortly after a tab switch would be swallowed.

Usage: python3 test/integration_tabjump.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

try:
    import pexpect                                     # noqa: F401
    import pyte                                        # noqa: F401
except ImportError as e:
    print(f"SKIP: missing dependency ({e}); pip3 install pexpect pyte")
    sys.exit(0)

from integration_tabgroups import (                     # noqa: E402
    Session, check, find_binary, failures, group_some_and_leave,
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


def open_tabs(s, n):
    """Open files 2..n as tabs through the file tree."""
    for name in s.names[1:n]:
        if not s.in_tree():
            s.send("\x02", 0.7)
        if not s.in_tree():
            return False
        for _ in range(3 * len(s.names) + 8):
            sel = s.tree_selection()
            if sel and sel.split()[0] == name:
                break
            s.send("\x1b[B", 0.08)
        s.send("\r", 0.5)
    if s.in_tree():
        s.send("\x02", 0.6)
    return True


def alt(s, digit, wait=0.25):
    s.send("\x1b" + str(digit), wait)


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
               test_the_modifier_may_stay_held_between_digits,
               test_a_modified_digit_after_the_window_is_its_own_jump,
               test_two_digits_reach_a_higher_tab,
               test_after_the_window_a_digit_is_just_text,
               test_a_non_digit_ends_the_window,
               test_an_out_of_range_number_keeps_the_first_jump,
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
