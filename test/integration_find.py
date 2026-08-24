#!/usr/bin/env python3
"""Integration test: Ctrl-F is a find BAR, not a blocking prompt.

It used to run its own read loop. Nothing could re-render while it was up,
so the matches it lit only became visible after it exited -- you searched
blind, then looked. It is now state plus a key handler plus a render, driven
by the main loop, which is what lets every match stay lit while you walk
between them.

Three things this pins down, in order of how easy they are to break:

  * Ctrl-F on a word searches for THAT WORD, without typing it.
  * Every match on the page is lit AND the one you are on looks different.
    A search that highlights everything identically tells you the pattern
    matched but not where you are in it, which is the whole point of
    stepping. `screen.display` cannot see this -- it returns text with the
    attributes stripped -- so these assertions read cell attributes out of
    `screen.buffer` instead.
  * The bar survives navigation. It closes on a second Ctrl-F or on ESC,
    and on nothing else.

Usage: python3 test/integration_find.py [path-to-fac-binary]
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

CTRL_F = "\x06"
CTRL_S = "\x13"
CTRL_HOME = "\x1b[1;5H"
UP, DOWN, RIGHT, LEFT = "\x1b[A", "\x1b[B", "\x1b[C", "\x1b[D"
PAGEUP, PAGEDOWN = "\x1b[5~", "\x1b[6~"
ENTER, TAB, ESC = "\r", "\t", "\x1b"
SHIFT_TAB = "\x1b[Z"
SHIFT_ENTER = "\x1b[13;2u"      # CSI-u; a plain terminal cannot express it
SHIFT_SPACE = "\x1b[32;2u"

# Line N carries the needle when N % 4 == 0. Twelve of them in a 48-line
# document: more than one screen holds, so "did the page move?" is a real
# question and not one the document is too short to ask.
NLINES = 48
NEEDLE = "MATCH"

failures = []


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if not ok:
        failures.append(name)
        if detail:
            for ln in str(detail).split("\n")[:6]:
                print("        " + ln[:110])


def find_binary():
    if len(sys.argv) > 1:
        return os.path.abspath(sys.argv[1])
    here = os.path.dirname(os.path.abspath(__file__))
    cand = os.path.join(os.path.dirname(here), "fac")
    if os.path.exists(cand):
        return cand
    print("SKIP: no fac binary (build with make, or pass a path)")
    sys.exit(0)


def document():
    out = []
    for i in range(1, NLINES + 1):
        if i % 4 == 0:
            out.append("line %02d has %s here" % (i, NEEDLE))
        else:
            out.append("line %02d plain text" % i)
    return "\n".join(out) + "\n"


class Editor:
    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_find_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_find_work_")
        self.path = os.path.join(self.work, "doc.txt")
        with open(self.path, "w") as f:
            f.write(document())
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.path], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.work)
        self.drain(2.5)

    def drain(self, w=0.3):
        end = time.time() + w
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def send(self, d, w=0.3):
        self.child.send(d)
        self.drain(w)

    # ---- reading the screen -------------------------------------------

    def bar(self):
        """The bottom line, which the find bar replaces while it is up."""
        return self.screen.display[-1].strip()

    def bar_visible(self):
        return self.bar().startswith("find") or self.bar().startswith("repl")

    def caret(self):
        import re
        m = re.search(r"Ln (\d+), Col (\d+)", self.screen.display[-1])
        return (int(m.group(1)), int(m.group(2))) if m else (-1, -1)

    def counter(self):
        """(index, total) off the bar, or None."""
        import re
        m = re.search(r"(\d+) of (\d+)", self.bar())
        return (int(m.group(1)), int(m.group(2))) if m else None

    def top_line(self):
        """Buffer line number shown in the first content row's gutter."""
        for row in self.screen.display[1:]:
            tok = row.strip().split(" ")
            if tok and tok[0].isdigit():
                return int(tok[0])
        return -1

    def highlights(self):
        """Every run of styled cells on screen, as (buffer_line, text, kind).

        kind is 'active' for the bold-on-orange match the caret is on and
        'match' for the plain yellow of the others. Reading attributes is
        the only way to tell them apart -- and the only way to notice if a
        future change makes them the same again.
        """
        runs = []
        for r in range(1, ROWS - 1):
            text = self.screen.display[r]
            tok = text.strip().split(" ")
            if not tok or not tok[0].isdigit():
                continue
            lineno = int(tok[0])
            cur = None
            for c in range(COLS):
                cell = self.screen.buffer[r][c]
                if cell.bold and cell.bg == "ff8700":
                    kind = "active"
                elif cell.bg == "brown":
                    kind = "match"
                else:
                    kind = None
                if kind is None:
                    if cur:
                        runs.append(cur)
                        cur = None
                    continue
                if cur and cur[2] == kind:
                    cur = (cur[0], cur[1] + cell.data, kind)
                else:
                    if cur:
                        runs.append(cur)
                    cur = (lineno, cell.data, kind)
            if cur:
                runs.append(cur)
        return runs

    def goto_needle(self, line):
        """Park the caret in the middle of the needle on `line`."""
        self.send(CTRL_HOME, 0.4)
        for _ in range(line - 1):
            self.send(DOWN, 0.05)
        # "line NN has MATCH here" -> the needle starts at column 13
        for _ in range(14):
            self.send(RIGHT, 0.04)
        self.drain(0.4)

    def close(self):
        try:
            self.child.close(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


# ---------------------------------------------------------------------------

def test_ctrl_f_on_a_word_searches_for_that_word(binary):
    print("\nCtrl-F on a word searches for it, without typing it")
    e = Editor(binary)
    try:
        e.goto_needle(4)
        ln, col = e.caret()
        check((ln, col) == (4, 15), "the caret starts inside the needle",
              f"caret at {ln},{col}")

        e.send(CTRL_F, 0.8)
        check(e.bar_visible(), "the bar is up", e.bar())
        check(NEEDLE in e.bar(), "seeded with the word under the caret", e.bar())
        check(e.counter() == (1, 12),
              "and it is on the FIRST of the twelve, not the next one down",
              f"counter {e.counter()}, bar {e.bar()!r}")
    finally:
        e.close()


def test_every_match_is_lit_and_the_active_one_differs(binary):
    print("\nEvery match on the page is lit, and the one you are on is not like the rest")
    e = Editor(binary)
    try:
        e.goto_needle(4)
        e.send(CTRL_F, 0.8)

        runs = e.highlights()
        lit = [r for r in runs if r[1] == NEEDLE]
        check(len(lit) >= 4,
              "several matches are highlighted at once, not just the current one",
              f"highlighted runs: {runs}")

        active = [r for r in lit if r[2] == "active"]
        others = [r for r in lit if r[2] == "match"]
        check(len(active) == 1,
              "exactly one match is drawn as the active one",
              f"active runs: {active}")
        check(len(others) >= 3,
              "and the others are drawn as plain matches",
              f"other runs: {others}")
        # The assertion that actually matters: they are DIFFERENT. A version
        # that painted every match identically would satisfy everything above
        # if 'active' and 'match' happened to collapse to one style.
        check(bool(active) and bool(others),
              "the active match is styled differently from the rest")
        check(active and active[0][0] == 4,
              "and it is the match the caret was on",
              f"active on line {active[0][0] if active else None}")
    finally:
        e.close()


def test_the_active_highlight_follows_the_navigation(binary):
    print("\nThe active highlight moves with you")
    e = Editor(binary)
    try:
        e.goto_needle(4)
        e.send(CTRL_F, 0.8)
        e.send(DOWN, 0.5)

        active = [r for r in e.highlights() if r[2] == "active"]
        check(len(active) == 1 and active[0][0] == 8,
              "after one step the active match is the next one down",
              f"active: {active}")
        check(e.counter() == (2, 12), "and the counter agrees", str(e.counter()))

        e.send(UP, 0.5)
        active = [r for r in e.highlights() if r[2] == "active"]
        check(len(active) == 1 and active[0][0] == 4,
              "and stepping back moves it back", f"active: {active}")
    finally:
        e.close()


def test_every_navigation_key_steps(binary):
    print("\nEvery key that names a direction steps through the matches")
    e = Editor(binary)
    try:
        e.goto_needle(4)
        e.send(CTRL_F, 0.8)

        # Each of these should advance the counter by exactly one. They are
        # checked one at a time from a known index rather than in a chain,
        # so a key that does nothing cannot be masked by the next one.
        for key, name in ((DOWN, "down"), (RIGHT, "right"), (PAGEDOWN, "pagedown"),
                          (ENTER, "enter"), (TAB, "tab"), (" ", "space")):
            before = e.counter()
            e.send(key, 0.45)
            after = e.counter()
            check(before is not None and after is not None
                  and after[0] == before[0] % 12 + 1,
                  f"{name} goes to the next match",
                  f"{before} -> {after}")
            check(e.bar_visible(), f"and the bar is still up after {name}", e.bar())

        for key, name in ((UP, "up"), (LEFT, "left"), (PAGEUP, "pageup"),
                          (SHIFT_ENTER, "shift-enter"), (SHIFT_TAB, "shift-tab"),
                          (SHIFT_SPACE, "shift-space")):
            before = e.counter()
            e.send(key, 0.45)
            after = e.counter()
            check(before is not None and after is not None
                  and after[0] == (before[0] - 2) % 12 + 1,
                  f"{name} goes to the previous match",
                  f"{before} -> {after}")
            check(e.bar_visible(), f"and the bar is still up after {name}", e.bar())
    finally:
        e.close()


def test_home_and_end_reach_the_ends_of_the_match_list(binary):
    print("\nHome and End are the first and last match")
    e = Editor(binary)
    try:
        e.goto_needle(20)
        e.send(CTRL_F, 0.8)
        e.send("\x1b[F", 0.6)            # End
        check(e.counter() == (12, 12), "End is the last match", str(e.counter()))
        e.send("\x1b[H", 0.6)            # Home
        check(e.counter() == (1, 12), "Home is the first", str(e.counter()))
    finally:
        e.close()


def test_the_bar_closes_only_on_ctrl_f_or_esc(binary):
    print("\nThe bar closes on a second Ctrl-F, or on ESC, and on nothing else")
    e = Editor(binary)
    try:
        e.goto_needle(4)
        e.send(CTRL_F, 0.8)
        for key, name in ((DOWN, "down"), (ENTER, "enter"), (TAB, "tab"), (" ", "space")):
            e.send(key, 0.4)
            check(e.bar_visible(), f"{name} does not close the bar", e.bar())

        e.send(CTRL_F, 0.6)
        check(not e.bar_visible(), "a second Ctrl-F closes it", e.bar())
        ln, _ = e.caret()
        check(ln in (4, 8, 12, 16, 20),
              "and leaves the caret on the match it was showing", f"line {ln}")

        e.send(CTRL_F, 0.8)
        check(e.bar_visible(), "Ctrl-F opens it again", e.bar())
        e.send(ESC, 0.6)
        check(not e.bar_visible(), "ESC closes it too", e.bar())
    finally:
        e.close()


def test_esc_clears_the_highlights_and_ctrl_f_keeps_them(binary):
    print("\nESC ends the search; Ctrl-F only puts the bar away")
    e = Editor(binary)
    try:
        e.goto_needle(4)
        e.send(CTRL_F, 0.8)
        e.send(CTRL_F, 0.6)
        check(len([r for r in e.highlights() if r[1] == NEEDLE]) >= 4,
              "after Ctrl-F the matches stay lit (n/N still walk them)")

        e.send(CTRL_F, 0.8)
        e.send(ESC, 0.6)
        check(not any(r[1] == NEEDLE for r in e.highlights()),
              "after ESC they are gone", str(e.highlights()))
    finally:
        e.close()


def test_typing_replaces_the_seeded_word(binary):
    print("\nThe seeded word behaves like selected text")
    e = Editor(binary)
    try:
        e.goto_needle(4)
        e.send(CTRL_F, 0.8)
        check(NEEDLE in e.bar(), "seeded", e.bar())

        e.send("p", 0.6)
        check(NEEDLE not in e.bar(),
              "the first character typed replaces the seed rather than extending it",
              e.bar())
        e.send("lain", 0.7)
        check(e.counter() is not None and e.counter()[1] == 36,
              "and the search follows what is now in the field",
              f"{e.counter()} bar {e.bar()!r}")
    finally:
        e.close()


def test_space_types_while_composing_and_steps_once_you_navigate(binary):
    print("\nSpace belongs to the field while you type, and to the matches after")
    e = Editor(binary)
    try:
        e.goto_needle(4)
        e.send(CTRL_F, 0.8)
        e.send("has", 0.7)
        n_before = e.counter()
        e.send(" ", 0.5)
        e.send("MATCH", 0.8)
        # The index is 2, not 1: "has MATCH" starts four characters to the
        # LEFT of where Ctrl-F anchored itself (the needle), so the anchor
        # sits inside the first occurrence and the search moves on to the
        # next. What matters here is that the space landed in the field.
        check("has MATCH" in e.bar() and e.counter() is not None
              and e.counter()[1] == 12,
              "a space typed mid-pattern is part of the pattern",
              f"before {n_before}, now {e.counter()}, bar {e.bar()!r}")

        e.send(DOWN, 0.45)          # navigating ends the composing run
        before = e.counter()
        e.send(" ", 0.45)
        after = e.counter()
        check(before and after and after[0] == before[0] % 12 + 1,
              "and once you have navigated, space steps instead",
              f"{before} -> {after}")
    finally:
        e.close()


def test_the_page_holds_still_between_visible_matches(binary):
    print("\nStepping between matches you can already see does not throw the page")
    e = Editor(binary)
    try:
        e.goto_needle(4)
        e.send(CTRL_F, 0.8)
        top = e.top_line()
        check(top == 1, "the document starts at the top", f"top line {top}")

        e.send(DOWN, 0.45)          # line 4 -> 8, both on screen
        check(e.top_line() == top,
              "the page did not move for a match already in view",
              f"{top} -> {e.top_line()}")

        # ...but it must still follow a match that is NOT in view. Without
        # this the previous assertion could be satisfied by never scrolling.
        for _ in range(6):
            e.send(DOWN, 0.35)
        moved = e.top_line()
        check(moved > top,
              "and it does follow one that is off screen",
              f"{top} -> {moved}")
        active = [r for r in e.highlights() if r[2] == "active"]
        check(len(active) == 1, "with the active match on screen", str(active))
    finally:
        e.close()


def test_ctrl_f_off_a_word_opens_empty(binary):
    print("\nCtrl-F on whitespace opens an empty bar ready to type")
    e = Editor(binary)
    try:
        e.send(CTRL_HOME, 0.4)
        for _ in range(3):
            e.send(DOWN, 0.05)
        for _ in range(11):          # column 12: the space before MATCH
            e.send(RIGHT, 0.04)
        e.drain(0.4)
        e.send(CTRL_F, 0.7)
        check(e.bar_visible(), "the bar is up", e.bar())
        check(NEEDLE not in e.bar(), "with nothing seeded", e.bar())
        e.send("MATCH", 0.9)
        check(e.counter() == (1, 12), "and typing searches", str(e.counter()))
    finally:
        e.close()


def test_the_bar_declines_what_it_does_not_own(binary):
    print("\nThe bar is one line, not a takeover: Ctrl-S still saves under it")
    e = Editor(binary)
    try:
        e.goto_needle(4)
        e.send("x", 0.4)                       # dirty the document
        e.send(CTRL_F, 0.8)
        check(e.bar_visible(), "bar up", e.bar())
        e.send(CTRL_S, 1.6)
        e.drain(1.0)
        saved = open(e.path).read()
        check("MATCHx" in saved or "MATCxH" in saved or "x" in saved.split("\n")[3],
              "Ctrl-S reached the editor while the bar was up",
              repr(saved.split("\n")[3]))
    finally:
        e.close()


def main():
    binary = find_binary()
    for fn in (test_ctrl_f_on_a_word_searches_for_that_word,
               test_every_match_is_lit_and_the_active_one_differs,
               test_the_active_highlight_follows_the_navigation,
               test_every_navigation_key_steps,
               test_home_and_end_reach_the_ends_of_the_match_list,
               test_the_bar_closes_only_on_ctrl_f_or_esc,
               test_esc_clears_the_highlights_and_ctrl_f_keeps_them,
               test_typing_replaces_the_seeded_word,
               test_space_types_while_composing_and_steps_once_you_navigate,
               test_the_page_holds_still_between_visible_matches,
               test_ctrl_f_off_a_word_opens_empty,
               test_the_bar_declines_what_it_does_not_own):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_find: FAILED ({len(failures)}): " + ", ".join(failures))
        return 1
    print("integration_find: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
