#!/usr/bin/env python3
"""
Integration test: mouse events must never reach the document as text.

Regression test for the prompt byte-leak bug. Prompts that read raw bytes
(goto, replace, rename, save-as, the yes/no quit prompts) treated the ESC
that opens an SGR mouse report as "cancel" and exited, leaving the rest of
the report in the tty. The main loop then read `[<0;30;10M` as ordinary
printable keys and typed it into the file -- so merely nudging the mouse
while a prompt was open corrupted the buffer. The fortress navigator had
the same defect, where the digits reached type-to-jump instead.

Drives the real binary in a pty and clicks once while each prompt is open.

Usage: python3 test/integration_mouse.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import os
import re
import shutil
import subprocess
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

# 20 identical-shaped lines, so any inserted junk is obvious in a diff
CONTENT = "".join(f"aaa{i:02d} alpha beta gamma\n" for i in range(1, 21))
ORIGINAL = CONTENT.splitlines()

failures = []


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}" + (f"  [{detail}]" if detail and not ok else ""))
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
    def __init__(self, binary, content, name="a.txt", args=None, extra_files=()):
        # HOME is nested one level down so the fortress navigator's parent
        # pane shows a directory we control rather than all of /tmp
        self.base = tempfile.mkdtemp(prefix="fac_mouse_")
        self.home = os.path.join(self.base, "home")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        self.target = os.path.join(self.home, name)
        with open(self.target, "w") as f:
            f.write(content)
        for extra in extra_files:
            with open(os.path.join(self.home, extra), "w") as f:
                f.write("hello\n")
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, args if args is not None else [self.target],
                                   dimensions=(ROWS, COLS), env=env, cwd=self.home,
                                   timeout=10)
        self.drain(1.5)

    def drain(self, wait=0.3):
        end = time.time() + wait
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def send(self, data, wait=0.4):
        self.child.send(data)
        self.drain(wait)

    def click(self, row, col, button=0):
        """1-based screen coords, SGR encoding."""
        self.child.send(f"\x1b[<{button};{col};{row}M")
        self.child.send(f"\x1b[<{button};{col};{row}m")
        self.drain(0.35)

    def wheel(self, row, col, up=True, times=1, button=None):
        b = button if button is not None else (64 if up else 65)
        for _ in range(times):
            self.child.send(f"\x1b[<{b};{col};{row}M")
        self.drain(0.5)

    def drag(self, row, col_from, col_to, button=0):
        self.child.send(f"\x1b[<{button};{col_from};{row}M")
        for c in range(col_from + 4, col_to + 1, 4):
            self.child.send(f"\x1b[<{button + 32};{c};{row}M")
        self.child.send(f"\x1b[<{button};{col_to};{row}m")
        self.drain(0.6)

    def menu_row(self, label):
        """Screen row of a menu entry, matched inside the box."""
        for y in range(ROWS):
            if label in self.screen.display[y] and "│" in self.screen.display[y]:
                return y + 1
        return None

    def menu_box(self):
        """(r0, r1, c0, c1) of the menu box, or None."""
        rs = [y + 1 for y in range(ROWS)
              if "┌" in self.screen.display[y] or "└" in self.screen.display[y]]
        if len(rs) < 2:
            return None
        r0, r1 = min(rs), max(rs)
        line = self.screen.display[r0 - 1]
        return r0, r1, line.index("┌") + 1, line.index("┐") + 1

    def highlighted(self):
        """Label of the reverse-video menu row. Scoped to the box's columns:
        the document is drawn on the same screen rows, to the left."""
        b = self.menu_box()
        if not b:
            return None
        r0, r1, c0, c1 = b
        for y in range(r0, r1 + 1):
            if any(self.screen.buffer[y - 1][x].reverse for x in range(c0, c1 - 1)):
                return "".join(self.screen.buffer[y - 1][x].data
                               for x in range(c0, c1 - 1)).strip()
        return None

    def hover(self, row, col):
        """Bare pointer motion: mode 1003 reports it as button 3 plus the
        motion bit."""
        self.child.send(f"\x1b[<35;{col};{row}M")
        self.drain(0.4)

    def menu_row_enabled(self, label):
        """False when the row is drawn dim. None when the row is absent."""
        row = self.menu_row(label)
        if row is None:
            return None
        c0 = self.screen.display[row - 1].index("│") + 1
        return self.screen.buffer[row - 1][c0 + 2].fg == "default"

    def tree_row(self, name):
        """Row of a name within the tree column only. Searching the whole
        screen would match the tab bar and the document, which is how an
        earlier version of this test right-clicked a tab by accident."""
        for y in range(1, ROWS):
            if name in self.screen.display[y][:30]:
                return y + 1
        return None

    def gutter_top(self):
        """First buffer line number visible in the leftmost gutter."""
        for y in range(2, ROWS):
            m = re.match(r"\s*(\d+)\s", self.screen.display[y - 1][:6])
            if m:
                return int(m.group(1))
        return None

    def row_text(self, row):
        return self.screen.display[row - 1].rstrip()

    def gutter_line(self, row):
        """The buffer line number the gutter shows on this screen row."""
        m = re.match(r"\s*(\d+)\s", self.screen.display[row - 1])
        return int(m.group(1)) if m else None

    def status_line_no(self):
        m = re.search(r"Ln (\d+)", self.screen.display[ROWS - 1])
        return int(m.group(1)) if m else None

    def status_ln_col(self):
        """Both coordinates: a click-through can move the column alone."""
        m = re.search(r"Ln (\d+), Col (\d+)", self.screen.display[ROWS - 1])
        return (int(m.group(1)), int(m.group(2))) if m else None

    def cursor_row(self):
        return self.screen.cursor.y + 1

    def tab_spans(self):
        """[(label, col0, col1)] for each tab drawn on row 1, 1-based."""
        bar = self.screen.display[0]
        return [(m.group(0), m.start() + 1, m.end())
                for m in re.finditer(r"\[\d+: [^\]]*\]", bar)]

    def active_tabs(self):
        """Labels drawn in reverse video: the tab bar marks the active one."""
        return [label for label, c0, c1 in self.tab_spans()
                if any(self.screen.buffer[0][x].reverse for x in range(c0 - 1, c1))]

    def tree_selected_rows(self):
        """Rows drawn in reverse video inside the tree column. Scanning the
        whole screen would also pick up the tab bar and the document."""
        out = []
        for y in range(ROWS):
            if any(self.screen.buffer[y][x].reverse for x in range(0, 26)):
                out.append(y + 1)
        return out

    def attributed_rows(self):
        """Rows carrying any non-default attribute: how the fortress panes
        mark their selection. Text alone does not change when it moves."""
        out = []
        for y in range(ROWS):
            text = self.screen.display[y].rstrip()
            if not text:
                continue
            for x in range(COLS):
                c = self.screen.buffer[y][x]
                if c.reverse or c.bold or c.fg != "default" or c.bg != "default":
                    out.append((y + 1, text[:34]))
                    break
        return out

    def find_row(self, needle):
        for y, r in enumerate(self.screen.display):
            if needle in r:
                return y + 1
        return None

    def save_and_read(self):
        self.child.send("\x13")
        self.drain(0.9)
        with open(self.target) as f:
            return f.read()

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.base, ignore_errors=True)


def changed_lines(text):
    out = []
    for i, line in enumerate(text.splitlines()):
        if i < len(ORIGINAL) and line != ORIGINAL[i]:
            out.append((i + 1, line))
    return out


# Prompts that read raw bytes: key, name, a marker proving it opened, and
# whether the buffer must be dirty first. The marker assertion is what keeps
# this test honest -- without it a prompt that silently failed to open would
# report a green "no leak".
#
# F2/Alt-N rename is not listed: with no language server for a .txt file it
# only prints a status message, so there is no prompt to click into. It uses
# show_text_prompt, the same loop as save-as below, which is covered.
PROMPTS = [
    ("\x07", "ctrl-g goto", "Go to (line:col)", False),
    ("\x12", "ctrl-r replace", "Replace:", False),
    ("\x06", "ctrl-f search", "[f]:", False),
    ("\x11", "ctrl-q quit with unsaved changes", "Unsaved changes", True),
    ("\x17", "ctrl-w close tab with unsaved changes", "Unsaved changes", True),
]


def test_close_pane_in_the_document_menu(binary):
    """Right-clicking a split document offers Close Pane.

    It acts on the pane that was CLICKED, which need not be the active one:
    opening the menu positions the caret, and positioning the caret in another
    pane focuses it, so by the time the row runs the clicked pane is active.
    """
    s = Session(binary, "aaa\nbbb\nccc\n")
    try:
        def pane_headers():
            # A split draws one labelled header per pane on row 2. An UNSPLIT
            # document has no header row at all -- row 2 is document text --
            # so "back to one pane" reads as zero headers, not one.
            return s.screen.display[1].count("[")

        s.click(6, 20, button=2)
        body = "\n".join(s.screen.display)
        check("Close Pane" not in body,
              "an unsplit document does not offer Close Pane", body[:200])
        s.send("\x1b", 0.5)

        s.send("\x1bv", 1.4)                      # alt-v: split vertically
        check(pane_headers() == 2, "the document is split in two",
              s.screen.display[1].rstrip())

        # The new pane is the active one, so click the LEFT pane: this closes
        # a pane that was not active when the menu opened.
        s.click(6, 12, button=2)
        body = "\n".join(s.screen.display)
        check("Close Pane" in body, "a split document offers it", body[:200])
        row = col = None
        for i, r in enumerate(s.screen.display):
            j = r.find("Close Pane")
            if j >= 0:
                row, col = i + 1, j + 1
                break
        if row is None:
            check(False, "found the row")
            return
        s.click(row, col + 2)
        s.drain(1.0)
        check(pane_headers() == 0, "choosing it leaves a single pane",
              s.screen.display[1].rstrip())
        check("aaa" in "\n".join(s.screen.display),
              "and the document is still open", s.screen.display[2].rstrip())
    finally:
        s.close()


def main():
    binary = find_binary()

    for seq, name, marker, needs_dirty in PROMPTS:
        s = Session(binary, CONTENT)
        row = s.find_row("aaa05")
        s.click(row, 6)
        if needs_dirty:
            s.send("Z", 0.4)          # dirty the buffer so the prompt appears
        s.send(seq, 1.2)

        opened = s.find_row(marker) is not None
        check(opened, f"{name}: prompt opened", f"no {marker!r} on screen")
        if not opened:
            s.close()
            continue

        s.click(10, 30)               # the event that used to corrupt the file
        s.wheel(10, 30)               # and a wheel notch, same encoding
        s.send("\x1b", 0.6)

        try:
            saved = s.save_and_read()
        except OSError as e:
            check(False, f"{name}: file readable after prompt", str(e))
            s.close()
            continue

        leaked = [c for c in changed_lines(saved) if "[<" in c[1]]
        check(not leaked, f"{name}: no mouse bytes typed into the buffer",
              str(leaked))
        s.close()

    # Save-As on an untitled buffer takes show_text_prompt, the same loop
    # rename uses.
    s = Session(binary, CONTENT)
    s.send("\x14", 0.8)               # ctrl-t: new untitled tab
    s.send("hello", 0.4)
    s.send("\x13", 1.2)               # ctrl-s: prompts for a filename
    check(s.find_row("Save as:") is not None, "save-as prompt opened",
          str(s.row_text(ROWS)))
    s.click(10, 30)
    s.wheel(10, 30)
    onscreen = [s.row_text(y) for y in range(1, ROWS + 1)]
    check(not any("[<" in r for r in onscreen),
          "save-as prompt: no mouse bytes echoed to the screen",
          str([r for r in onscreen if "[<" in r]))
    s.close()

    # Fortress navigator (welcome menu -> 'b' to browse). The old ESC handler
    # returned '<' as an unrecognised "arrow", after which the report's digits
    # reached type-to-jump one at a time and moved the selection. Two things
    # make this observable: the selection is marked by cell attributes, not by
    # the row's text, and the sandbox must contain names starting with the
    # digits a click's coordinates produce. The welcome menu is a separate
    # module and was never affected.
    s = Session(binary, CONTENT, args=[],
                extra_files=["0alpha.txt", "2beta.txt", "6gamma.txt", "zeta.txt"])
    s.drain(1.0)
    check(s.find_row("Welcome Menu") is not None, "welcome menu reached",
          str(s.row_text(2)))
    s.send("b", 1.4)                  # browse: opens the navigator
    check(s.find_row("FORTRESS") is not None, "navigator reached from menu",
          str(s.row_text(1)))

    sel = s.attributed_rows()
    check(len(sel) > 0, "navigator selection is detectable", "no attributed rows")
    s.send("\x1b[B", 0.7)             # a real arrow must still move it
    moved = s.attributed_rows()
    check(moved != sel, "navigator: arrow key still moves the selection")

    s.click(6, 20)                    # a click must not
    s.click(7, 20, button=2)
    after_click = s.attributed_rows()
    check(after_click == moved, "navigator: a click does not move the selection",
          f"{moved[:2]} -> {after_click[:2]}")
    s.wheel(6, 20)
    check(s.attributed_rows() == moved,
          "navigator: the wheel does not move the selection")
    check(not any("[<" in s.row_text(y) for y in range(1, ROWS + 1)),
          "navigator: no mouse bytes echoed")

    s.send("\x1b", 1.4)               # lone ESC must still quit
    check(not s.child.isalive(), "navigator: lone ESC still quits")
    s.close()

    # Split panes: a click must select the line it points at. The stored pane
    # rect used to be the header row while the caret renderers added the
    # header offset themselves, so every click in a split landed one line
    # low and the caret was drawn one row below the pointer.
    lines = "".join(f"line{i:02d} alpha beta\n" for i in range(1, 41))
    for split_key, label in (("", "unsplit"), ("\x1bv", "alt-v split"),
                             ("\x1bs", "alt-s split")):
        s = Session(binary, lines)
        if split_key:
            s.send(split_key, 1.2)
        wrong_line, wrong_caret, probed = [], [], 0
        for row in range(3, 16):
            shown = s.gutter_line(row)
            if shown is None:
                continue
            s.click(row, 12)
            probed += 1
            if s.status_line_no() != shown:
                wrong_line.append((row, shown, s.status_line_no()))
            if s.cursor_row() != row:
                wrong_caret.append((row, s.cursor_row()))
        check(probed >= 8, f"{label}: probed enough rows", f"only {probed}")
        check(not wrong_line, f"{label}: click selects the line it points at",
              str(wrong_line[:3]))
        check(not wrong_caret, f"{label}: caret is drawn on the clicked row",
              str(wrong_caret[:3]))
        s.close()

    # Tab bar: clicking a [n: name] region activates that tab. The layout
    # only ever existed during the draw -- label widths vary with the
    # filename and the modified marker, and in tree mode the bar does not
    # start at column 1 -- so the renderer now records each span.
    s = Session(binary, "AAA first\n", name="alpha.txt",
                extra_files=["beta.txt", "gamma.txt"])
    for name in ("beta.txt", "gamma.txt"):
        s.send("\x14", 0.7)               # ctrl-t: new tab
        s.send("\x0f", 1.0)               # ctrl-o: fortress navigator
        s.send(name, 0.8)                 # type-to-jump
        s.send("\r", 1.2)                 # open
    spans = s.tab_spans()
    check(len(spans) >= 3, "several tabs are open", f"{len(spans)} tabs")

    wrong = []
    for label, c0, c1 in spans:
        s.click(1, (c0 + c1) // 2)
        if s.active_tabs() != [label]:
            wrong.append((label, s.active_tabs()))
    check(not wrong, "clicking a tab activates it", str(wrong[:3]))

    # and the document really follows the tab, not just the highlight
    named = [(lbl, c0, c1) for lbl, c0, c1 in spans if "alpha.txt" in lbl]
    if named:
        s.click(1, (named[0][1] + named[0][2]) // 2)
        check("AAA first" in s.row_text(2),
              "the document follows the clicked tab", s.row_text(2))
    s.close()

    # Panels must swallow clicks instead of letting them move the caret in the
    # document behind. The document-symbols panel did not: a click on a symbol
    # row moved the caret, and typing afterwards edited the file while the
    # user was looking at a symbol list. Needs a language server, so each
    # panel self-skips if it will not open.
    csrc = ("#include <stdio.h>\n"
            "int alpha_one(int a) { return a; }\n"
            "int beta_two(int b) { return b; }\n"
            "int gamma_three(int c) { return c; }\n"
            "int delta_four(int d) { return d; }\n"
            "int main(void) { return 0; }\n")
    for open_key, marker, label in (("\x1bo", "Symbols", "document symbols"),
                                    ("\x1be", "Diagnostics", "diagnostics")):
        s = Session(binary, csrc, name="main.c")
        with open(os.path.join(s.home, "compile_flags.txt"), "w") as f:
            f.write("-std=c11\n")
        s.drain(3.5)                      # let the language server attach
        s.click(5, 10)
        before = s.status_ln_col()
        s.send(open_key, 2.5)
        if s.find_row(marker) is None:
            print(f"  ..  skip {label}: panel did not open (no language server?)")
            s.close()
            continue
        s.click(5, 80)                    # inside the panel's column strip
        s.send("\x1b", 1.0)               # close it and re-read the caret
        check(before is not None and s.status_ln_col() == before,
              f"{label} panel: a click does not move the document caret",
              f"{before} -> {s.status_ln_col()}")
        s.close()

    # The fuss-mode chevron leads the status bar, in the bottom-left corner
    # next to what it toggles. A multibyte filename is part of the check: the
    # bar is padded by display cells, and getting that wrong used to move the
    # chevron out from under its own clickable region.
    for fname, label in (("a.txt", "ascii filename"),
                         ("café_résumé.txt", "multibyte filename")):
        s = Session(binary, "hello world\nsecond line\n", name=fname)

        def chevron_col():
            row = s.screen.buffer[ROWS - 1]
            for c in range(1, COLS + 1):
                if row[c - 1].data in ("»", "«"):
                    return c
            return None

        def chevron_glyph():
            c = chevron_col()
            return s.screen.buffer[ROWS - 1][c - 1].data if c else None

        check(chevron_col() == 1, f"{label}: chevron sits in the first column",
              f"col {chevron_col()}")
        check(chevron_glyph() == "»", f"{label}: points right while the tree is closed",
              str(chevron_glyph()))

        s.click(ROWS, 1)
        check(chevron_glyph() == "«", f"{label}: clicking it opens the tree and flips it",
              str(chevron_glyph()))
        s.click(ROWS, 1)
        check(chevron_glyph() == "»", f"{label}: clicking again closes the tree",
              str(chevron_glyph()))
        s.close()

    # Clicking it must really move the tree, not just redraw the glyph
    s = Session(binary, "hello world\n", name="a.txt")
    plain = s.row_text(3)
    s.click(ROWS, 1)
    check(s.row_text(3) != plain, "the chevron click actually opens the file tree",
          f"row 3 unchanged: {plain!r}")
    s.close()

    # It is a control, not a status: a timed message must not displace it
    s = Session(binary, "hello world\n", name="a.txt")
    s.send("\x1bOQ", 1.2)                  # F2 rename with no language server
    bar = s.row_text(ROWS)
    check(s.screen.buffer[ROWS - 1][0].data in ("»", "«"),
          "the chevron survives a status message", bar[:50])
    s.close()

    # File tree rows. render_tree_node already carried the row next to the
    # item index, so each drawn row claims itself and a click delegates to the
    # keyboard handler: space expands a directory, enter opens a file. Before
    # this the tree was a total mouse dead zone.
    s = Session(binary, "CONTENT OF AAA\n", name="aaa.txt",
                extra_files=["bbb.txt"])
    with open(os.path.join(s.home, "bbb.txt"), "w") as f:
        f.write("CONTENT OF BBB\n")
    os.makedirs(os.path.join(s.home, "subdir"))
    with open(os.path.join(s.home, "subdir", "inner.txt"), "w") as f:
        f.write("INNER FILE\n")
    s.send("\x02", 1.8)                   # ctrl-b: open the tree

    row = s.find_row("bbb.txt")
    check(row is not None, "tree lists bbb.txt")
    if row:
        s.click(row, 6)
        check("CONTENT OF BBB" in "".join(s.screen.display),
              "clicking a file row opens that file")

    # Opening a file leaves the tree open, so subdir is still listed
    row = s.find_row("subdir")
    check(row is not None, "tree lists subdir")
    if row:
        check(s.find_row("inner.txt") is None, "subdir starts collapsed")
        s.click(row, 6)
        check(s.find_row("inner.txt") is not None,
              "clicking a directory row expands it")

    # Keyboard navigation must still work: the click path delegates to it
    before = s.tree_selected_rows()
    s.send("\x1b[B", 0.7)                 # down arrow
    check(before and s.tree_selected_rows() != before,
          "arrow keys still move the tree selection",
          f"{before} -> {s.tree_selected_rows()}")
    s.close()

    # The wheel must move what the pointer is over. Its coordinates used to be
    # discarded at decode time, so a tick could only ever move the active
    # pane -- scrolling over the tab bar, the status bar or an inactive pane
    # moved a document the pointer was not on.
    long_file = "".join(f"line{i:03d} alpha beta gamma\n" for i in range(1, 200))
    s = Session(binary, long_file)

    top = s.gutter_top()
    s.wheel(10, 30, up=False, times=2)
    check(top is not None and s.gutter_top() > top,
          "wheel over the document scrolls it", f"{top} -> {s.gutter_top()}")

    for row, where in ((1, "tab bar"), (ROWS, "status bar")):
        top = s.gutter_top()
        s.wheel(row, 30, up=False, times=2)
        check(s.gutter_top() == top, f"wheel over the {where} scrolls nothing",
              f"{top} -> {s.gutter_top()}")

    # A modified wheel used to fall into the shift/alt/ctrl click branches,
    # which have no handler, so it did nothing at all
    top = s.gutter_top()
    s.wheel(10, 30, times=2, button=81)      # ctrl + wheel down
    check(s.gutter_top() != top, "ctrl+wheel still scrolls",
          f"{top} -> {s.gutter_top()}")
    s.close()

    # In a vertical split the right pane is active, so the left one is the
    # inactive pane: wheeling over it must move it and not the active one.
    s = Session(binary, long_file)
    s.send("\x1bv", 1.3)
    top = s.gutter_top()
    s.wheel(6, 20, up=False, times=3)
    check(top is not None and s.gutter_top() is not None and s.gutter_top() > top,
          "wheel over the inactive pane scrolls that pane",
          f"{top} -> {s.gutter_top()}")
    s.close()

    # Only the left button drags out a selection. Mouse mode 1002 reports
    # motion whichever button is held, so a right- or middle-drag used to
    # arrive at the same handler as button 34/33 and silently start selecting.
    def drag_selects(button):
        s = Session(binary, "".join(f"line{i:02d} alpha beta gamma\n"
                                    for i in range(1, 15)))
        s.child.send(f"\x1b[<{button};10;5M")
        for c in (14, 18, 22):
            s.child.send(f"\x1b[<{button + 32};{c};5M")
        s.drain(0.6)
        # A right-press opens the context menu, whose selected row is drawn in
        # reverse video. Skip any row carrying box glyphs so the menu's own
        # highlight is not counted as a document selection.
        boxy = {y for y in range(ROWS)
                if any(g in s.screen.display[y] for g in ("┌", "│", "└", "├"))}
        cells = sum(1 for y in range(1, ROWS - 1) for x in range(COLS)
                    if y not in boxy and s.screen.buffer[y][x].reverse)
        s.child.send(f"\x1b[<{button};22;5m")
        s.drain(0.3)
        s.close()
        return cells

    check(drag_selects(0) > 0, "left-drag still selects")
    check(drag_selects(2) == 0, "right-drag does not select")
    check(drag_selects(1) == 0, "middle-drag does not select")

    # Right-click context menu. The document is filled with a character that
    # appears nowhere in the menu, so anything of it seen inside the box is a
    # hole: the existing boxes place their right border with a separate cursor
    # move and leave the gap unpainted, which this must not do.
    filler = "@" * 45
    s = Session(binary, "".join(f"{filler} line{i:02d}\n" for i in range(1, 25)))

    def box_rows():
        return [y + 1 for y in range(ROWS)
                if any(g in s.screen.display[y] for g in ("┌", "│", "└", "├"))]

    s.click(6, 20, button=2)
    rows = box_rows()
    check(len(rows) >= 8, "right-click opens the menu", str(rows))

    # It must survive the frame it was opened on. The right-button release
    # arrives in the same coalesced burst and used to dismiss it instantly.
    s.drain(1.0)
    check(box_rows() == rows, "the menu is still there on the next frame",
          f"{rows} -> {box_rows()}")

    if rows:
        r0, r1 = min(rows), max(rows)
        c0 = s.screen.display[r0 - 1].index("┌") + 1
        wid = len(s.screen.display[r0 - 1][c0 - 1:].split("┐")[0]) + 1
        leaked = [r for r in range(r0, r1 + 1)
                  if "@" in "".join(s.screen.buffer[r - 1][x].data
                                    for x in range(c0 - 1, c0 - 1 + wid))]
        check(not leaked, "the box fully occludes the document", str(leaked))
        check(c0 == 20 and r0 == 6, "the pointer cell is the top-left corner",
              f"row {r0} col {c0}")

    s.send("\x1b", 0.6)
    check(not box_rows(), "escape dismisses the menu")

    # A left click away from the menu dismisses it and does NOT also move the
    # caret to wherever the user aimed to close it.
    s.click(8, 30, button=0)
    s.click(6, 20, button=2)
    check(len(box_rows()) >= 8, "menu reopens")
    # Baseline after opening: the right-click itself moves the caret, so this
    # isolates the dismissing click.
    before = s.status_ln_col()
    s.click(20, 70, button=0)
    check(not box_rows(), "a click away dismisses the menu")
    check(s.status_ln_col() == before,
          "and does not move the caret", f"{before} -> {s.status_ln_col()}")

    # A tab now has its own menu -- this used to assert the opposite, back
    # when the tab bar was the one clickable surface that swallowed a right
    # click. The status bar still opens nothing.
    s.click(1, 5, button=2)
    check(len(box_rows()) >= 4, "a tab opens its own menu")
    check("Close Tab" in "\n".join(s.screen.display),
          "and it offers Close Tab")
    s.send("\x1b", 0.6)
    check(not box_rows(), "escape dismisses it")
    s.click(ROWS, 5, button=2)
    check(not box_rows(), "no menu on the status bar")
    s.close()

    # Ctrl-Q closes the menu before it quits, and cannot be trapped by it
    s = Session(binary, "hello\n")
    s.click(6, 20, button=2)
    check(len(box_rows()) >= 8, "menu open before ctrl-q")
    s.send("\x11", 1.0)
    check(s.child.isalive() and not box_rows(),
          "ctrl-q closes the menu rather than quitting")
    s.send("\x11", 1.5)
    check(not s.child.isalive(), "the next ctrl-q quits")
    s.close()

    # Menu rows actually run. Dispatch goes through the real key handler
    # rather than the leaf routines, so undo state, doc revision and the
    # ghost refresh all happen exactly as they do for the keystroke.
    lines3 = "alpha bravo charlie\ndelta echo foxtrot\ngolf hotel india\n" * 5

    s = Session(binary, lines3)
    s.click(4, 8)
    before = len(s.save_and_read().splitlines())
    s.click(6, 30, button=2)
    row = s.menu_row("Cut Line")
    check(row is not None, "with no selection the row reads 'Cut Line'")
    if row:
        s.click(row, 32)
        after = len(s.save_and_read().splitlines())
        check(after == before - 1, "clicking Cut Line removes a line",
              f"{before} -> {after}")
    s.close()

    # Copy Line then Paste puts the text back into the buffer: proves both
    # rows really ran, which a label check alone would not. The copy carries
    # no trailing newline, so it pastes inline and the line count is
    # unchanged -- the text getting longer is the signal.
    s = Session(binary, lines3)
    s.click(4, 8)
    before = s.save_and_read()
    s.click(6, 30, button=2)
    row = s.menu_row("Copy Line")
    check(row is not None, "the menu offers Copy Line")
    if row:
        s.click(row, 32)
        s.click(6, 30, button=2)
        row = s.menu_row("Paste")
        check(row is not None, "the menu offers Paste")
        if row:
            s.click(row, 32)
            after = s.save_and_read()
            check(len(after) > len(before),
                  "Copy Line then Paste puts the text back",
                  f"{len(before)} -> {len(after)} chars")
    s.close()

    # The caret rule: inside a selection nothing moves, outside it the caret
    # follows the pointer.
    s = Session(binary, lines3)
    s.click(3, 7)
    s.drag(3, 7, 18)
    pinned = s.status_ln_col()
    s.click(3, 12, button=2)
    check(s.menu_row("Cut Line") is None,
          "with a selection the row reads 'Cut', not 'Cut Line'")
    check(s.status_ln_col() == pinned,
          "a right-click inside the selection leaves the caret alone",
          f"{pinned} -> {s.status_ln_col()}")
    s.send("\x1b", 0.5)

    s.click(3, 7)
    moved_from = s.status_ln_col()
    s.click(7, 15, button=2)
    check(s.status_ln_col() != moved_from,
          "a right-click outside any selection moves the caret",
          f"{moved_from} -> {s.status_ln_col()}")
    s.close()

    # Keyboard drives the menu too
    s = Session(binary, lines3)
    s.click(4, 8)
    s.click(6, 30, button=2)
    check(s.menu_row("Cut Line") is not None, "menu open for keyboard test")
    s.send("\x1b[B", 0.4)
    s.send("\r", 0.8)
    check(s.menu_row("Copy Line") is None and s.menu_row("Cut Line") is None,
          "enter activates a row and closes the menu")
    s.close()

    # Rows that cannot work are greyed and inert rather than hidden
    s = Session(binary, lines3)
    s.click(6, 20, button=2)
    row = s.menu_row("Go to Definition")
    check(row is not None, "the LSP rows are listed even without a server")
    if row:
        c0 = s.screen.display[row - 1].index("│") + 1
        greyed = s.screen.buffer[row - 1][c0 + 2].fg != "default"
        check(greyed, "Go to Definition is greyed without a language server",
              str(s.screen.buffer[row - 1][c0 + 2].fg))
        s.click(row, c0 + 5)
        check(s.menu_row("Go to Definition") is not None,
              "clicking a disabled row does nothing and leaves the menu open")
    s.close()

    # With a language server the same rows are live. Self-skips without one.
    s = Session(binary,
                "#include <stdio.h>\n"
                "int alpha_one(int a) { return a; }\n"
                "int beta_two(int b) { return alpha_one(b); }\n"
                "int main(void) { return beta_two(1); }\n", name="main.c")
    with open(os.path.join(s.home, "compile_flags.txt"), "w") as f:
        f.write("-std=c11\n")
    s.drain(4.0)
    s.click(5, 25, button=2)
    row = s.menu_row("Go to Definition")
    if row:
        c0 = s.screen.display[row - 1].index("│") + 1
        if s.screen.buffer[row - 1][c0 + 2].fg == "default":
            check(True, "Go to Definition is live with a language server")
        else:
            print("  ..  skip LSP-enabled check: no language server attached")
    s.send("\x1b", 0.5)

    # The Command Palette row hands control to a blocking loop, so the menu
    # must be gone from the screen before that loop starts drawing.
    s.click(5, 25, button=2)
    row = s.menu_row("Command Palette")
    check(row is not None, "the menu offers the Command Palette")
    if row:
        c0 = s.screen.display[row - 1].index("│") + 1
        s.click(row, c0 + 5)
        # "Cut Line" is menu-only text; the palette lists "Cut"
        leftovers = any("Cut Line" in s.screen.display[y] for y in range(ROWS))
        check(not leftovers, "no menu residue behind the palette")
        s.send("\x1b", 1.0)
        check(s.child.isalive(), "the editor survives the palette round trip")
    s.close()

    # The menu is reachable from the keyboard, anchored at the caret. No caret
    # rule applies there: the caret is already where the user put it.
    s = Session(binary, "alpha bravo charlie\n" * 12)
    s.click(5, 12)
    pinned = s.status_ln_col()
    for seq, name in (("\x1b[21;2~", "shift-f10"), ("\x1bz", "alt-z")):
        s.send(seq, 0.9)
        check(s.menu_row("Cut Line") is not None, f"{name} opens the menu")
        check(s.status_ln_col() == pinned, f"{name} leaves the caret alone",
              f"{pinned} -> {s.status_ln_col()}")
        s.send("\x1b", 0.5)
    s.close()

    # Tree menu. Git rows are greyed from the per-file status the tree already
    # tracks, and invoked by setting the ctrl-g prefix and sending the letter,
    # so the menu and the keyboard cannot diverge.
    if shutil.which("git") is None:
        print("  ..  skip tree context menu: git not available")
    else:
        s = Session(binary, "open me\n", name="anchor.md")

        def git(*args):
            return subprocess.run(("git",) + args, cwd=s.home,
                                  capture_output=True, text=True)

        git("init", "-q")
        git("config", "user.email", "t@t")
        git("config", "user.name", "t")
        git("add", "anchor.md")
        git("commit", "-qm", "init")
        with open(os.path.join(s.home, "zulu.txt"), "w") as f:
            f.write("new\n")                       # untracked
        with open(os.path.join(s.home, "yankee.txt"), "w") as f:
            f.write("s\n")
        git("add", "yankee.txt")                   # staged
        os.makedirs(os.path.join(s.home, "sub"))
        with open(os.path.join(s.home, "sub", "i.txt"), "w") as f:
            f.write("i\n")

        s.send("\x02", 1.8)                        # ctrl-b: open the tree

        for name, want_stage, want_unstage in (("zulu.txt", True, False),
                                               ("yankee.txt", False, True)):
            row = s.tree_row(name)
            check(row is not None, f"{name} appears in the tree")
            if row:
                s.click(row, 6, button=2)
                check(s.menu_row_enabled("Stage") == want_stage,
                      f"{name}: Stage is {'live' if want_stage else 'greyed'}",
                      str(s.menu_row_enabled("Stage")))
                check(s.menu_row_enabled("Unstage") == want_unstage,
                      f"{name}: Unstage is {'live' if want_unstage else 'greyed'}",
                      str(s.menu_row_enabled("Unstage")))
                s.send("\x1b", 0.6)

        row = s.tree_row("sub")
        if row:
            s.click(row, 6, button=2)
            check(s.menu_row("Expand or Collapse") is not None,
                  "a directory row offers Expand or Collapse")
            check(s.menu_row("Open to the Side") is None,
                  "a directory row offers no splits")
            check(s.menu_row("Stage") is None,
                  "a directory row offers no git actions")
            s.send("\x1b", 0.6)

        # And the action really runs
        row = s.tree_row("zulu.txt")
        if row:
            s.click(row, 6, button=2)
            mr = s.menu_row("Stage")
            if mr:
                c0 = s.screen.display[mr - 1].index("│") + 1
                s.click(mr, c0 + 3)
                out = git("status", "--porcelain", "zulu.txt").stdout.strip()
                check(out.startswith("A"), "clicking Stage stages the file",
                      f"git says {out!r}")
        s.close()

    # With the tree open, a click in the document takes focus. Fuss mode is
    # modal for the keyboard, so leaving the tree open with the caret in the
    # document would send the next keystroke to the tree's fuzzy search.
    s = Session(binary, "".join(f"line{i:02d} alpha beta\n" for i in range(1, 15)),
                name="doc.txt", extra_files=["other.txt"])
    s.send("\x02", 1.6)
    check(s.tree_row("other.txt") is not None, "tree opens")
    before = s.status_ln_col()
    s.click(6, 60)                             # the document half
    check(s.tree_row("other.txt") is None, "clicking the document closes the tree")
    check(s.status_ln_col() != before, "and puts the caret where it was clicked",
          f"{before} -> {s.status_ln_col()}")
    s.send("Z", 0.6)
    check("Z" in s.save_and_read(), "typing afterwards edits the document")

    # A click on a tree row is still a tree click, not a focus change
    s.send("\x02", 1.5)
    row = s.tree_row("other.txt")
    if row:
        s.click(row, 6)
        check(any("other" in s.row_text(y) for y in range(1, 6)),
              "clicking a tree row still opens that file")
    s.close()

    # Modifier clicks. The decoder classifies by modifier before button, so a
    # right-click with anything held arrives as mouse-ctrl:/mouse-shift:/
    # mouse-alt: rather than mouse-click: and used to be dropped entirely.
    # Ctrl+left-click opens the menu too, the macOS convention.
    s = Session(binary, "".join(f"line{i:02d} alpha beta\n" for i in range(1, 15)))

    def opens_menu(button):
        s.click(4, 10)                          # reset somewhere harmless
        s.click(7, 30, button=button)
        got = s.menu_row("Cut Line") is not None or s.menu_row("Cut") is not None
        if got:
            s.send("\x1b", 0.4)
        return got

    check(opens_menu(2), "plain right-click opens the menu")
    check(opens_menu(16), "ctrl+left-click opens the menu")
    check(opens_menu(18), "ctrl+right-click opens the menu")
    check(opens_menu(6), "shift+right-click opens the menu")
    check(not opens_menu(4), "shift+left-click stays unbound")
    check(not opens_menu(8), "alt+left-click stays the multi-cursor toggle")

    # A ctrl-drag must not spawn a menu per motion event
    s.click(4, 10)
    s.child.send("\x1b[<16;10;8M")
    for c in (14, 18, 22, 26):
        s.child.send(f"\x1b[<48;{c};8M")
    s.drain(0.8)
    s.child.send("\x1b[<16;26;8m")
    s.drain(0.5)
    boxes = sum(1 for y in range(ROWS) if "┌" in s.screen.display[y])
    check(boxes <= 1, "a ctrl-drag does not stack menus", f"{boxes} boxes")
    s.close()

    # Hover. The mouse and the arrow keys drive the same highlight, which
    # needs the terminal to report bare pointer motion (mode 1003) -- fac runs
    # in 1002 otherwise, where motion is only reported while a button is held.
    s = Session(binary, "".join(f"line{i:02d} alpha beta\n" for i in range(1, 20)))
    s.click(4, 20, button=2)
    check(s.highlighted() is not None and s.highlighted().startswith("Cut"),
          "the first row starts highlighted", str(s.highlighted()))

    # Always-enabled rows, so this does not depend on a language server
    for label in ("Paste", "Command Palette", "Copy Line"):
        row = s.menu_row(label)
        if row:
            s.hover(row, 25)
            got = s.highlighted()
            check(got is not None and got.startswith(label),
                  f"hovering {label} highlights it", str(got))

    # A disabled row, a separator, and the space outside must all leave it be
    disabled = next((lbl for lbl in ("Go to Definition", "Find References",
                                     "Toggle Comment")
                     if s.menu_row_enabled(lbl) is False), None)
    if disabled:
        before = s.highlighted()
        s.hover(s.menu_row(disabled), 25)
        check(s.highlighted() == before, "hovering a disabled row moves nothing",
              f"{before} -> {s.highlighted()}")
    else:
        print("  ..  skip disabled-row hover: every row is live here")

    before = s.highlighted()
    sep = next((y + 1 for y in range(ROWS) if "├" in s.screen.display[y]), None)
    if sep:
        s.hover(sep, 25)
        check(s.highlighted() == before, "hovering a separator moves nothing",
              f"{before} -> {s.highlighted()}")
    s.hover(2, 90)
    check(s.highlighted() == before, "hovering off the menu moves nothing",
          f"{before} -> {s.highlighted()}")

    # Keyboard still drives the same highlight
    before = s.highlighted()
    s.send("\x1b[B", 0.5)
    check(s.highlighted() != before, "arrow keys still move the highlight",
          f"{before} -> {s.highlighted()}")
    s.send("\x1b", 0.5)
    s.close()

    # Clicking a row acts on THAT row, whatever was highlighted beforehand
    s = Session(binary, "".join(f"line{i:02d} alpha beta\n" for i in range(1, 20)))
    s.click(4, 20)
    before_text = s.save_and_read()
    s.click(6, 30, button=2)
    s.hover(s.menu_row("Paste"), 32)           # highlight is on Paste
    row = s.menu_row("Cut Line")
    if row:
        s.click(row, 32)                        # but Cut Line is clicked
        after = s.save_and_read()
        check(len(after.splitlines()) == len(before_text.splitlines()) - 1,
              "clicking a row runs that row, not the highlighted one",
              f"{len(before_text.splitlines())} -> {len(after.splitlines())}")

    # Motion tracking must be turned off with the menu: drag-select still works
    s.drag(3, 7, 22)
    cells = sum(1 for y in range(1, ROWS - 1) for x in range(COLS)
                if s.screen.buffer[y][x].reverse)
    check(cells > 0, "drag-select still works after a menu has been open",
          f"{cells} highlighted cells")
    s.close()

    test_close_pane_in_the_document_menu(binary)

    if failures:
        print(f"integration_mouse: FAILED ({len(failures)}: {', '.join(failures)})")
        sys.exit(1)
    print("integration_mouse: ALL PASSED")


if __name__ == "__main__":
    main()
