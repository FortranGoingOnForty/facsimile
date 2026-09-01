#!/usr/bin/env python3
"""
Integration test: Ctrl-O opens the file browser as a WINDOW, not a takeover.

It used to blank the terminal, paint a full-screen browser and run its own
blocking input loop. Nothing else could draw or respond while it was up, and
returning was a hard cut rather than dismissing a dialog.

It is now a box drawn into the editor frame and driven by the main loop, on
the same footing as the tab group dialog: show/hide, a key handler that
claims or declines, a render, and a result the caller acts on.

The sharpest assertion here is that the document SURVIVES around the box.
The display used to finish rows with ESC[K, which clears to the end of the
terminal line rather than the box -- inside a window that erases the document
beside it, the same defect once found in the ghost-text block.

`fac` with no arguments still gets the full-screen browser; there is no
editor for it to be a window inside.

Usage: python3 test/integration_fortress_modal.py [path-to-fac-binary]
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

ROWS, COLS = 28, 110
CTRL_O = "\x0f"
SHIFT_ENTER = "\x1b[13;2u"

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


class Editor:
    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_fm_home_")
        os.makedirs(os.path.join(self.home, ".config", "fac"))
        with open(os.path.join(self.home, ".config", "fac", "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        with open(os.path.join(self.home, ".config", "fac", "settings.json"), "w") as f:
            f.write('{"ui.theme": "steel", "ui.color_mode": "truecolor"}\n')
        self.work = tempfile.mkdtemp(prefix="fac_fm_work_")
        os.makedirs(os.path.join(self.work, "ch5"))
        for n in ("alpha.c", "beta.c"):
            with open(os.path.join(self.work, "ch5", n), "w") as f:
                f.write("int %s;\n" % n[:-2])
        self.target = os.path.join(self.work, "top.c")
        # Long enough to run PAST the right edge of a centred window. With
        # short lines there is nothing to the right of the box, and a stray
        # ESC[K -- which clears rightward -- would damage nothing observable.
        with open(self.target, "w") as f:
            # The marker must start BEYOND the window's right edge (a centred
            # 70% box on 110 columns ends near column 93, plus two of shadow),
            # or it sits under the box and proves nothing.
            # Valid C, so no language server diagnostics arrive mid-test and
            # change the status bar under us.
            f.writelines("int line%02d_%s = %d; // RIGHTEDGE%02d\n"
                         % (i, "x" * 70, i, i) for i in range(1, 25))

        env = {**os.environ, "TERM": "xterm-256color", "COLORTERM": "truecolor",
               "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        env.pop("NO_COLOR", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.work)
        self.drain(2.5)

    def drain(self, w=0.6):
        end = time.time() + w
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def send(self, d, w=0.8):
        self.child.send(d)
        self.drain(w)

    def text(self):
        return "\n".join(r.rstrip() for r in self.screen.display)

    def doc_lines(self):
        """Document lines still legible on screen, by their gutter number."""
        out = []
        for y in range(1, ROWS - 1):
            row = self.screen.display[y]
            head = row[:12]
            if "int line" in row or head.strip().isdigit():
                out.append(head.rstrip())
        return out

    def selected_row(self):
        """The active Fortress entry is bold and underlined, not reversed."""
        for y in range(1, ROWS - 1):
            cells = [self.screen.buffer[y][x] for x in range(COLS)]
            selected = [c for c in cells if c.bold and c.underscore]
            if selected:
                return y, "".join(c.data for c in selected).strip()
        return None, None

    def has_box(self):
        return "╭" in self.text() and "FORTRESS" in self.text()

    def close(self):
        self.child.close(force=True)
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def test_it_is_a_window_over_the_document(binary):
    print("\nCtrl-O draws a window, and the document survives around it")
    s = Editor(binary)
    try:
        before = s.doc_lines()
        check(len(before) > 10, "the document is on screen to begin with",
              str(len(before)))

        s.send(CTRL_O, 1.8)
        check(s.has_box(), "a bordered FORTRESS window appeared", s.text()[:200])

        during = s.doc_lines()
        # The box covers part of the width, so the gutter and the left of each
        # line must still be there. This is the ESC[K assertion.
        check(len(during) >= len(before) - 2,
              "the document rows are still drawn beside the box",
              f"{len(before)} -> {len(during)}")
        check("int line01" in s.text(),
              "text to the LEFT of the box survives", s.text()[:160])
        # The real ESC[K assertion. Clear-to-end-of-line wipes RIGHTWARD, so
        # it can only be seen on a row the box actually spans -- checking the
        # whole screen passes on the untouched rows above and below it.
        top = bottom = None
        for y in range(ROWS):
            if "╭" in s.screen.display[y]:
                top = y
            elif "╰" in s.screen.display[y]:
                bottom = y
        check(top is not None and bottom is not None and bottom > top + 1,
              "found the box's rows", f"{top}..{bottom}")
        if top is not None and bottom is not None:
            top_line = s.screen.display[top]
            left = top_line.index("╭")
            right = top_line.rindex("╮")
            interior = [s.screen.buffer[y][x]
                        for y in range(top + 1, bottom)
                        for x in range(left + 1, right)]
            panel_bg = interior[0].bg if interior else "default"
            check(interior and panel_bg != "default" and
                  all(cell.bg == panel_bg for cell in interior),
                  "the Fortress body is one continuous panel surface",
                  sorted({cell.bg for cell in interior}))
            spanned = [s.screen.display[y] for y in range(top + 1, bottom)]
            check(any("RIGHTEDGE" in r for r in spanned),
                  "document text right of the box survives on its own rows",
                  repr(spanned[:2]))
    finally:
        s.close()


def test_esc_dismisses_and_restores(binary):
    print("\nEsc closes the window and leaves the document as it was")
    s = Editor(binary)
    try:
        before = s.text()
        s.send(CTRL_O, 1.8)
        check(s.has_box(), "window open")
        s.send("\x1b", 1.4)
        check(not s.has_box(), "window gone", s.text()[:200])
        # Everything but the status bar: that line carries transient
        # messages and asynchronous diagnostics, which say nothing about
        # whether the window cleaned up after itself.
        def body(t):
            return "\n".join(t.split("\n")[:ROWS - 1])
        check(body(s.text()) == body(before),
              "the document is exactly as it was", "differs")
    finally:
        s.close()


def test_arrows_move_the_window_not_the_document(binary):
    print("\nArrows drive the window while it is up")
    s = Editor(binary)
    try:
        s.send(CTRL_O, 1.8)
        first = s.selected_row()
        check(first[0] is not None, "something is selected to begin with")
        if first[0] is not None:
            selected_cells = [s.screen.buffer[first[0]][x] for x in range(COLS)
                              if s.screen.buffer[first[0]][x].bold and
                              s.screen.buffer[first[0]][x].underscore]
            check(selected_cells and not any(cell.reverse for cell in selected_cells),
                  "the selection is bold and underlined without reverse video")
        s.send("\x1b[B", 0.7)
        after = s.selected_row()
        check(after != first, "the selection moved on the arrow",
              f"{first} -> {after}")
        check(s.has_box(), "and is still open")
        # The document behind must not have scrolled.
        check("int line01" in s.text(),
              "the document did not scroll underneath", s.text()[:200])
    finally:
        s.close()


def test_enter_on_a_file_opens_it(binary):
    print("\nEnter on a file opens it and closes the window")
    s = Editor(binary)
    try:
        s.send(CTRL_O, 1.8)
        # walk to top.c in the right-hand pane
        for _ in range(12):
            if "top.c" in s.text():
                break
            s.send("\x1b[B", 0.25)
        # select it: move down until the selection lands on it, then Enter
        for _ in range(12):
            s.send("\x1b[B", 0.25)
            if s.screen.display[0].count("top.c") >= 1:
                break
        s.send("\r", 1.6)
        check(not s.has_box(), "the window closed", s.text()[:200])
    finally:
        s.close()


def test_ctrl_q_is_not_trapped(binary):
    print("\nCtrl-Q still quits from inside the window")
    s = Editor(binary)
    try:
        s.send(CTRL_O, 1.8)
        check(s.has_box(), "window open")
        s.send("\x11", 1.5)
        s.drain(1.0)
        check(not s.child.isalive() or "save" in s.text().lower()
              or not s.has_box(),
              "ctrl-q was not swallowed by the window", s.text()[-200:])
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_it_is_a_window_over_the_document,
               test_esc_dismisses_and_restores,
               test_arrows_move_the_window_not_the_document,
               test_enter_on_a_file_opens_it,
               test_ctrl_q_is_not_trapped):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_fortress_modal: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_fortress_modal: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
