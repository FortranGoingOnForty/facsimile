#!/usr/bin/env python3
"""
Integration test: the caret-move fast path must be invisible.

A plain arrow key repaints only the few lines whose appearance changed
instead of the whole screen -- roughly 300 bytes instead of 3300, which is
the difference between comfortable and unusable when the terminal is at the
far end of an ssh link.

The property that has to hold is equivalence, not speed: for the same keys,
the screen must end up byte-for-byte what a full repaint would have produced.
This drives the same input twice in one binary -- once normally, once with
the fast path defeated by an edit that forces a full frame -- and compares
text, every cell attribute, and the caret.

Defeating the fast path without a second binary keeps this runnable on any
checkout: the comparison run ends with ctrl-l, which clears and fully redraws
without touching the caret, the selection or the buffer.

Usage: python3 test/integration_fastpath.py [path-to-fac-binary]
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

ROWS, COLS = 30, 100

# Brackets and indentation, so bracket-match highlighting and the
# current-line gutter both come into play.
SRC = """program demo
    integer :: alpha(10)
    if (alpha(1) > 0) then
        call thing(alpha(2), beta)
    end if
    do i = 1, 10
        alpha(i) = (i * 2) + (i - 1)
    end do
end program demo
""" + "".join(f"    ! filler line {i:03d}\n" for i in range(1, 60))

# Block comments spanning lines. Tokenizing carries in_multiline_comment from
# one line to the next, so repainting a line in isolation gets it wrong: a
# continuation line comes out as plain code, and a line that opens a comment
# leaves the flag set and colours everything after it as comment. The Fortran
# fixture above cannot catch that -- it has only `!` line comments.
SRC_C = """/* A block comment that runs
   across several lines, which is
   the whole point of this fixture. */
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

/* A second block comment,
   also spanning lines. */
int mul(int a, int b) {
    return a * b;
}

int main(void) {
    printf("%d\\n", add(1, 2));
    printf("%d\\n", mul(3, 4));
    return 0;
}
""" + "".join(f"// filler line {i:03d}\n" for i in range(1, 40))

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


def snapshot(binary, keys, force_full, src=None, name="d.f90"):
    """Drive the keys and return (text, attributes, caret).

    The filename shows in the status bar, so both runs must use the same
    path or every screen differs for no interesting reason.
    """
    root = "/tmp/fac_fastpath_cmp"
    shutil.rmtree(root, ignore_errors=True)
    home = os.path.join(root, "home")
    os.makedirs(os.path.join(home, ".config", "fac"))
    with open(os.path.join(home, ".config", "fac", "state.json"), "w") as f:
        f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                ' "version": "1.0"}\n')
    target = os.path.join(home, name)
    with open(target, "w") as f:
        f.write(SRC if src is None else src)
    env = {**os.environ, "TERM": "xterm-256color", "HOME": home}
    env.pop("XDG_CONFIG_HOME", None)
    screen = pyte.Screen(COLS, ROWS)
    stream = pyte.Stream(screen)
    child = pexpect.spawn(binary, [target], dimensions=(ROWS, COLS), env=env,
                          cwd=home, timeout=15)

    def drain(wait=0.4):
        end = time.time() + wait
        while time.time() < end:
            try:
                stream.feed(child.read_nonblocking(65536, 0.08)
                            .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    drain(1.8)
    for k in keys:
        child.send(k)
        drain(0.25)
    if force_full:
        # ctrl-l clears and repaints everything, and changes no editor state,
        # so the final frame cannot have come from the caret fast path.
        child.send("\x0c")
        drain(0.8)
    drain(0.5)

    text = [screen.display[y].rstrip() for y in range(ROWS)]
    attrs = [(y + 1, x + 1, screen.buffer[y][x].fg, screen.buffer[y][x].bg,
              screen.buffer[y][x].reverse)
             for y in range(ROWS) for x in range(COLS)
             if screen.buffer[y][x].fg != "default"
             or screen.buffer[y][x].bg != "default"
             or screen.buffer[y][x].reverse]
    caret = (screen.cursor.y, screen.cursor.x)
    child.close(force=True)
    shutil.rmtree(root, ignore_errors=True)
    return text, attrs, caret


CASES = {
    "move right": [b"\x1b[C"] * 6,
    "move down": [b"\x1b[B"] * 4,
    "onto a bracket": [b"\x1b[B"] * 2 + [b"\x1b[C"] * 7,
    "off the bracket again": [b"\x1b[B"] * 2 + [b"\x1b[C"] * 10,
    "up and down repeatedly": [b"\x1b[B", b"\x1b[A"] * 5,
    "scroll past the bottom": [b"\x1b[B"] * 40,
    "left at column 1": [b"\x1b[D"] * 3,
    "edit then move": [b"X", b"\x1b[C", b"\x1b[B"],
}


def main():
    binary = find_binary()

    for name, keys in CASES.items():
        fast_text, fast_attrs, fast_caret = snapshot(binary, keys, force_full=False)
        full_text, full_attrs, full_caret = snapshot(binary, keys, force_full=True)

        rows = [i + 1 for i, (a, b) in enumerate(zip(fast_text, full_text)) if a != b]
        check(not rows, f"{name}: text matches a full repaint", str(rows))
        check(fast_attrs == full_attrs, f"{name}: every cell attribute matches",
              str(sorted(set(fast_attrs) ^ set(full_attrs))[:4]))
        check(fast_caret == full_caret, f"{name}: the caret lands identically",
              f"{fast_caret} vs {full_caret}")

    # The same equivalence, in a language with block comments. Tokenizing is a
    # sequential state machine, so a partial repaint has to arrive at each line
    # with the comment state a full frame would have had.
    #
    # Deliberately no plain letters here: typing one can raise a word-scan
    # completion, and ctrl-l drops the ghost, so the two runs would differ for
    # a reason that has nothing to do with the fast path.
    c_cases = {
        "C: down inside a block comment": [b"\x1b[B"] * 2,
        "C: down across its close": [b"\x1b[B"] * 4,
        "C: down into the second comment": [b"\x1b[B"] * 10,
        "C: down past both comments": [b"\x1b[B"] * 16,
        "C: up and down repeatedly": [b"\x1b[B", b"\x1b[A"] * 6,
        "C: right along a comment line": [b"\x1b[B"] + [b"\x1b[C"] * 8,
        "C: edit inside a comment": [b"\x1b[B"] * 2 + [b"%"],
    }
    for name, keys in c_cases.items():
        fast_text, fast_attrs, fast_caret = snapshot(binary, keys, False,
                                                     src=SRC_C, name="d.c")
        full_text, full_attrs, full_caret = snapshot(binary, keys, True,
                                                     src=SRC_C, name="d.c")
        rows = [i + 1 for i, (a, b) in enumerate(zip(fast_text, full_text)) if a != b]
        check(not rows, f"{name}: text matches a full repaint", str(rows))
        check(fast_attrs == full_attrs,
              f"{name}: comment colouring matches a full repaint",
              str(sorted(set(fast_attrs) ^ set(full_attrs))[:4]))
        check(fast_caret == full_caret, f"{name}: the caret lands identically",
              f"{fast_caret} vs {full_caret}")

    # Bursts, not single keys. The main loop coalesces up to 64 keystrokes and
    # renders once, so a held arrow key delivers many keys per frame. The
    # guard has to hold for the whole burst: checking only its last key let a
    # batch that scrolled on its earlier repeats and stopped scrolling on its
    # last one (which is what happens on reaching end of file) repaint two
    # lines over a screen still showing the pre-scroll viewport.
    for name, key, count in (("burst down past end of file", b"\x1b[B", 300),
                             ("burst up back to the top", b"\x1b[A", 300)):
        fast_text, fast_attrs, fast_caret = snapshot(binary, [key * count],
                                                     force_full=False)
        full_text, full_attrs, full_caret = snapshot(binary, [key * count],
                                                     force_full=True)
        rows = [i + 1 for i, (a, b) in enumerate(zip(fast_text, full_text)) if a != b]
        check(not rows, f"{name}: no stale rows after the burst",
              f"{len(rows)} rows wrong, first: {rows[:5]}")
        check(fast_caret == full_caret, f"{name}: caret lands identically",
              f"{fast_caret} vs {full_caret}")

    if failures:
        print(f"integration_fastpath: FAILED ({len(failures)}: {', '.join(failures)})")
        sys.exit(1)
    print("integration_fastpath: ALL PASSED")


if __name__ == "__main__":
    main()
