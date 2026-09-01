#!/usr/bin/env python3
"""
Integration test: opening more files than the tab cap must not corrupt them.

`create_tab` silently returns when the tab array is already at `max_tabs`.
Callers did not check, so `open_file_in_editor` then loaded the new file's text
into whatever tab was already active -- a tab still carrying the *previous*
file's name. Ctrl-S afterwards wrote the new file's content over the old file's
path.

That is reachable with no exotic setup: open ten files, open an eleventh, save.
This suite opens twelve and asserts, for every tab, that the name in the tab bar
matches the text on screen -- and then that nothing on disk changed.

Usage: python3 test/integration_max_tabs.py [path-to-fac-binary]
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

ROWS, COLS = 30, 120
N_FILES = 12
failures = []


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if not ok:
        failures.append(name)
        if detail:
            for ln in str(detail).split("\n")[:14]:
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


class Session:
    """A workspace of N_FILES files, each whose content names itself."""

    def __init__(self, binary):
        self.home = tempfile.mkdtemp(prefix="fac_maxtabs_")
        cfg = os.path.join(self.home, ".config", "fac")
        os.makedirs(cfg)
        with open(os.path.join(cfg, "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')

        self.ws = os.path.join(self.home, "ws")
        os.makedirs(self.ws)
        self.names = [f"file{i:02d}.txt" for i in range(1, N_FILES + 1)]
        for n in self.names:
            # Content is a unique token derived from the name, so a buffer that
            # ends up under the wrong name is unmistakable.
            with open(os.path.join(self.ws, n), "w") as f:
                f.write(f"CONTENT_OF_{n}\n")
        self.original = {n: open(os.path.join(self.ws, n), "rb").read()
                         for n in self.names}

        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        first = os.path.join(self.ws, self.names[0])
        self.child = pexpect.spawn(binary, [first], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.ws)
        self.drain(1.5)

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

    def send(self, data, w=0.5):
        self.child.send(data)
        self.drain(w)

    def display(self):
        return "\n".join(r.rstrip() for r in self.screen.display)

    def tree_selection(self):
        """The reverse-video row in the file tree, i.e. what Enter would act on."""
        for y in range(1, ROWS - 1):
            row = self.screen.buffer[y]
            cells = "".join(row[x].data if row[x].reverse else ""
                            for x in range(30)).strip()
            if len(cells) > 1 and cells not in ("\u2717", "\u2191"):
                return cells
        return None

    def in_tree(self):
        return self.tree_selection() is not None

    def tab_bar(self):
        return self.screen.display[0].rstrip()

    def status(self):
        return self.screen.display[ROWS - 1].rstrip()

    def active_name(self):
        """The active file according to the status bar.

        Deliberately not the tab bar: that truncates on overflow (it exits on
        the first label that does not fit), so with a dozen tabs the active one
        may not be drawn at all. That is a real bug, but a different one -- it
        loses nothing and corrupts nothing.
        """
        for n in self.names:
            if n in self.status():
                return n
        return None

    def body(self):
        """Everything below the tab bar and above the status bar."""
        return "\n".join(r.rstrip() for r in self.screen.display[1:ROWS - 1])

    def on_disk(self, name):
        return open(os.path.join(self.ws, name), "rb").read()

    def close(self):
        try:
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)


def open_the_rest(s):
    """Open files 2..N through the file tree.

    Arrow-navigate to each row rather than fuzzy-typing: the fuzzy filter
    matches several of these names at once. The tree legitimately stays open
    after Enter, so that is not treated as a failure to open.

    Returns the number of distinct files that became tabs.
    """
    for n in s.names[1:]:
        if not s.in_tree():
            s.send("\x02", 0.9)          # ctrl-b: file tree
        if not s.in_tree():
            break
        found = False
        for _ in range(2 * N_FILES + 8):
            sel = s.tree_selection()
            if sel and n in sel:
                found = True
                break
            s.send("\x1b[B", 0.12)
        if not found:
            break
        s.send("\r", 0.8)               # Enter: open the file

    if s.in_tree():
        s.send("\x02", 0.8)             # leave the tree so the body is visible
    return len([n for n in s.names if n in s.tab_bar()])


def test_every_tab_shows_its_own_file(binary):
    s = Session(binary)
    try:
        open_the_rest(s)

        # The invariant: the file the editor says is active, and the text it is
        # actually showing, must be the same file. When create_tab silently
        # refused, they were not.
        body = s.body()
        content_of = [n for n in s.names if f"CONTENT_OF_{n}" in body]
        check(len(content_of) == 1,
              "exactly one file's content is displayed", body[:400])
        if len(content_of) != 1:
            return

        displayed = content_of[0]
        named = s.active_name()
        check(named is not None, "the status bar names the active file", s.status())
        check(named == displayed,
              f"the editor shows {displayed} and says it is editing {named}",
              f"status: {s.status()}\nbody: {body[:200]}")
    finally:
        s.close()


def test_saving_does_not_overwrite_another_file(binary):
    """The headline case.

    Saving immediately after the failed open is not enough: `editor%filename`
    has already been pointed at the new file, so Ctrl-S writes its text to its
    own path and nothing looks wrong. The damage is that the *tab* absorbed the
    text while keeping the old name -- so it surfaces on the next switch back to
    that tab. Walk every tab and save each one, which is what cycling with
    Ctrl-S actually does.
    """
    s = Session(binary)
    try:
        open_the_rest(s)
        for _ in range(N_FILES + 2):
            s.send("\x13", 0.45)            # ctrl-s
            s.send("\x1b[6;5~", 0.35)       # ctrl-pagedown: next tab

        damaged = []
        for n in s.names:
            if s.on_disk(n) != s.original[n]:
                damaged.append(
                    f"{n}: {s.original[n]!r} -> {s.on_disk(n)!r}")
        check(not damaged,
              "cycling every tab and saving corrupts no file",
              "\n".join(damaged))
    finally:
        s.close()


def test_the_active_tab_is_always_drawn(binary):
    """The bar used to stop at the first label that did not fit, so with a
    dozen tabs the active one could be entirely off-screen -- and therefore
    unclickable, since a click resolves against what was drawn."""
    s = Session(binary)
    try:
        open_the_rest(s)
        missing = []
        for step in range(N_FILES + 2):
            name = s.active_name()
            if name and name not in s.tab_bar():
                missing.append((step, name, s.tab_bar()[:80]))
            s.send("\x1b[6;5~", 0.3)          # ctrl-pagedown
        check(not missing,
              "the active tab is drawn in the bar at every step",
              "\n".join(f"step {a}: {b} absent from {c!r}" for a, b, c in missing[:4]))
    finally:
        s.close()


def test_overflow_is_announced(binary):
    """Tabs past the edge are indicated rather than silently dropped."""
    s = Session(binary)
    try:
        open_the_rest(s)
        # Walk back to the first tab; tabs to the right must then be flagged.
        for _ in range(N_FILES + 2):
            s.send("\x1b[5;5~", 0.25)         # ctrl-pageup
        bar = s.tab_bar()
        check(">" in bar or "<" in bar,
              "an overflow marker is shown when tabs do not all fit", repr(bar))
    finally:
        s.close()


def test_all_files_are_reachable(binary):
    """Every file opened should be reachable as its own tab."""
    s = Session(binary)
    try:
        open_the_rest(s)
        seen = set()
        for _ in range(N_FILES + 4):
            body = s.body()
            for n in s.names:
                if f"CONTENT_OF_{n}" in body:
                    seen.add(n)
            s.send("\x1b[6;5~", 0.35)    # ctrl-pagedown: next tab
        check(len(seen) == N_FILES,
              f"all {N_FILES} files are reachable as tabs (saw {len(seen)})",
              "missing: " + ", ".join(sorted(set(s.names) - seen)))
    finally:
        s.close()


def main():
    binary = find_binary()
    for fn in (test_every_tab_shows_its_own_file,
               test_the_active_tab_is_always_drawn,
               test_overflow_is_announced,
               test_saving_does_not_overwrite_another_file,
               test_all_files_are_reachable):
        try:
            fn(binary)
        except Exception as exc:            # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    if failures:
        print(f"\nintegration_max_tabs: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("\nintegration_max_tabs: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
