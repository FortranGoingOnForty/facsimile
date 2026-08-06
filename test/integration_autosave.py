#!/usr/bin/env python3
"""
Integration test: crash backups for buffers with unsaved changes.

Until this existed, backup_create had exactly one caller -- the quit prompt's
discard branch -- so an orderly quit was recoverable and a CRASH was not. A
language server deadlocked the editor for two and a half hours and the work
survived only because the failure mode happened to be a quit.

Opt-in, so the first thing checked is that it stays off. Then: that it writes
after a pause, that it writes anyway for someone who never pauses, that it
does NOT write when the text is back to what was already backed up, and --
the point of the whole thing -- that a SIGKILLed editor comes back with the
text on offer.

Usage: python3 test/integration_autosave.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import glob
import json
import os
import shutil
import signal
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
    def __init__(self, binary, enabled=None, idle_ms=600, max_ms=2000,
                 text="int original;\n", home=None, work=None):
        self.home = home or tempfile.mkdtemp(prefix="fac_as_home_")
        cfg = os.path.join(self.home, ".config", "fac")
        os.makedirs(cfg, exist_ok=True)
        with open(os.path.join(cfg, "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        if enabled is not None:
            with open(os.path.join(cfg, "settings.json"), "w") as f:
                # One key per line: the settings parser reads line by line,
                # so a single-line object gives it nothing to match on.
                json.dump({"backup.autosave.enabled": enabled,
                           "backup.autosave.idle_ms": idle_ms,
                           "backup.autosave.max_interval_ms": max_ms},
                          f, indent=2)
        self.work = work or tempfile.mkdtemp(prefix="fac_as_work_")
        self.target = os.path.join(self.work, "dcl.c")
        if not os.path.exists(self.target):
            with open(self.target, "w") as f:
                f.write(text)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.work)
        self.drain(2.2)

    def drain(self, w=0.4):
        end = time.time() + w
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                continue
            except pexpect.EOF:
                break

    def send(self, d, w=0.4):
        self.child.send(d)
        self.drain(w)

    def status(self):
        return self.screen.display[ROWS - 1].rstrip()

    def text(self):
        return "\n".join(r.rstrip() for r in self.screen.display)

    def autosaves(self):
        return sorted(glob.glob(os.path.join(self.work, ".fac", "backups",
                                             "*.autosave")))

    def autosave_body(self):
        f = self.autosaves()
        return open(f[0]).read() if f else None

    def close(self, hard=False):
        if hard:
            self.child.kill(signal.SIGKILL)
        else:
            self.child.close(force=True)

    def cleanup(self):
        shutil.rmtree(self.home, ignore_errors=True)
        shutil.rmtree(self.work, ignore_errors=True)


def test_off_by_default(binary):
    print("\nIt is opt-in: nothing is written unless asked for")
    s = Session(binary, enabled=None)
    try:
        s.send("X", 0.5)
        s.drain(3.0)                       # past both thresholds
        check(s.autosaves() == [], "no autosave file exists",
              str(s.autosaves()))
    finally:
        s.close(); s.cleanup()


def test_writes_after_a_pause(binary):
    print("\nEnabled, it writes once the typing stops")
    s = Session(binary, enabled=True)
    try:
        s.send("MARKER", 0.5)
        check(s.autosaves() == [], "nothing yet, mid-typing", str(s.autosaves()))
        s.drain(2.5)
        body = s.autosave_body()
        check(body is not None, "an autosave appeared")
        check(body is not None and "MARKER" in body,
              "holding the typed text", repr(body))
        check("Backed up" in s.status(), "and the status line said so",
              s.status())
    finally:
        s.close(); s.cleanup()


def test_one_rolling_file(binary):
    print("\nRepeated writes overwrite rather than accumulate")
    s = Session(binary, enabled=True)
    try:
        for word in ("ONE", "TWO", "THREE"):
            s.send(word, 0.4)
            s.drain(2.0)
        files = s.autosaves()
        check(len(files) == 1, "exactly one autosave file", str(files))
        body = s.autosave_body()
        check(body is not None and "THREE" in body,
              "holding the latest text", repr(body))
    finally:
        s.close(); s.cleanup()


def test_no_write_when_nothing_changed(binary):
    print("\nA buffer back to what was already backed up is not rewritten")
    # A long idle window so the type-and-undo below happens INSIDE it: with a
    # short one the intermediate text is itself due for a backup, which would
    # be correct behaviour and would make this test measure nothing.
    s = Session(binary, enabled=True, idle_ms=1500, max_ms=60000)
    try:
        s.send("Q", 0.3)
        s.drain(2.5)
        first = s.autosave_body()
        check(first is not None, "backed up once")
        mtime = os.path.getmtime(s.autosaves()[0])

        # Type and undo back to the same text, quickly enough that the
        # intermediate state is never itself due.
        s.send("Z", 0.15)
        s.send("\x1a", 0.15)
        s.drain(3.0)
        check(s.autosave_body() == first,
              "the backup still holds the same text", repr(s.autosave_body()))
        check(os.path.getmtime(s.autosaves()[0]) == mtime,
              "and the file was not rewritten")
    finally:
        s.close(); s.cleanup()


def test_the_ceiling_covers_continuous_typing(binary):
    print("\nSomeone who never pauses is still backed up")
    s = Session(binary, enabled=True, idle_ms=60000, max_ms=1500)
    try:
        # Never idle for idle_ms, so only the ceiling can fire.
        end = time.time() + 4.0
        while time.time() < end:
            s.send("a", 0.2)
        check(s.autosave_body() is not None,
              "the ceiling produced a backup", str(s.autosaves()))
    finally:
        s.close(); s.cleanup()


def test_a_killed_editor_can_be_recovered(binary):
    print("\nAnd the point of it: a SIGKILLed editor offers the text back")
    s = Session(binary, enabled=True)
    home, work = s.home, s.work
    try:
        s.send("RECOVERME", 0.5)
        s.drain(2.5)
        check(s.autosave_body() is not None, "backed up before the kill")
        s.close(hard=True)
        s.drain(0.5)

        on_disk = open(s.target).read()
        check("RECOVERME" not in on_disk,
              "the file itself was never written", repr(on_disk))

        s2 = Session(binary, enabled=True, home=home, work=work)
        try:
            body = "\n".join(r.rstrip() for r in s2.screen.display)
            check("RECOVERME" in body or "backup" in body.lower()
                  or "recover" in body.lower(),
                  "the restart offers the recovery", body[:300])
        finally:
            s2.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)
        shutil.rmtree(work, ignore_errors=True)


def main():
    binary = find_binary()
    for fn in (test_off_by_default,
               test_writes_after_a_pause,
               test_one_rolling_file,
               test_no_write_when_nothing_changed,
               test_the_ceiling_covers_continuous_typing,
               test_a_killed_editor_can_be_recovered):
        try:
            fn(binary)
        except Exception as exc:              # noqa: BLE001
            check(False, f"{fn.__name__}: {exc!r}")

    print()
    if failures:
        print(f"integration_autosave: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("integration_autosave: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
