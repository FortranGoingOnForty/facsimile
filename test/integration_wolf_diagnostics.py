#!/usr/bin/env python3
"""Regression: a long Wolf diagnostic must remain on the status row.

Wolf diagnostics contain line breaks and multibyte punctuation after a long
ASCII prefix. That shape exposed a byte-budget bug which wrapped the status
text into the document; the caret-only arrow-key renderer then copied lines
over those wrapped rows. This drives the real Wolf LSP when it is installed.

Usage: python3 test/integration_wolf_diagnostics.py [path-to-fac-binary]
Requires: wolf, pexpect, pyte
"""

import codecs
import os
import shutil
import sys
import tempfile
import time

try:
    import pexpect
    import pyte
except ImportError as exc:
    print(f"SKIP: missing dependency ({exc}); pip3 install pexpect pyte")
    sys.exit(0)


ROWS, COLS = 32, 93
SOURCE = '''//! check: run(exit=0)
//! phase: run

fn main() -> !int {
    let t = "howl\\tat\\tthe\\nmoon"
    let s = detab(t)
    print(s)
    0
}

fn detab(s: str) -> str {
    var i = 0
    var t = ""

    while i < s.len {
        if (s[i..i+1] == "\\t""{
            t += "<tab>"
        } else if (s[i..i+1] == "\\n") {
            t += "<nl>"
        } else {
            t += s[i..i+1]
        }
        i += 1
    }
    t
}
'''


def find_binary():
    if len(sys.argv) > 1:
        return os.path.abspath(sys.argv[1])
    return os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "fac"))


def main():
    if not shutil.which("wolf"):
        print("SKIP: wolf is not installed")
        return 0

    home = tempfile.mkdtemp(prefix="fac_wolf_diag_home_")
    work = tempfile.mkdtemp(prefix="fac_wolf_diag_work_")
    child = None
    try:
        os.makedirs(os.path.join(home, ".config", "fac"))
        with open(os.path.join(home, ".config", "fac", "state.json"), "w") as fh:
            fh.write('{"first_run_completed":true,"lsp_installer_seen":true,'
                     '"version":"1.0"}\n')
        target = os.path.join(work, "detab.lu")
        with open(target, "w") as fh:
            fh.write(SOURCE)

        env = {**os.environ, "HOME": home, "TERM": "xterm-256color"}
        env.pop("FAC_SESSION", None)
        env.pop("XDG_CONFIG_HOME", None)
        screen = pyte.Screen(COLS, ROWS)
        stream = pyte.Stream(screen)
        decoder = codecs.getincrementaldecoder("utf-8")("replace")
        child = pexpect.spawn(find_binary(), [target], dimensions=(ROWS, COLS),
                              env=env, cwd=work)

        def drain(seconds):
            end = time.time() + seconds
            while time.time() < end:
                try:
                    data = child.read_nonblocking(65536, 0.1)
                    stream.feed(decoder.decode(data))
                except pexpect.TIMEOUT:
                    pass
                except pexpect.EOF:
                    break

        drain(7.0)
        child.send("\x1b[1;5H" + 15 * "\x1b[B")
        drain(3.0)
        for key in ("\x1b[C", "\x1b[B", "\x1b[A", "\x1b[D") * 3:
            child.send(key)
            drain(0.25)

        rows = [row.rstrip() for row in screen.display]
        diagnostic_rows = [i for i, row in enumerate(rows)
                           if "this string never closes" in row]
        source_rows = [i for i, row in enumerate(rows)
                       if 'if (s[i..i+1] == "\\t""{' in row]
        ok = (diagnostic_rows == [ROWS - 1] and len(source_rows) == 1 and
              rows[-1].endswith("...") and "when ..." not in rows[-1])
        if not ok:
            print("FAIL: Wolf diagnostic escaped the status row")
            for i, row in enumerate(rows, 1):
                if row.strip():
                    print(f"{i:02}: {row}")
            return 1

        print("integration_wolf_diagnostics: ALL PASSED")
        return 0
    finally:
        if child is not None:
            try:
                child.terminate(force=True)
            except Exception:
                pass
        shutil.rmtree(home, ignore_errors=True)
        shutil.rmtree(work, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
