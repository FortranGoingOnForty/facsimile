#!/usr/bin/env python3
"""
Integration test: model-backed shadow text in the real editor.

Two things are being proved. First, that with the feature off -- the default --
the editor behaves exactly as it did before it existed. Second, that with it on
and a live model, a suggestion appears, Tab inserts it, and dismissing it leaves
the buffer byte-identical.

Skips the model half cleanly when ollama is not running, so it is safe anywhere.

Usage: python3 test/integration_ai_ghost.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import json
import os
import shutil
import sys
import tempfile
import time
import urllib.request

try:
    import pexpect
    import pyte
except ImportError as e:
    print(f"SKIP: missing dependency ({e}); pip3 install pexpect pyte")
    sys.exit(0)

ROWS, COLS = 24, 100
MODEL = "qwen2.5-coder:1.5b-base"
failures = []


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if not ok:
        failures.append(name)
        if detail:
            for ln in str(detail).split("\n")[:16]:
                print("        " + ln[:96])


def have_model():
    try:
        with urllib.request.urlopen("http://127.0.0.1:11434/api/tags", timeout=3) as r:
            names = [m["name"] for m in json.load(r).get("models", [])]
        return MODEL in names
    except Exception:
        return False


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
    def __init__(self, binary, content, ai_on, name="s.c"):
        self.home = tempfile.mkdtemp(prefix="fac_aig_")
        cfg = os.path.join(self.home, ".config", "fac")
        os.makedirs(cfg)
        with open(os.path.join(cfg, "state.json"), "w") as f:
            f.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                    ' "version": "1.0"}\n')
        if ai_on:
            with open(os.path.join(cfg, "settings.json"), "w") as f:
                f.write('{\n'
                        '  "ai.enabled": true,\n'
                        '  "ai.host": "127.0.0.1",\n'
                        '  "ai.port": 11434,\n'
                        '  "ai.model": "%s",\n'
                        '  "ai.debounce_ms": 120\n'
                        '}\n' % MODEL)
        self.target = os.path.join(self.home, name)
        with open(self.target, "w") as f:
            f.write(content)
        env = {**os.environ, "TERM": "xterm-256color", "HOME": self.home}
        env.pop("XDG_CONFIG_HOME", None)
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)
        self.child = pexpect.spawn(binary, [self.target], dimensions=(ROWS, COLS),
                                   env=env, cwd=self.home)
        self.drain(1.5)

    def drain(self, w=0.4):
        end = time.time() + w
        while time.time() < end:
            try:
                self.stream.feed(self.child.read_nonblocking(65536, 0.1)
                                 .decode("utf-8", "replace"))
            except pexpect.TIMEOUT:
                pass
            except pexpect.EOF:
                break

    def send(self, data, w=0.4):
        self.child.send(data)
        self.drain(w)

    def wait_for(self, pred, timeout=8.0):
        end = time.time() + timeout
        while time.time() < end:
            self.drain(0.25)
            if pred(self.screen):
                return True
        return False

    def display(self):
        return "\n".join(r.rstrip() for r in self.screen.display)

    def saved(self):
        self.send("\x13", 0.6)
        with open(self.target) as f:
            return f.read()

    def close(self):
        try:
            self.child.terminate(force=True)
        except Exception:
            pass
        shutil.rmtree(self.home, ignore_errors=True)


def test_off_by_default(binary):
    """With no settings file at all, the editor must behave as before."""
    s = Session(binary, "int main(void) {\n    \n}\n", ai_on=False)
    try:
        s.send("\x1b[B", 0.3)
        s.send("\x1b[F", 0.3)
        s.send("int x = 1;", 1.2)
        check(s.saved() == "int main(void) {\n    int x = 1;\n}\n",
              "typing is unaffected when AI is off", s.saved())
    finally:
        s.close()


def test_ghost_appears_and_accepts(binary):
    s = Session(binary, "/* Return the larger of a and b. */\n"
                        "int max_of(int a, int b) {\n"
                        "    ret\n"
                        "}\n", ai_on=True)
    try:
        s.send("\x1b[B\x1b[B", 0.3)      # to line 3
        s.send("\x1b[F", 0.3)            # end of "    ret"
        # retype the trigger character so a completion is requested
        s.send("\x7f", 0.3)
        s.send("t", 0.4)

        got = s.wait_for(lambda sc: any("return" in r for r in sc.display[1:6]),
                         timeout=12)
        check(got, "a model suggestion appears as ghost text", s.display())

        if got:
            before = open(s.target).read()
            check("return" not in before,
                  "the ghost is NOT in the buffer before accepting", repr(before))

            s.send("\t", 1.2)
            after = s.saved()
            check("return" in after, "Tab inserts the suggestion", repr(after))
    finally:
        s.close()


def test_dismiss_leaves_buffer_untouched(binary):
    original = ("/* Add a and b. */\n"
                "int add(int a, int b) {\n"
                "    ret\n"
                "}\n")
    s = Session(binary, original, ai_on=True)
    try:
        s.send("\x1b[B\x1b[B", 0.3)
        s.send("\x1b[F", 0.3)
        s.send("\x7f", 0.3)
        s.send("t", 0.4)
        s.wait_for(lambda sc: any("return" in r for r in sc.display[1:6]), timeout=12)

        s.send("\x1b[B", 0.6)            # move away: dismisses the ghost
        check(s.saved() == original,
              "dismissing leaves the buffer byte-identical", repr(s.saved()))
    finally:
        s.close()


def main():
    binary = find_binary()

    test_off_by_default(binary)

    if not have_model():
        print(f"SKIP: {MODEL} not available on 127.0.0.1:11434 "
              "(off-by-default checks still ran)")
    else:
        test_ghost_appears_and_accepts(binary)
        test_dismiss_leaves_buffer_untouched(binary)

    if failures:
        print(f"\nintegration_ai_ghost: FAILED ({len(failures)}): "
              + ", ".join(failures))
        return 1
    print("\nintegration_ai_ghost: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
