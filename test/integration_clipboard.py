#!/usr/bin/env python3
"""Integration test: copying must not leak helper output into the editor PTY.

The historical copy command piped `cat` into each possible clipboard utility.
On a machine without the early utilities, cat could report a broken pipe on
FACSIMILE's terminal before a later utility succeeded. A deliberately noisy
fake cat makes that race deterministic while a fake pbcopy records the text.

Usage: python3 test/integration_clipboard.py [path-to-fac-binary]
Requires: pip3 install pexpect pyte
"""

import os
import shutil
import stat
import sys
import tempfile
import time

try:
    import pexpect
    import pyte  # noqa: F401 - CI requires both PTY test dependencies
except ImportError as exc:
    print(f"SKIP: missing dependency ({exc}); pip3 install pexpect pyte")
    sys.exit(0)


failures = []


def check(ok, name, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}" +
          (f"  [{detail!r}]" if detail and not ok else ""))
    if not ok:
        failures.append(name)


def find_binary():
    if len(sys.argv) > 1:
        return os.path.abspath(sys.argv[1])
    here = os.path.dirname(os.path.abspath(__file__))
    candidate = os.path.join(os.path.dirname(here), "fac")
    if os.path.exists(candidate):
        return candidate
    print("SKIP: no fac binary (build with make, or pass a path)")
    sys.exit(0)


def write_executable(path, body):
    with open(path, "w") as handle:
        handle.write(body)
    os.chmod(path, stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR)


def drain(child, wait=0.4):
    data = b""
    end = time.time() + wait
    while time.time() < end:
        try:
            data += child.read_nonblocking(65536, 0.08)
        except pexpect.TIMEOUT:
            continue
        except pexpect.EOF:
            break
    return data


def test_copy_is_silent_and_reaches_the_clipboard(binary, quoted_temp=False):
    # Keep the ordinary case short enough that the released implementation's
    # fixed 512-byte command buffer reaches every fallback. A separate quoted
    # path below exercises the fixed implementation's safe path handling.
    root = tempfile.mkdtemp(prefix="facclip_", dir="/tmp")
    child = None
    try:
        home = os.path.join(root, "home")
        fake_bin = os.path.join(root, "bin")
        temp_dir = root
        if quoted_temp:
            temp_dir = os.path.join(root, "tmp'quoted")
        capture = os.path.join(root, "copied.txt")
        os.makedirs(os.path.join(home, ".config", "fac"))
        os.makedirs(fake_bin)
        if quoted_temp:
            os.makedirs(temp_dir)
        with open(os.path.join(home, ".config", "fac", "state.json"), "w") as handle:
            handle.write('{"first_run_completed": true, "lsp_installer_seen": true,'
                         ' "version": "1.0"}\n')

        target = os.path.join(root, "copy.txt")
        with open(target, "w") as handle:
            handle.write("clipboard line\nsecond line\n")

        # The old pipeline invoked this once per fallback and exposed its
        # stderr. The fixed path never invokes cat at all.
        write_executable(
            os.path.join(fake_bin, "cat"),
            "#!/bin/sh\n"
            "echo 'cat: stdout: Broken pipe' >&2\n"
            "exec /bin/cat \"$@\"\n",
        )
        write_executable(
            os.path.join(fake_bin, "pbcopy"),
            "#!/bin/sh\n"
            "exec /bin/cat > \"$FAC_TEST_CLIPBOARD_CAPTURE\"\n",
        )

        env = {**os.environ, "TERM": "xterm-256color", "HOME": home,
               "PATH": fake_bin + os.pathsep + os.environ.get("PATH", ""),
               "TMPDIR": temp_dir,
               "FAC_TEST_CLIPBOARD_CAPTURE": capture}
        env.pop("XDG_CONFIG_HOME", None)
        env.pop("FAC_SESSION", None)
        child = pexpect.spawn(binary, [target], dimensions=(24, 100),
                              env=env, cwd=root, timeout=15)
        startup = drain(child, 1.0)
        ready_by = time.time() + 4.0
        while b"clipboard line" not in startup and time.time() < ready_by:
            startup += drain(child, 0.2)
        check(b"clipboard line" in startup,
              "clipboard fixture is rendered before ctrl-c" +
              (" with a quoted temp path" if quoted_temp else ""))

        child.send("\x03")  # ctrl-c copies the current line
        copy_output = drain(child, 1.0)

        copied = None
        if os.path.exists(capture):
            with open(capture) as handle:
                copied = handle.read()
        context = " with a quoted temp path" if quoted_temp else ""
        check(copied == "clipboard line",
              "ctrl-c reaches the available clipboard utility" + context,
              copied)
        check(b"Broken pipe" not in copy_output and b"cat:" not in copy_output,
              "clipboard fallback writes nothing onto the editor PTY" + context,
              copy_output.decode("utf-8", "replace"))
        check(not os.path.exists(os.path.join(temp_dir, "facsimile_clipboard.tmp")),
              "clipboard temp file is removed" + context)
    finally:
        if child is not None:
            try:
                child.send("\x11")
                drain(child, 0.5)
                child.terminate(force=True)
            except Exception:
                pass
        shutil.rmtree(root, ignore_errors=True)


def main():
    test_copy_is_silent_and_reaches_the_clipboard(find_binary())
    test_copy_is_silent_and_reaches_the_clipboard(find_binary(), quoted_temp=True)
    if failures:
        print(f"\nintegration_clipboard: FAILED ({len(failures)}): " +
              ", ".join(failures))
        return 1
    print("\nintegration_clipboard: ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
