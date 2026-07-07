---
name: verify
description: Verify fac (terminal text editor) changes by driving the TUI in a pty with pexpect + pyte. Use after changing editor behavior, rendering, input handling, or LSP features.
---

# Verifying fac end-to-end

fac is a TUI; verification means driving it in a pty and inspecting the
rendered screen. No tmux on this machine — use Python `pexpect` (pty driver)
plus `pyte` (virtual terminal that interprets the ANSI stream).

## Setup (once per session)

```bash
cd <scratchpad>
python3 -m venv venv && ./venv/bin/pip install pexpect pyte
```

## Driver pattern

```python
import pexpect, pyte, time
screen = pyte.Screen(80, 24); stream = pyte.Stream(screen)
child = pexpect.spawn("/abs/path/to/fac", ["file.txt"], dimensions=(24, 80),
                      env={**os.environ, "TERM": "xterm-256color"})
def drain(wait=0.4):   # read fac's output into the virtual terminal
    end = time.time() + wait
    while time.time() < end:
        try: stream.feed(child.read_nonblocking(65536, 0.1).decode("utf-8","replace"))
        except pexpect.TIMEOUT: pass
        except pexpect.EOF: break
child.send("x"); drain()          # type; then inspect screen.display
```

## Screen geometry gotchas

- Row 0 of `screen.display` is the **tab bar**; buffer line N is display
  index N (1-based line lands on 0-based index N). Status bar = last row.
- Line-number gutter is 6 columns (`LINE_NUMBER_WIDTH=5` + space):
  buffer column c (1-based) is at screen col `5 + c` (0-based).
- Colors: `screen.buffer[row][col].fg` — e.g. ghost text is `brightblack`
  (ESC[90m), normal text `default`. pyte does not track dim (SGR 2).

## Key bytes

ctrl-e `\x05`, ctrl-s `\x13`, ctrl-q `\x11`, ctrl-z `\x1a`, tab `\t`,
enter `\r`, esc `\x1b`, backspace `\x7f`, down `\x1b[B`, right `\x1b[C`,
shift-home `\x1b[1;2H`, alt-r `\x1br`, ctrl-space `\x00` (NUL).

## Proving edits are real

Send ctrl-s then read the file from disk — screen checks can lie about
buffer state; the saved file can't.

## LSP scenarios

- clangd is installed; open a `.c` file and fac auto-starts it
  (verify with `pgrep -a clangd`). Allow ~3s init before typing;
  allow ~2s drain after a request before asserting.
- Use a `.txt` file to guarantee NO LSP server (isolates non-LSP paths).
- To prove a completion came from LSP, use a symbol that appears nowhere
  in the file text (e.g. `printf` with only the `#include` present).
- Document sync to servers is debounced 500ms (`document_sync_module`);
  features that need current text must force-flush
  (`flush_pending_changes(..., .true.)`) before requesting.
- References (alt-r) is a known-good callback-wired LSP feature — use it
  as a control when LSP plumbing seems broken.

## Unit tests

Standalone programs in `test/`; compile against the .o/.mod files that
`make` leaves in the repo root:

```bash
gfortran -O0 -g -fcheck=all -I. test/test_X.f90 src/.../dep1.o src/.../dep2.o -o /tmp/t && /tmp/t
```
