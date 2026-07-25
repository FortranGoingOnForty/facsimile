# AI inline completion

`fac` can complete code inline using a local language model, shown as dim
shadow text ahead of the caret. It reads the comment above the caret and the
code on **both sides** of it, so it completes intent rather than just matching
identifiers that already exist.

It is **off by default** and nothing runs, connects, or leaves your machine
until you turn it on.

---

## Quick start

```bash
# 1. A model that can fill in the middle. -base variants have no chat
#    template to fall back into, which matters (see "Why -base" below).
ollama pull qwen2.5-coder:1.5b-base

# 2. On Arch, the default ollama package is CPU-only. Without this you get
#    ~1.1 s per completion instead of ~0.3 s.
sudo pacman -S ollama-cuda
sudo systemctl enable --now ollama
```

Then in `fac`: press **`Alt+I`** (or `Ctrl+P` → **AI: Toggle Inline Completion**).

`Alt+I` is also the quick way to silence it: turning off is instant and
clears any suggestion on screen. The setting persists, so it stays off until
you turn it back on.

The status bar reports what happened — `ready: <model> on <host>`, or exactly
why not. The message stays up until you move the caret, so there is time to
read it.

Once enabled, a small **`[AI]`** marker sits in the status bar for as long as
it is on — `[AI:down]` or `[AI:no-fim]` if the backend is not usable. Without
it, "off" and "misconfigured" look identical, since neither produces
suggestions.

> The ollama **service** runs as user `ollama` with
> `OLLAMA_MODELS=/var/lib/ollama`, not your `~/.ollama/models`. Pull models
> while the service is running so they land where it will find them.

---

## Using it

| Key | Action |
|---|---|
| `Tab` | Accept the whole suggestion |
| `Right` | Accept, when the caret is at end of line |
| `Ctrl+Right` | Accept one word, keeping the rest offered |
| `Alt+Right` | Accept one line of a multi-line suggestion |
| `Alt+\` | Deep completion here — a bigger model, a longer budget |
| `Alt+I` | Turn completion on or off (`Alt+Shift+I` works too) |
| any other key | Dismiss |

Nothing is ever inserted without one of those explicit accepts.

`Ctrl+Right` and `Alt+Right` shadow word-move-right while a suggestion is
showing, as Copilot does. `Alt+F` still moves by word in both cases.

### When suggestions appear

After a word character, `(`, `,`, `{`, `:`, `.`, `=`, space, backspace — and
**after Enter**, which is the most valuable moment of all: finish a comment
describing what you want, press Enter, and the completion arrives on the
fresh line.

### Typing into a suggestion is free

If you type the character the suggestion already predicted, it advances in
place and **no request is sent**. During normal forward typing into a correct
suggestion this eliminates most requests, which is what makes a ~300 ms
backend feel immediate.

### Asking the same question twice is free

Backspace is a trigger, so deleting a few characters and retyping them asks
the model something it answered a moment ago. Completions are cached on the
exact prompt — model, the code before the caret, the code after it, and the
token budget — and a hit returns the already-validated text in **0 ms**
without touching the backend. Four identical delete-and-retype cycles cost
one request, not four.

Rejections are cached too, for 30 s, so a prompt the model keeps answering
badly does not burn the rate limiter on every keystroke.

The cache holds 64 entries, is cleared when you switch files or change
models, and reports itself in **AI: Status**:

```
… | sent 1, shown 3, rejected 0 | cache 75% (3 saved)
```

Because entries are keyed on the surrounding code rather than on the caret's
coordinates, an undo needs no special handling — different code is a
different key, and a stale answer can never be served for it.

---

## Settings

`~/.config/fac/settings.json`. Every key is optional; the defaults below apply
when absent.

```json
{
  "ai.enabled": false,
  "ai.host": "127.0.0.1",
  "ai.port": 11434,
  "ai.model": "qwen2.5-coder:1.5b-base",
  "ai.debounce_ms": 150,
  "ai.num_predict": 24,
  "ai.temperature_x100": 15,
  "ai.prefix_bytes": 6000,
  "ai.suffix_bytes": 2000,
  "ai.max_block_lines": 4,
  "ai.context.file_header": true,
  "ai.context.symbols": true,

  "ai.remote.enabled": false,
  "ai.remote.host": "",
  "ai.remote.port": 11434,
  "ai.remote.model": "qwen2.5-coder:32b",
  "ai.remote.num_predict": 256,
  "ai.remote.gate_url": ""
}
```

`ai.temperature_x100` is temperature × 100 (`15` = 0.15). Not zero: pure greedy
decoding makes base models repeat themselves.

---

## The remote tier is a separate opt-in

`ai.enabled` gets you **loopback only**. Reaching another machine requires
`ai.remote.enabled` as well — a second, deliberate decision, because source
code leaving your machine deserves its own switch.

While a non-loopback remote is enabled, the status bar carries a permanent
`[AI->host]` badge. You should never have to remember what you configured.

The remote model is used **only** for `Alt+\`. Automatic keystroke completion
always stays local: a ~640 ms suggestion is worse than none, and firing one per
keystroke at a shared machine is rude.

**Not evicting someone else's model.** Before a deep request, `fac` checks
`/api/ps` on the remote host and skips if a *different* model is resident,
falling back to the local one.

`ai.remote.gate_url` is an optional availability gate: a path on the remote
host that must return a non-empty body for the remote tier to fire. If you keep
an availability marker such as `~/pool-up`, expose it over HTTP and point this
at it — `fac` deliberately has no SSH client.

---

## Why `-base` models, and why FIM matters

Inline completion needs **fill-in-the-middle**: the model must see the code
*after* the caret, or it is guessing at what it is completing into.

At enable time `fac` asks ollama whether the model reports the `insert`
capability, and refuses it if not:

```
model 'qwen3-coder:30b' has no FIM support; inline completion disabled
```

This matters more than it sounds. The same model, same prompt, three request
shapes:

| Request | Result |
|---|---|
| with `suffix` (FIM) | `return a + b;` |
| `raw`, no suffix | overruns the function, then starts rambling |
| neither | `"Certainly! It looks like you're starting to define a simple"` |

Without a suffix, ollama applies the model's **chat template** and you get
English prose where code should be. `fac` always sends `suffix`, even empty.

`-base` variants have no chat template at all, which is why they are preferred
locally.

---

## What it will and will not do

Suggestions are checked before you ever see them. Rejected outright:

- anything containing terminal control bytes — shadow text is written straight
  to your terminal, so an escape sequence in model output would be *executed*
  (screen clears, cursor jumps, or `OSC 52` writing your system clipboard)
- invalid UTF-8, which would corrupt column arithmetic all the way into the
  saved file
- leaked model tokens (`<think>`, `<|endoftext|>`, `fim_*`)
- markdown fences and conversational openers — a fenced reply is a
  *restatement*, not a continuation
- repetition loops, which base models fall into at low temperature
- text that merely repeats what is already after the caret

The rule throughout is **reject rather than repair**. A rejected suggestion
costs you nothing; a repaired-but-wrong one costs you a silent defect.

---

## Performance

Measured on an RTX 3080 Laptop, warm, 48-token cap:

| Model | prompt_eval | generation | total |
|---|---|---|---|
| `1.5b-base` | 7–19 ms | 112 ms | **~325 ms** |
| `3b-base` | 8–24 ms | 76–393 ms | 292–599 ms |
| `7b-base` | 14–32 ms | 257–466 ms | 471–697 ms |

Context is cheap; **generation is not**. There is a ~190 ms fixed overhead per
request, so latency tracks `ai.num_predict` far more than window size — raise
the windows freely, raise the token cap carefully.

The first request after enabling loads the model, which can take a minute for a
large one. `keep_alive` holds it resident afterwards.

---

## Troubleshooting

Run **AI: Status** from the command palette. It reports the backend, model,
health, last latency, how many suggestions were shown versus rejected, and the
most recent rejection reason.

| Symptom | Cause |
|---|---|
| `model '…' is not pulled on …` | Pull it *with the service running* — see the note above |
| `has no FIM support` | Not a fill-in-the-middle model; use a `-base` coder model |
| `no ollama at …` | Service not running, or wrong host/port |
| Suggestions are slow | Check `nvidia-smi` during a request; on Arch, plain `ollama` is CPU-only |
| Nothing appears at all | `ai.enabled` is false, or the caret is not after a word character |
