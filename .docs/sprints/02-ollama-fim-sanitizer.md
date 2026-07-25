# Sprint 2 — Ollama client, FIM prompt, and the airtight sanitizer

Still no ghost integration. This sprint ends with a palette command that
*displays* a completion, so the whole pipeline is observable before anything
can touch a buffer.

**The sanitizer's adversarial test table is the gate for Sprint 3.** Nothing
reaches ghost state until it passes.

---

## Targets, in order

### 1. `src/ai/ai_json_module.f90`

Not a general JSON library — a request builder and a field extractor, sized
for one job. `json_module` stays where it is, serving LSP.

Three independent reasons it cannot be reused, each sufficient on its own:

- **It emits invalid JSON for real source files.** `escape_string`
  (`json_module.f90:319`) handles only `" \ \b \f \n \r \t`; every other byte
  passes through, including `0x00`–`0x1F`. Source buffers legitimately contain
  ESC in string literals, and `fac`'s gap buffer uses `char(0)` as a sentinel.
  Raw control bytes inside a JSON string are invalid per RFC 8259 and Go's
  `encoding/json` rejects the whole request with 400 — a user editing one such
  file would get silent, permanent completion failure.
- **It corrupts most C/C++ completions.** Ollama serves through Go's
  `encoding/json`, which HTML-escapes `<`, `>` and `&`. **Verified live**: a
  FIM request returned `"response":"a>b?1:-1"`. `json_module` keeps
  unknown escapes verbatim (`:388-389`), so that reaches the ghost as the
  literal text `a>b?1:-1`. Every completion containing `<`, `>`, `&&`,
  `&ptr` or a template is affected.
- **It leaks every parsed object** (raw pointers, no finalizer) and builds
  strings with `str = str // c` per character. On a path that fires several
  times a second for hours, that is an always-on leak.

Fixing `json_module` is a good idea and the LSP path wants it too — but it is
a *separate* change with its own tests, not something to couple this feature to.

Shape:

- `ai_json_escape_into(src, dst, pos)` — `"` → `\"`, `\` → `\\`,
  `0x08 0x0C 0x0A 0x0D 0x09` → their short forms, any other byte `< 0x20` or
  `0x7F` → `\u00XX`, everything else verbatim (UTF-8 passes straight through).
  Single pass, one allocation sized `6*len + overhead`.
- `ai_json_find_key(doc, key, vstart, vend, ok)` — top-level scan only,
  tracking string state and brace/bracket depth so a key nested inside
  `context` or `details` cannot be mistaken for the real one. Never descends,
  so skipping ollama's multi-thousand-element `context` array costs nothing.
- `ai_json_decode_string(raw, out, ok)` — full `\uXXXX` including **UTF-16
  surrogate pair recombination**, emitting UTF-8. One allocation.
- **An unknown escape rejects the whole response.** `json_module` keeps them
  as literal text; we must not. An unrecognised escape means the scanner has
  desynchronised from the framing, and continuing risks pushing a literal
  backslash sequence into the buffer.

### 2. `src/ai/ollama_client_module.f90`

- `GET /api/tags` — which models are installed.
- `POST /api/show` — probe `capabilities` for `insert`.
- `POST /api/generate` with `prompt` **and always `suffix`**.

**The request shape is a safety control.** Verified on one model with one
prompt: with `suffix` → `return a + b;`. With `raw:true` and no suffix →
overruns the function then rambles. With neither → `"Certainly! It looks like
you're starting to define a simple"` — English prose that would land in a
source file. Without `suffix`, ollama applies the model's chat template
(confirmed: the returned `context` begins `<|im_start|>system`).

So: always send `suffix` even when empty; refuse any model that does not
report `insert`; prefer `-base` models locally.

`keep_alive` on every request to hide the cold load (58 s measured on a 32b).

### 3. `src/ai/completion_context_module.f90`

Prefix and suffix windows either side of the cursor, snapped to line
boundaries. Budget generously — `prompt_eval` is 7–32 ms on GPU, so context
length is no longer the latency driver; `num_predict` is.

### 4. `src/ai/completion_sanitize_module.f90` — the airtight layer

**Reject over repair.** A rejected completion costs the user nothing — they
see no ghost, exactly as today. A repaired-but-wrong completion costs them a
silent defect. Where reconciliation is unavoidable it is *subtractive only*:
we delete from the model's text, never add to it.

Ordered pipeline; ordering is load-bearing:

1. **Control bytes** — reject any byte `< 0x20` except LF and TAB, and `0x7F`.
   Ghost text is written straight to the terminal, so an `ESC [` in model
   output is executed: cursor moves, screen clears, **OSC 52 writes the system
   clipboard**. One predicate kills ESC, CSI, OSC, CR, BEL, SO/SI, NUL.
2. **UTF-8 well-formedness** — no overlongs, no encoded surrogates, ≤ U+10FFFF.
   Invalid UTF-8 desynchronises every `utf8_*` walk and the corruption reaches
   the saved file.
3. **Special tokens** — reject on `<think>`, `<|`, `|>`, `fim_`, `endoftext`.
   Their presence means the template is wrong and everything after is suspect.
4. **Chat-mode shapes** — reject a leading markdown fence or a prose lead-in
   (`Here`, `Sure`, `Certainly`, `It looks like`). Do not strip the fence: a
   fenced response is a *restatement*, not a continuation, and stripping it
   yields plausible-looking but wrong insertions.
5. **Budget** — reject beyond a hard byte cap rather than truncating a
   statement in half.
6. **Repetition** — reject ≥3 identical consecutive lines, or a short cycle
   repeated to fill the budget. Base FIM models loop pathologically at low
   temperature.
7. **Context reconciliation, subtractive** — prefix echo (model re-emits what
   was typed), suffix echo (`foo(` + `bar)` completing `bar)` → `foo(bar)bar)`),
   full duplicate of the text after the cursor.
8. **Single-line mode** — cut at the first LF.
9. **Trailing partial token** when generation hit the token cap.

### 5. `AI: Status` command and the `ai_tick` pump site

Deferred from Sprint 1 because there was nothing to report or pump. Both land
here.

---

## Verification

- **The sanitizer gets the heaviest unit suite in the project**: a table of
  adversarial inputs — ANSI escapes, `ESC[2J`, OSC 52, NUL, invalid UTF-8,
  overlong encodings, lone surrogates, fenced blocks, `<think>`, 100 KB of
  output, repetition loops, suffix duplication, pure whitespace — each
  asserted reject-or-normalise. This suite is the definition of "airtight".
- **JSON**: round-trip every control byte; decode `>` to `>`; decode a
  surrogate pair to a 4-byte emoji; reject an unknown escape; confirm a key
  nested inside `context` is not mistaken for a top-level one.
- **Live**: against the local ollama, assert FIM output for C, Python and
  Fortran fixtures, and record real latency per model.
