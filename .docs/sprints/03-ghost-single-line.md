# Sprint 3 — Ghost integration, single line

The first sprint with a user-visible feature. Everything from Sprints 1–2 gets
wired to the ghost text that already exists.

---

## Targets, in order

### 1. `GHOST_SRC_LLM` and separate bookkeeping

`ghost_text_t` gains LLM request state kept **distinct** from the LSP
`pending_request_id`, so the two sources can never alias each other's replies.

### 2. Invert the trigger

Today `update_ghost_suggestion` *sends* from the keystroke path, and force-flushes
document sync on the way (`notify_buffer_change` does a full `buffer_to_string`
copy). Change it to *record* intent — anchor, prefix, revision, timestamp — and
let `ai_tick` decide whether to send.

This moves prompt building and the document copy off the keystroke path
entirely, so it is a net latency *improvement* over today, not a new cost.

Fire only when all hold:
- debounce elapsed
- no request in flight
- token bucket allows it
- the anchor still matches the live cursor
- **`terminal_input_available()` is false** — never spend a request on a
  keystroke the user has already superseded

### 3. Prefix extension — the reason this feels fast

Measured single-line latency is ~250–330 ms on the 1.5b. A 150 ms debounce puts
first ghost at ~450 ms. The fix is not to shave model time but to **stop
re-querying**.

When the user types the character the ghost already predicted, advance the
suggestion in place: `prefix` grows by one, `anchor_col` moves right,
`suggestion` is untouched, and no request is sent. During normal forward typing
into a correct suggestion this eliminates most requests outright.

This falls out cleanly because `ghost_suffix()` is defined as
`suggestion(len(prefix)+1:)` — growing the prefix shrinks the suffix for free.

### 4. Arbitration

Three sources now: word-scan (0 ms), LSP (~50 ms), LLM (~300 ms). Naive
last-writer-wins flickers, and worse, **changes what Tab does between the user
deciding to press it and pressing it**.

- Ranked: `WORDS < LSP < LLM`. A late arrival may only replace the current
  ghost if its rank is higher *and* the anchor is unchanged.
- **The LLM does not displace LSP for a bare identifier.** LSP identifiers are
  type-correct and cannot be hallucinated; the LLM only wins when its
  completion contains punctuation or spaces — i.e. it is predicting *code*
  rather than guessing a *name*. This kills the most annoying flicker case.
- Any anchor change resets the floor.

### 5. Opt-in

`ai.enabled`, default **false**. Nothing resolves, connects or sends until it is
turned on. A palette toggle flips it and persists.

**Test that with it off, no socket is ever opened.**

### 6. `ai_tick` pump site and `AI: Status`

Both deferred from Sprints 1–2 because they had no real consumer. They land
here, where the engine gives them something to pump and something to report.

---

## Verification

- Unit through `handle_key_command`: Tab accepts an LLM suggestion; Right
  accepts at EOL only; every other key clears; nothing is ever inserted
  without an explicit accept.
- Prefix extension: typing the predicted character advances the ghost and
  sends no request.
- Arbitration: an LLM identifier does not displace an LSP suggestion; an LLM
  code fragment does.
- **With `ai.enabled=false`, assert no connection is attempted at all.**
- pty: with a live model, type a comment and see the ghost appear, accept with
  Tab, and confirm the saved file matches. Confirm the buffer is untouched when
  the ghost is dismissed instead.
