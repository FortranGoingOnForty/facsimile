# Sprint 6 — Remote pool tier, model management, docs

The last sprint. Adds the on-demand deep tier, closes the capability-probe gap
carried from Sprint 3, and writes the documentation the release needs.

---

## Targets, in order

### 1. Capability probe at enable time (carried from Sprint 3)

`ollama_model_supports_fim` has existed since Sprint 2 but nothing calls it.
Today a model without FIM is caught only by the sanitizer rejecting its output,
which works but tells the user nothing useful.

At enable time:

1. `GET /api/tags` — is the backend reachable, and is the configured model
   actually pulled? If not, say exactly that, naming the model and host. This
   is the first failure a new user will hit.
2. `POST /api/show` — does the model report `insert`? If not, refuse it for
   inline completion and say so. A chat model cannot see the code after the
   caret; it guesses, and the guesses look plausible.

This is the difference between "AI completion is broken" and
`ai: model 'qwen3.5:9b' has no FIM support; inline completion disabled`.

### 2. Remote tier as its own opt-in

`ai.remote.enabled`, default **false**, **independent** of `ai.enabled`.
Turning on completion enables loopback only; reaching a remote host is a second,
deliberate decision. Source code leaving the machine deserves its own switch.

- Nothing about the remote backend is resolved or contacted while it is off.
- A persistent status-bar badge whenever a **non-loopback** host is enabled, so
  it is never ambiguous whether code is leaving the machine.

### 3. Deep completion on demand

The local 1.5b is the per-keystroke tier at ~300 ms. The remote 32b is ~640 ms
warm — too slow to fire on every keystroke, and rude on a shared box, but
excellent when explicitly asked.

`alt-\` requests a large multi-line block from the remote model, with a longer
token budget and deadline. Automatic keystroke completion stays local.

### 4. Availability

Do not evict a model someone else is using: check `/api/ps` before a deep
request and skip if the host is busy with a different model.

`~/pool-up` is the user's own availability marker on `hasu`. Reading it needs
SSH, which the editor deliberately does not do — so it is exposed as an
optional `ai.remote.gate_url`: if set, the body must be non-empty for the
remote tier to fire. That keeps the mechanism honest and general without
building an SSH client into an editor.

### 5. Failover

Remote unreachable → fall back to the local model once, with a note. Local
never falls back to remote: a 640 ms interactive ghost is worse than none.

### 6. Documentation

`docs/AI_COMPLETION.md` (new), README section, `KEYBINDINGS.md`, and a
`config_spec.md` section covering `settings.json` — which this feature
introduced and which the spec does not yet describe.

---

## Verification

- **With `ai.remote.enabled` false, the remote address is never resolved and no
  connection to it is attempted.** Asserted directly.
- Capability probe: a model that does not report `insert` is refused with a
  message naming it, rather than silently producing rejected completions.
- Deep request uses the remote backend when enabled and the local one when not.
- The status badge appears only for a non-loopback host that is actually enabled.
