# Sprint 1 — Settings and HTTP transport

Foundation for local-model shadow text. **No AI behaviour and no user-visible change**
beyond one diagnostic command. The point of this sprint is that everything the later
sprints stand on is proven in isolation first.

Prior art and constraints live in the plan; the load-bearing ones are repeated here so
this file stands alone.

---

## Targets, in order

### 1. SIGPIPE cannot be allowed to kill the editor

**Do this first, before any socket exists.**

`fac` installs no signal handlers anywhere — `grep -rn "SIGPIPE\|signal(\|sigaction" src/ app/`
returns nothing. The default disposition of SIGPIPE is *terminate the process*. A
`send()` to a peer that has closed (ollama restarted, laptop suspended, model evicted)
would therefore kill the editor and take every unsaved buffer with it.

Three independent defences, all of them:

- `signal(SIGPIPE, SIG_IGN)` once in `ai_http_init()`
- `MSG_NOSIGNAL` on every `send()` (Linux)
- `SO_NOSIGPIPE` via `setsockopt` after socket creation (macOS, where `MSG_NOSIGNAL`
  does not exist)

**Done when:** a test closes the listening socket mid-request and the editor survives.

### 2. `src/workspace/settings_module.f90`

`fac` has no configuration system — `config_module` resolves the XDG *directory* and
nothing else, and `state.json` holds three fields via hand-rolled `index()` matching.
This sprint builds the first real one, because every later sprint needs settings and
because `docs/config_spec.md` already specifies conventions nothing implements.

- File: `~/.config/fac/settings.json` (via the existing `get_config_dir`).
- Flat dotted keys — `ai.enabled`, `ai.local.model`, `ai.debounce_ms`. A dotted key keeps
  the reader a line scanner; no nested-object parser, no dependency on `json_module`
  (which leaks every parsed object and cannot decode `\uXXXX` — see the plan).
- Typed getters, each taking the default at the call site so there is no defaults table
  to drift:
  - `settings_get_logical(key, default) result(v)`
  - `settings_get_integer(key, default) result(v)`
  - `settings_get_string(key, default) result(v)`
- `settings_set_*` + `settings_save()`.
- **Atomic write**: write `settings.json.tmp`, then `rename()`. Copy the pattern already
  in `src/workspace/backup_module.f90:110-138`. Fortran has no `rename` intrinsic; if a
  C helper is needed put it in the new AI C file rather than shelling out — the config
  spec explicitly forbids executing commands from config handling.
- Missing file is **normal**, not an error: every key falls back to its default.
- Corrupt file → move aside to `settings.json.corrupted` and continue on defaults, per
  `docs/config_spec.md:448-480`.
- Load once into memory; do not re-read per key.

**Done when:** round-trip, defaults-on-missing, corrupt-recovery and atomicity all pass.

### 3. `src/ai/ai_http.c` + `src/ai/ai_http_module.f90`

Non-blocking HTTP/1.1 to `host:port`. One implementation serves both `127.0.0.1:11434`
and `hasu` over Tailscale — no TLS (Tailscale is already encrypted, loopback needs
none), so no new dependencies and no `LDFLAGS`.

Why not the alternatives:
- `lsp_process_wrapper.c` writes every message body to `/tmp/fac_lsp_read.log` (6 call
  sites, always on). Routing source code through it would write user code to a
  world-readable file. It also does a single non-blocking `write()` with no retry queue,
  which truncates any body larger than the socket buffer.
- Shelling out to `curl` blocks the editor for the whole generation, or requires writing
  source code to `/tmp` and polling it.

State machine, pumped once per main-loop iteration:

```
IDLE -> CONNECTING -> SENDING -> RECV_HEAD -> RECV_BODY -> DONE | ERROR
```

- `socket()` + `fcntl(O_NONBLOCK)` (not `SOCK_NONBLOCK` — macOS), `TCP_NODELAY`.
- `connect()` returns 0 on loopback or `-1/EINPROGRESS`; poll writability with
  `poll(POLLOUT, timeout=0)`, then `getsockopt(SO_ERROR)` to get the real result.
- `SENDING` keeps an offset and retries on `EAGAIN` — partial writes are expected for a
  multi-KB body.
- `RECV_HEAD` accumulates into a fixed header region, scans for `CRLFCRLF`, parses the
  status line and `Content-Length`. Overflow → `ERROR`.
- `RECV_BODY` counts down `Content-Length`, or reads to EOF. **`Content-Length` is
  sufficient** — verified that ollama sets it when `"stream": false`. Chunked decoding is
  deferred to the streaming work, not built speculatively.
- Send `Connection: close` and open a fresh connection per request: connect is ~50 µs on
  loopback and 4.7 ms to `hasu`, against 250–1000 ms of inference. This removes the whole
  keep-alive failure surface and makes EOF a valid terminator.
- Hard caps: max body bytes, a wall-clock deadline checked with `CLOCK_MONOTONIC`, and
  `ai_http_abort()` which closes the socket. Closing the socket also **cancels generation
  server-side**, which matters on a shared host.

`getaddrinfo` blocks and has no async form. Resolve **once at enable time**, cache the
`sockaddr`, and never resolve on the completion path. Prefer a numeric IP in config.

Fortran side mirrors the `lsp_*_f` naming:

```
ai_http_init_f()                       ! ignore SIGPIPE, once
ai_http_resolve_f(host, len, port, addr, ok)   ! BLOCKING, enable-time only
ai_http_begin_f(addr, body, body_len, deadlines, handle)
ai_http_pump_f(handle, state, status, nbytes)  ! non-blocking, one step
ai_http_take_f(handle, buf, cap, len)
ai_http_abort_f(handle)
```

### 4. Pump site

`ai_tick(editor, buffer)` called once per main-loop iteration, immediately after
`process_server_messages(editor%lsp_manager)` at `app/main.f90:561`.

**No callback plumbing.** The LSP trampoline workaround (`saved_editor_for_callback`)
exists only because LSP callbacks fire deep inside `process_server_messages` with no
access to `editor`. `ai_tick` is called from the loop where `editor` and `buffer` are
both in scope — no procedure pointers, no saved pointers, no one-shot registry.

Cost when idle with no request: one integer compare, zero syscalls.

### 5. Build registration

- `src/ai/ai_http.c` → `C_SOURCES` in the Makefile (fpm picks up `src/**/*.c`).
- New modules → `SOURCES` in dependency order; `.NOTPARALLEL` is set so order matters.
- Windows: raw sockets need winsock2 and `-lws2_32`, and there is currently no
  `LDFLAGS`/`LIBS` variable at all. **Decide explicitly this sprint** — either `#ifdef
  _WIN32` with a Windows-only link variable, or compile the feature out on Windows and
  say so in the README. Do not discover this at release.

### 6. `AI: Status` diagnostic command

Palette command performing `GET /api/tags` against the configured host and reporting the
result in the status bar. This is the only user-visible output of the sprint, and it is
what proves the transport end to end.

---

## Verification

- **Unit — settings**: round-trip every type; defaults when the file is absent; a
  truncated/garbage file recovers to defaults and leaves a `.corrupted` copy; killing
  between write and rename leaves the previous file intact.
- **Unit — HTTP**: against a `python3 -m http.server`-style fixture on 127.0.0.1 —
  `Content-Length` body, slow drip (body arriving across many pumps), connection refused,
  peer close mid-response, body over the cap, deadline expiry, abort mid-flight.
- **Survival**: close the listener while a request is in flight; assert the process is
  still alive (this is the SIGPIPE test, and it is the one that matters most).
- **Timing**: with a request in flight, assert a full `ai_tick` stays under ~2 ms so the
  50 ms input loop is untouched.
- No change to any existing test. The full suite must stay green: 18 unit programs and
  7 integration suites at the start of this sprint.

---

## Out of scope

Prompt construction, FIM, sanitization, ghost integration, model selection, the remote
tier. Sprint 1 ends with a transport that can fetch a URL and a settings file that can
remember whether the feature is on — nothing more.
