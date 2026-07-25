# Sprint 5 — Context intelligence

Where the quality comes from. Sprints 1–4 built a correct pipeline; this one
makes what goes into it worth completing.

The measurements steer this. `prompt_eval` is **7–32 ms** on GPU against
112 ms+ of generation, so **context is cheap and generation is not**. Every
decision here spends context to buy quality, and spends tokens only where it
must.

---

## Targets, in order

### 1. Prompt assembly as a pure function

`completion_prompt_module` takes (buffer, filename, line, col, options,
symbol digest) and returns prefix and suffix. No I/O, no editor state, no
network — so the whole of the rest of this sprint is unit-testable against
golden strings without a model running.

### 2. Language-aware stop sequences

The single biggest quality win available, because it fixes *overrun* rather
than trying to improve the model. A base FIM model asked for 96 tokens will
happily continue past the end of the construct and start writing the next
function.

Ollama honours `"stop": [...]`, so per-language:

- C family: a closing brace at column 1, and the start of a new top-level
  definition
- Python: `\ndef `, `\nclass ` at column 0
- Fortran: `\nend subroutine`, `\nend function`, `\nend module`
- Also, universally: a second consecutive blank line

These come from `comment_syntax_module`'s existing language detection, so no
new language table is introduced.

### 3. File header line

One line in the file's own comment syntax naming the path, prepended to the
prefix. One line, disproportionate effect: it tells the model the language and
the file's role when the window alone would not.

### 4. Guaranteed comment block

The contiguous comment block immediately above the caret is what makes
"describe it and get it written" work. It is normally inside the prefix window
anyway; this makes it guaranteed even when the budget is tight, by reserving
space for it before the ordinary window is filled.

### 5. Symbol digest

A compact list of what is defined in this file, prepended as comments. On a
large file the prefix window cannot reach a function defined 800 lines up, and
the model then invents a plausible-looking call.

Two sources, in order of preference:

- **LSP document symbols**, when the symbols panel happens to be populated.
  Accurate and type-aware, but only present if the user has opened the panel —
  fetching them proactively needs async plumbing this sprint does not add.
- **A syntactic scan of the buffer** otherwise. Language-agnostic, synchronous,
  always available. Less precise, but it covers the case that matters: naming
  things that actually exist in this file.

### 6. Recency

A small window around the most recently edited region elsewhere in the file,
which is usually what the user is working against. Cheap given `prompt_eval`
cost, and skipped when it would duplicate the main window.

---

## Verification

- Golden-string unit tests on prompt assembly across several languages and
  caret positions — the point of making it a pure function.
- Stop sequences asserted per language, including that an unknown extension
  yields none rather than a wrong one.
- Symbol digest asserted on a file with definitions far outside the window.
- A reported (non-gating) quality harness: fixture files with a known-correct
  next line, so prompt changes can be compared run to run. Models vary, so this
  informs rather than fails the build.
