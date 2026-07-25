# Sprint 4 — Multi-line blocks and partial accept

The type, the renderer and the accept path are all single-line today. This
sprint makes each of them handle a block, and adds word-at-a-time acceptance.

---

## Targets, in order

### 1. Block state, added rather than replacing

`ghost_text_t` keeps `suggestion`/`prefix` exactly as they are, so the word-scan
and LSP sources are byte-identical in behaviour. A block adds:

- `block_text` — the full LF-separated insertion
- `block_lines` — 0 means "not a block", so every existing check still works

`ghost_insert_text()` returns `block_text` when there is one and `ghost_suffix()`
otherwise. Everything that inserts goes through it.

### 2. Blocks only at end of line

**The single most important simplification in this sprint.** A block is offered
only when the caret is past the last character of its line.

- `render_line_tail_shifted` — the mid-line "open the line up" trick — is then
  never invoked for a block, so the two mechanisms never interact.
- It matches the accept rule already in place (Right accepts only at EOL).
- Mid-line block completion is incoherent anyway: does the rest of the line go
  after block line 1, or after line N?

Mid-line stays single-line only, exactly as it works today.

### 3. Rendering

Row 1 is drawn at the caret as now. Rows 2..N are drawn dim below it.

The real buffer lines those rows were showing are **pushed down**, not
overwritten — an overlay would make the file look like it had changed.
`render_line_with_selections` already draws one line's content given a row, so
the push-down reuses it.

Hard bounds: at most `ai.max_block_lines` (default 4, cap 8), and never past
the last content row, the status bar, the terminal panel, or any visible panel.
If it does not fit, show line 1 plus a dim `+N more (Tab)` marker.

### 4. Accept

One `buffer_insert_string` with the whole block, not a per-character loop.
`insert_line_text` does a gap move per character and mis-tracks `column` across
a newline; `buffer_insert` is byte-oriented and line count is derived by
scanning for LF, so embedded newlines just work.

**Always checkpoint undo for a block accept** — drop the `last_action_was_edit`
shortcut, so one Ctrl-Z removes the whole thing.

### 5. Partial accept

`ctrl-right` accepts one word and re-anchors the remainder rather than
discarding it. Useful precisely when the model is 80% right, which converts a
rejected suggestion into a partial win.

---

## Verification

- Unit: block accept produces exactly the expected buffer; undo reverts it in
  one step; the caret lands at the end of the inserted block.
- Unit: partial accept consumes one word and leaves the rest ghosted, with the
  anchor moved.
- Unit: a block is never offered mid-line.
- pty: a multi-line suggestion renders below the caret without scrolling the
  view, and the real lines below it are still visible.
