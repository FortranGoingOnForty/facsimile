# facsimile
(noun) : something clever, trevor

Terminal text editor written in Fortran. VSCode-style keybindings.

## Build

### Using Make (Recommended)
The Makefile provides optimized, platform-specific builds:
- **macOS arm64**: Uses flang-new for better apple silicon support
- **macOS Intel/Linux**: Uses gfortran with standard optimization flags

```bash
make
./fac [filename]
```

### Using fpm (Development)
```bash
fpm build
./build/gfortran_*/app/fac [filename]
or 
fpm run -- [filename]
```

## Keybindings

### Navigation
- `arrows` - move cursor
- `ctrl-a` / `home` - smart home (toggle between first non-whitespace and column 1)
- `ctrl-e` / `end` - end of line
- `alt-left/right` - jump by word/punctuation group
- `pageup/pagedown` - page scroll
- `alt-[` / `alt-]` - jump to matching bracket

### Mouse

- `click` - position the cursor; with the file tree open this also takes focus
  and closes the tree, since the tree owns the keyboard while it is up
- `drag` - select text (left button only)
- `alt-click` - add or remove a cursor
- `right-click` (or `ctrl-click`, for a one-button pointer) - context menu at
  the pointer; `shift-f10` or `alt-z` opens the same menu at the caret.
  Right-clicking inside a selection keeps it, so Cut and Copy act on the
  selection rather than the line
- `wheel` - scrolls whatever is under the pointer: the pane, an inactive pane,
  or the terminal panel's scrollback. Over the tab bar or status bar it does
  nothing rather than moving a document you are not pointing at
- click a `[n: name]` tab to switch to it
- click a file tree row to open it, a directory row to expand it; right-click a
  row for open-in-split and the git actions (stage, unstage, diff) that
  otherwise hide behind the `ctrl-g` prefix
- click the `»`/`«` chevron in the bottom-left corner to toggle the file tree;
  it points the way the tree will move (`ctrl-b` still does the same)

### Selection
- `shift-arrows` - character selection
- `shift-alt-left/right` - word selection
- `shift-ctrl-a/e` - select to line start/end
- `shift-home/end` - select to line boundaries
- `shift-pageup/pagedown` - page selection
- `alt-a` - select all
- `esc` - clear selection / exit multi-cursor mode

### Editing
- `backspace` / `ctrl-h` - delete backward
- `delete` - delete forward
- `tab` - indent to the next tab stop, or indent the selection (a hard tab in Makefiles, where spaces are a syntax error)
  - on a line holding only whitespace it jumps straight to where the line belongs, read from the block above, so re-entering a nested block is one press
- `backspace` in leading whitespace - removes a whole indent level, not one space
- `shift-tab` - dedent selection or current line
- `ctrl-/` - toggle line comment (indent-aware; comments whole lines for a partial selection)
- `ctrl-k` - kill line forward (yank stack)
- `ctrl-shift-k` - delete line outright (no clipboard, no yank stack)
- `ctrl-u` - kill line backward (yank stack)
- `ctrl-y` - yank from stack
- `ctrl-w` / `alt-backspace` - delete word backward (at column 1 it takes the line break, so it eats blank lines)
- `alt-d` / `alt-delete` / `fn-alt-backspace` - delete word forward
- `ctrl-t` - transpose characters
- `ctrl-j` - join lines

### Clipboard
- `ctrl-x` - cut line/selection
- `ctrl-c` - copy line/selection
- `ctrl-v` - paste

### Lines
- `alt-up/down` - move line up/down
- `alt-shift-up/down` - duplicate line up/down

### Multiple Cursors
- `ctrl-d` - select next match (creates selections + cursors)
- `alt-click` - add/remove cursor at position
- `ctrl-alt-up` - add cursor on line above
- `ctrl-alt-down` - add cursor on line below
- `esc` - exit multi-cursor mode (keep active cursor only)

### Search
- `ctrl-f` - search forward
- `ctrl-r` - find and replace

### Special
- `alt-'` - cycle quotes: " → ' → ` → "
- `alt-shift-'` - remove surrounding brackets/quotes
- `ctrl-z` - undo
- `ctrl-]` / `ctrl-shift-z` - redo (use ctrl-] if terminal intercepts ctrl-shift-z)
- `ctrl-l` - clear/redraw screen

### File
- `ctrl-s` - save
- `ctrl-q` - quit
- `ctrl-b` - toggle file tree (fuss mode)

### Tab Management
- `ctrl-t` - create new tab
- `ctrl-pageup` / `ctrl-pagedown` - previous / next tab
- `alt-1` .. `alt-9` - jump to a tab by number (`alt-0` is tab 10)
  - a further digit within half a second extends it: `alt-1` then `5` goes to tab 15
  - if the first digit lands on a tab inside a group, the next digit picks that
    group's Nth member instead; the status bar says which it will be

### Integrated Terminal
- `f5` / `alt-t` - toggle the terminal panel
- `ctrl-shift-up` / `ctrl-shift-down` - taller / shorter, while the terminal has focus
- `ctrl-shift-m` - maximize, or go back to the previous height
- drag the separator bar (marked `⇕`) to resize it with the mouse

The height is kept as a fraction of the screen, so the panel holds its
proportion when the window is resized, and it is remembered per workspace.
Set `"terminal.height_percent"` in `settings.json` to change where it starts.

`fac` typed at the panel's prompt opens in the session you are already in,
rather than starting a second editor inside the first:

```
$ fac notes.md      # opens as a tab here
$ fac src/parser    # opens as a tab group here
```

A directory means something different in the panel than outside it. From a
normal shell `fac src/parser` opens that directory as a whole new workspace,
which is unchanged; from the panel you already have a workspace, so it opens
the group dialog instead. Focus moves out of the terminal either way. Pass
`-w` to opt out and get a separate editor.

### Tab Groups
Open a directory as a sub-workspace: press `enter` on a directory in the file
tree and tick the files you want. The group appears in the tab bar as
`src/ (4)`, and its members get their own row while you are inside it.

- `super+ctrl+left` / `right` - previous / next file; steps through a group's members, and off the end of one leaves it
- `super+ctrl+down` / `up` - enter a group / leave it from any member in one press
- `ctrl-pageup` / `ctrl-pagedown` or `alt-ctrl-left` / `right` - the same, for terminals that do not report Super

Left/right is one continuous line through every open file, so it always gets you
out of a group even if your window manager eats the Super bindings.

Hovering a group previews its members without moving your text, and the preview
stays while the pointer is over the group's own area, so you can move down into
the members rather than watching them vanish as you reach for them. Group members
are read from disk the first time you look at them, so a forty-file group costs
one file read, not forty. Groups are saved in `workspace.json` and come back on
restart. See [docs/KEYBINDINGS.md](docs/KEYBINDINGS.md#tab-groups) for detail.

### Pane Management
Split your view into multiple panes for side-by-side editing of the same file.

**Creating Panes:**
- `alt-v` - split pane vertically (creates pane to the right)
- `alt-s` - split pane horizontally (creates pane below)

**Navigating Panes:**
- `alt-h` / `ctrl-shift-left` - move to left pane
- `alt-l` / `ctrl-shift-right` - move to right pane
- `alt-k` / `ctrl-shift-up` - move to pane above
- `alt-j` / `ctrl-shift-down` - move to pane below

**Managing Panes:**
- `alt-q` - close current pane only
- `ctrl-w` - close current pane (closes tab when last pane)

**Features:**
- Each pane has independent viewport and cursor
- Line numbers display in all panes
- Active pane shows with visible cursor
- Inactive panes have subtle dark background
- Minimum pane size enforced (20 columns)

### File Tree (Fuss Mode)
When in fuss mode (ctrl-b), you get a split view with a git-aware file tree on the left (30%) and editor on the right (70%).

**Navigation:**
- `↑` / `↓` - move to the previous/next visible row, whatever its depth
- `→` - descend into a directory (expanding it first if needed)
- `←` - go back out to the parent

**Opening Files:**
- `enter` or `o` - open file in new tab

**Display Options:**
- `.` - toggle hiding dotfiles and gitignored files
  - When enabled, both dotfiles and gitignored files are hidden from view
  - Directories containing only hidden files are greyed out but remain visible
  - Uses `git check-ignore` to detect gitignored files

**Git Operations:**
- `a` - stage file (git add)
- `u` - unstage file (git restore --staged)

**Status Indicators:**
- Green `↑` - staged changes
- Red `✗` - modified tracked files
- Gray `✗` - untracked files

**Memory:**
- closing and reopening the tree keeps the directories you had open, and
  whether hidden files were showing
- folders holding an open file are opened up to, even hidden or gitignored
  ones; everything else hidden stays hidden. Open tabs are saved with the
  workspace, so this survives a restart

**Exit:**
- `esc` - exit fuss mode back to editor
- `ctrl-b` - toggle fuss mode off

### AI inline completion (opt-in)

Complete code inline from a local model, shown as dim shadow text. Reads the
comment above the caret and the code on both sides of it, so it completes
intent rather than matching identifiers that already exist.

**Off by default** — nothing runs, connects, or leaves your machine until you
turn it on with `alt-i` (or `ctrl-p` → *AI: Toggle Inline Completion*).
`alt-i` is also the quick off switch — instant, and it clears anything on screen.

- `tab` - accept the suggestion
- `right` - accept, at end of line
- `ctrl-right` / `alt-right` - accept one word / one line
- `alt-\` - deep completion here (bigger model, longer budget)
- any other key dismisses it

Reaching a *remote* model is a second, separate opt-in, and shows a permanent
`[AI->host]` badge in the status bar while active.

See **[docs/AI_COMPLETION.md](docs/AI_COMPLETION.md)** for setup, settings and
troubleshooting.

### Help
- `ctrl-?` (ctrl-shift-/) or `F1` - show keybindings

`ctrl-/` and `ctrl-?` are the same byte (0x1F) in the legacy terminal
encoding, so `fac` negotiates the kitty keyboard protocol at startup to tell
them apart. In terminals that decline it (and inside `tmux` without
`set -g extended-keys on`), `ctrl-/` toggles a comment and `F1` opens help.

## Terminal Compatibility Notes

Some keybindings may be intercepted by your terminal emulator:
- **Ctrl+A**: Often intercepted by tmux/screen (use `Home` instead)
- **Ctrl+Shift+Z**: Intercepted by WezTerm in multi-pane mode (use `Ctrl+]` instead)
- **Ctrl+'**: Most terminals send plain apostrophe (use `Alt+'` instead)
- **Ctrl+Alt+Backspace**: Most terminals send alt-backspace (use `Alt+Shift+'` instead)
- **F2**: Frequently grabbed by the window manager or desktop shell (GNOME, KDE and
  several tiling WMs bind it for "rename"), so no program ever sees it — use
  `Alt+N` for rename symbol instead. `python3 tools/keycap.py` shows whether a key
  reaches the terminal at all.

For WezTerm users, add to `~/.wezterm.lua` to enable Ctrl+Shift+Z:
```lua
config.keys = {
  { key = 'Z', mods = 'CTRL|SHIFT', action = wezterm.action.DisableDefaultAssignment },
}
```

## Implementation

Gap buffer for text storage. Pure Fortran with ANSI escape sequences.

## License

MIT
