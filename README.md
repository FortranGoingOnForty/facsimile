# facsimile
(noun) : something clever, trevor

Terminal text editor written in Fortran. VSCode-style keybindings.

## Build

```bash
fpm build
```

## Usage

```bash
./build/gfortran_*/app/fac [filename]
```

## Keybindings

### Navigation
- `arrows` - move cursor
- `ctrl-a` / `home` - smart home (toggle between first non-whitespace and column 1)
- `ctrl-e` / `end` - end of line
- `alt-left/right` - jump by word/punctuation group
- `pageup/pagedown` - page scroll
- `mouse click` - position cursor
- `mouse wheel/trackpad` - scroll viewport
- `alt-[` / `alt-]` - jump to matching bracket

### Selection
- `shift-arrows` - character selection
- `shift-alt-left/right` - word selection
- `shift-ctrl-a/e` - select to line start/end
- `shift-home/end` - select to line boundaries
- `shift-pageup/pagedown` - page selection
- `mouse drag` - select text
- `esc` - clear selection / exit multi-cursor mode

### Editing
- `backspace` / `ctrl-h` - delete backward
- `delete` - delete forward
- `tab` - insert 4 spaces (or indent selection)
- `shift-tab` - dedent selection or current line
- `ctrl-k` - kill line forward (yank stack)
- `ctrl-u` - kill line backward (yank stack)
- `ctrl-y` - yank from stack
- `ctrl-w` / `alt-backspace` - delete word backward
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
- `n` - next match (only after search)
- `N` - previous match (only after search)
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
- `tab` / `shift-tab` - switch between tabs

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
- `j` / `↓` - move down in tree
- `k` / `↑` - move up in tree

**Git Operations:**
- `a` - stage file (git add)
- `u` - unstage file (git restore --staged)
- `enter` - open file in editor

**Status Indicators:**
- Green `↑` - staged changes
- Red `✗` - modified tracked files
- Gray `✗` - untracked files

**Exit:**
- `esc` - exit fuss mode back to editor
- `ctrl-b` - toggle fuss mode off

### Help
- `ctrl-/` or `ctrl-?` - show keybindings (ctrl-/ is more reliable)

## Terminal Compatibility Notes

Some keybindings may be intercepted by your terminal emulator:
- **Ctrl+A**: Often intercepted by tmux/screen (use `Home` instead)
- **Ctrl+Shift+Z**: Intercepted by WezTerm in multi-pane mode (use `Ctrl+]` instead)
- **Ctrl+'**: Most terminals send plain apostrophe (use `Alt+'` instead)
- **Ctrl+Alt+Backspace**: Most terminals send alt-backspace (use `Alt+Shift+'` instead)

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
