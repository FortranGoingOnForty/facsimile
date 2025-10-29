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
- `/` - search forward
- `n` - next match (only after search)
- `N` - previous match (only after search)
- `ctrl-r` - find and replace (deprecated - use `/` for search)

### Special
- `alt-'` - cycle quotes: " → ' → ` → "
- `alt-shift-'` - remove surrounding brackets/quotes
- `ctrl-z` - undo
- `ctrl-]` / `ctrl-shift-z` - redo (use ctrl-] if terminal intercepts ctrl-shift-z)
- `ctrl-l` - clear/redraw screen

### File
- `ctrl-s` - save
- `ctrl-q` - quit

### Help
- `ctrl-?` - show keybindings

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
