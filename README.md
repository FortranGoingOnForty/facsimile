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
- `ctrl-a` / `home` - start of line
- `ctrl-e` / `end` - end of line
- `alt-left/right` - word jump
- `pageup/pagedown` - page scroll
- `click` - position cursor
- `alt-click` - add/remove cursor

### Selection
- `shift-arrows` - character selection
- `shift-alt-left/right` - word selection
- `shift-ctrl-a/e` - select to line start/end
- `shift-home/end` - select to line boundaries
- `shift-pageup/pagedown` - page selection

### Editing
- `backspace` / `ctrl-h` - delete backward
- `delete` - delete forward
- `tab` - insert 4 spaces
- `ctrl-k` - kill line forward (yank stack)
- `ctrl-u` - kill line backward (yank stack)
- `ctrl-y` - yank from stack
- `ctrl-w` / `alt-backspace` - delete word backward
- `alt-d` - delete word forward
- `ctrl-t` - transpose characters

### Clipboard
- `ctrl-x` - cut line/selection
- `ctrl-c` - copy line/selection
- `ctrl-v` - paste

### Lines
- `alt-up/down` - move line
- `alt-shift-up/down` - duplicate line

### Multiple Cursors
- `ctrl-d` - select next match
- `alt-click` - add/remove cursor

### Special
- `ctrl-'` - cycle quotes (", ', `)
- `ctrl-opt-backspace` - remove surrounding brackets

### File
- `ctrl-s` - save
- `ctrl-q` - quit

### Help
- `ctrl-?` - show keybindings

## Implementation

Gap buffer for text storage. Pure Fortran with ANSI escape sequences.

## License

MIT
