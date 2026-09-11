# fac keybindings

Every key `fac` binds, and the reasoning behind the ones that look strange.
Where two chords do the same thing, the `Alt` one is the one that survives
contact with a real terminal — see [Tips](#tips) for why.

`F1` inside the editor opens a scrollable quick-reference modal with the most
useful entries from this list.

## Navigation

| Keybinding | Command | Description |
|------------|---------|-------------|
| `↑` `↓` `←` `→` | Move Cursor | Move cursor one character/line |
| `Alt+↑` | Move Line Up | Move current line up |
| `Alt+↓` | Move Line Down | Move current line down |
| `Home` or `Ctrl+A` | Line Start | Jump to beginning of line (smart toggle) |
| `End` or `Ctrl+E` | Line End | Jump to end of line |
| `Ctrl+Home` | File Start | Jump to beginning of file |
| `Ctrl+End` | File End | Jump to end of file |
| `Alt+←` / `Alt+→` | Word Jump | Move cursor by word (`Ctrl+←` / `Ctrl+→` and `Alt+B` / `Alt+F` do the same) |
| `Alt+Shift+↑` / `Alt+Shift+↓` | Duplicate Line | Copy the line up or down |
| `Alt+,` | Jump Back | Return to the previous location on the jump stack |
| `Page Up` / `Page Down` | Page Scroll | Move up/down one screen |
| `Alt+[` / `Alt+]` | Match Bracket | Jump to matching bracket/paren |

---

## Editing

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+Z` | Undo | Undo last change |
| `Ctrl+Shift+Z` or `Ctrl+]` | Redo | Redo last undone change |
| `Shift+F10` or `Alt+Z` | Context Menu | Open the context menu at the caret (right-click opens it at the pointer) |
| `Tab` | Indent | Advance to the next tab stop. On a line holding only whitespace it jumps straight to the indentation the line belongs at, judged from the block above — so getting back into a nested block is one press, not four |
| `Backspace` (in leading whitespace) | Unindent | Removes a whole indent level at a time rather than one space |
| `Ctrl+/` | Toggle Line Comment | Comment/uncomment the cursor's lines or the selection |
| `Ctrl+Shift+K` | Delete Line | Delete the cursor's lines outright — nothing is copied or yanked (needs a terminal supporting the kitty keyboard protocol; otherwise use the command palette) |
| `Alt+Backspace` | Delete Word | Delete word backward; at column 1 it deletes the line break, so blank lines are eaten one press at a time |
| `Backspace` | Delete Char | Delete character backward |
| `Delete` | Delete Forward | Delete character forward |
| `Alt+D` or `Alt+Delete` | Delete Word Forward | Delete the word after the caret |
| `Ctrl+K` / `Ctrl+U` | Kill Line | Cut forward to end of line / back to line start, onto the yank stack |
| `Alt+Shift+J` | Join Lines | Pull the next line onto this one |
| `Alt+'` | Cycle Quotes | Rotate the quoting around the caret: `"` → `'` → `` ` `` → `"` |
| `Ctrl+Alt+Backspace` or `Alt+Shift+'` | Unwrap | Remove the innermost surrounding brackets or quotes while keeping the caret on its content |
| `Shift+Tab` | Dedent | Dedent the selection, or the current line |
| `Ctrl+Shift+S` | Save All | Write every modified tab |

`Ctrl+Alt+Backspace` needs a terminal that speaks CSI-u (kitty, foot, WezTerm,
recent Ghostty). Use `Alt+Shift+'` for the same command elsewhere.

### Inline completion

Off until you turn it on. See [AI_COMPLETION.md](AI_COMPLETION.md).

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Alt+I` / `Alt+Shift+I` | Toggle | Turn inline completion on or off. Off is instant; on probes the backend and reports what it found |
| `Tab` | Accept | Take the whole suggestion |
| `Ctrl+→` | Accept One Word | Take a single word, keeping the rest offered |
| `Alt+→` | Accept One Line | Take one line of a multi-line suggestion, keeping the rest |
| `Alt+\` | Deep Completion | Ask the large model for a bigger block |

Any other key dismisses the suggestion.

---

## Selection

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Shift+↑↓←→` | Select | Extend selection with arrow keys |
| `Shift+Home` / `Shift+End` | Select to Line Edge | Select to the start or end of the line (`Ctrl+Shift+A` / `Ctrl+Shift+E` do the same) |
| `Shift+PageUp` / `Shift+PageDown` | Select by Page | Extend the selection a screen at a time |
| `Alt+Shift+←` / `Alt+Shift+→` | Select by Word | Extend the selection a word at a time (`Ctrl+Shift+←` / `→` do the same) |
| `Alt+A` | Select All | Select entire file (`Ctrl+A` is Line Start) |
| `Ctrl+D` | Select Next | Add cursor at next match of selection |
| `Esc` | Clear Selection | Deselect and return to single cursor |

---

## Search & Replace

`Ctrl+F` opens the **find bar**. If the caret is on a word, that word is
already in it and every occurrence is lit — the one you are on in orange, the
rest in yellow. The bar stays up while you walk between them.

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+F` | Find | Open the find bar, seeded from the word under the caret |
| `Ctrl+G` | Go to Line | Jump to specific line number |
| **In the find bar:** | | |
| `↓` `→` `PgDn` `Enter` `Tab` `Space` | Next Match | Step forward |
| `↑` `←` `PgUp` `Shift+Enter` `Shift+Tab` `Shift+Space` | Previous Match | Step back |
| `Home` / `End` | First / Last | Jump to the first or last match |
| any character | Edit | Replace the seeded pattern and search live as you type |
| `Alt+↑` / `Alt+↓` | History | Walk previous searches |
| `Alt+C` | Toggle Case | Case-sensitive matching |
| `Alt+W` | Toggle Whole Word | Whole-word matching |
| `Alt+R` | Toggle Regex | POSIX regex matching |
| `Alt+S` | Search in Selection | Confine the search to the selection |
| `Ctrl+R` | Replace One | Replace the current match and step on |
| `Ctrl+A` | Replace All | Replace every match |
| `Ctrl+F` | Close | Put the bar away, leaving the matches lit for `n` / `N` |
| `Esc` | End Search | Close the bar and clear the highlights |
| **After the bar is closed with `Ctrl+F`:** | | |
| `n` / `N` | Next / Previous | Keep walking the matches |
| `Ctrl+D` | Select Word & Find | Select the word and jump to its next match |

Two keys mean something to both the text field and the match list. The rule
is the same for both: **while you are typing, `Space` and `Tab` belong to the
field; the moment you navigate, they belong to the matches.** Opening the bar
on a word does not count as typing, so every navigation key is live
immediately.

`Shift+Enter` and `Shift+Space` need a terminal that speaks CSI-u (kitty,
foot, WezTerm, recent Ghostty); everything else in the table works anywhere.

---

## Clipboard & Yank Stack

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+X` | Cut | Cut selection and add to yank stack |
| `Ctrl+C` | Copy | Copy selection to clipboard |
| `Ctrl+V` | Paste | Paste from clipboard |
| `Ctrl+K` | Kill Line Forward | Cut from the caret to end of line, onto the yank stack |
| `Ctrl+U` | Kill Line Backward | Cut from line start to the caret, onto the yank stack |
| `Ctrl+Y` | Yank | Paste the top of the yank stack |

---

## Multiple Cursors

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+D` | Select Word & Next | Select word under cursor, add cursor at next match |
| `Alt+C` | Match Case | Toggle case sensitivity for `Ctrl+D` matching |
| `Alt+Click` | Add/Remove Cursor | Add or remove cursor at mouse position |
| *(see the Mouse section below for the rest)* | | |
| `Ctrl+Alt+↑` | Add Cursor Above | Add cursor on line above |
| `Ctrl+Alt+↓` | Add Cursor Below | Add cursor on line below |
| `Super+↑` / `Super+↓` | Add Cursor Above / Below | The same, for terminals that report Super |
| `Ctrl+Shift+Alt+↑` / `↓` | Add Cursor Above / Below | The same again, for desktops that eat the other two |
| `Esc` | Single Cursor | Return to single cursor mode |

On GNOME and KDE, `Ctrl+Alt+↑`/`↓` switches workspace and `Super+↑`/`↓` tiles
or maximises the window, so neither reaches the editor on a stock desktop.
`Ctrl+Shift+Alt+↑`/`↓` is bound to the same commands as a chord that neither
of them claims. `Alt+Click` avoids the question entirely.

| *(while multiple cursors active)* | | |
| Type normally | Edit All | Type at all cursor positions |
| Arrow keys | Move All | Move all cursors together |
| `Backspace`/`Delete` | Delete All | Delete at all cursors |

---

## File Operations

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+S` | Save | Save current file. Caps lock and a held Shift make no difference — a chord's letter case is ignored |
| `Ctrl+Shift+S` | Save All | Write every modified tab |
| `Ctrl+Q` | Quit | Exit editor (prompts if unsaved) |
| `Ctrl+O` | Open File | Open file browser (Fortress mode) |
| `Ctrl+T` | New Tab | Create new empty tab |

---

## Tabs & Windows

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+T` | New Tab | Create new empty tab |
| `Ctrl+W` | Close Tab | Close the current tab, prompting if it has unsaved changes. `Alt+Q` is the one that closes a single pane |
| `Ctrl+PageDown` or `Ctrl+Alt+Right` | Next Tab | Switch to next tab |
| `Ctrl+PageUp` or `Ctrl+Alt+Left` | Previous Tab | Switch to previous tab |
| `Alt+1` to `Alt+9` | Jump to Tab | Inside a tab group, switch to that visibly numbered member; otherwise switch to the global numbered tab. A missing local member falls through to the global tab. `Alt+0` means 10 |
| `Ctrl+1` to `Ctrl+9` | Jump to Group | Switch to the Nth tab group from the left, ignoring loose tabs. `Ctrl+0` is group 10 |
| ...then another digit | Extend the Jump | Within half a second a further digit greedily extends a tab, active-group member, or group number. If the composite does not exist, its final digit is retried as a fresh jump. After a global Alt jump lands inside a group, the next digit instead picks the visibly numbered Nth member |
| `Alt+V` | Split Vertical | Split current pane vertically |
| `Alt+S` | Split Horizontal | Split current pane horizontally |
| `Alt+Q` | Close Pane | Close current pane only |
| `Alt+H` | Navigate Left | Move to pane on the left. No `Ctrl+Shift+Left` alias — that extends the selection by a word |
| `Alt+L` | Navigate Right | Move to pane on the right. Likewise |
| `Alt+K` or `Ctrl+Shift+Up` | Navigate Up | Move to pane above. While the terminal panel has focus this resizes it instead — pane navigation would have nowhere to go |
| `Alt+J` or `Ctrl+Shift+Down` | Navigate Down | Move to pane below. Same exception as above |

---

## File Tree (Fuss Mode)

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+B` or `F3` | Toggle Tree | Show/hide file tree |
| **In Tree:** | | |
| `↑` / `↓` | Navigate | Move to the previous or next VISIBLE row, whatever its depth — not to the next sibling |
| `→` | Enter | Descend into a directory, expanding it first if needed |
| `←` | Exit | Collapse an open directory, or go out to the parent |
| `Enter` | Open | Open the file in a new tab |
| `Alt+V` / `Alt+S` | Open in Split | Open the file in a vertical or horizontal split |
| `Space` | Toggle Expand | Expand or collapse a directory (scanned lazily, on first open) |
| letters | Fuzzy Search | Typing filters the tree. This is why `j`/`k` do not navigate — they are search input |
| `.` | Toggle Hidden | Show or hide dotfiles and gitignored entries |
| `Ctrl+/` | Hints | Expand or collapse the hint line |
| `Esc` | Close Tree | Hide file tree |

---

## Tab groups

| Key | Action |
|---|---|
| `Super+Ctrl+Left` / `Right` | Previous / next file — steps *through* a group's members, and off the end of one leaves it |
| `Super+Ctrl+Down` | Enter the group on the top row |
| `Super+Ctrl+Up` | Leave the current group, from any member, in one press |
| `Ctrl+PageUp` / `PageDown` | Same as Super+Ctrl+Left/Right |
| `Alt+Ctrl+Left` / `Right` | Same again, for terminals that do not report Super |

Left/right is one continuous line through every open file. Stepping into a
group lands on the member nearest the side you came from — moving right enters
at the first member, moving left at the last — so retracing your steps visits
the same files in reverse. Stepping off the last member leaves the group.

That means left/right alone will always get you out of a group, which matters
because `Super+Ctrl+Up` is the binding a window manager is most likely to take.
For a group with many members, `Super+Ctrl+Up` still leaves in a single press
from wherever you are. `Ctrl+1`-`Ctrl+9` jump directly to groups from left to
right; after an Alt jump lands in a group, its continuation digit uses the
visible member numbers on row two.

Super is reported by kitty, ghostty, foot and wezterm; elsewhere the window
manager often takes `Super+Arrow` before the terminal sees it. The `Ctrl+PageUp`
and `Alt+Ctrl+Arrow` bindings are the ones that work everywhere — treat Super as
the enhancement, not the headline.

The file tree keeps its place: closing and reopening it leaves the same
directories open and the same hidden/shown choice in effect. It also opens the
folders holding files you have open, even hidden or gitignored ones — having a
file open in a directory is taken as a better answer to "should this be shown"
than the name it starts with. Other hidden entries stay hidden, and because
open tabs are already saved with the workspace, this survives a restart.

Hovering a group on the tab bar previews its members without moving your text.
The preview stays while the pointer is somewhere that belongs to the group —
its entry on the top row, or the member strip itself — so you can move down into
the members you are previewing. Moving anywhere else hides it again: sideways
off the entry, or on down into the document.
Set `"tabs.group_hover_preview": false` in `settings.json` to turn that off; the
pinned member row still works, and it stops the editor asking the terminal to
report every pointer movement.

---

## Command & Utility

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+P` | Command Palette | Search and execute any command |
| `F5` or `Alt+T` | Integrated Terminal | Toggle the terminal panel |
| `Ctrl+Shift+Up` / `Down` | Resize Terminal | Taller / shorter — **only while the terminal has focus** |
| `Ctrl+Shift+M` | Maximize Terminal | Fill the screen, or go back to the previous height |
| `Ctrl+?` or `F1` | Help | Open the help modal (`Ctrl+?` needs a terminal that supports the kitty keyboard protocol; `F1` always works) |
| `Ctrl+B` or `F3` | File Tree | Toggle file explorer (Fuss mode) |
| `Ctrl+L` | Redraw Screen | Clear and redraw the screen |
| `Esc` | Cancel/Close | Close panels, cancel operations. In the terminal panel it closes the panel only from a bare shell prompt — with text on the line, or inside a full-screen program, the shell gets it |

---

## LSP Features

| Keybinding | Command | Description |
|------------|---------|-------------|
| `F12` or `Ctrl+\` or `Alt+G` | Go to Definition | Jump to where a symbol is defined |
| `Shift+F12` or `Alt+R` | Find References | Find all usages of a symbol |
| `F2` or `Alt+N` | Rename Symbol | Rename symbol across entire project |
| `F10` or `Alt+.` | Code Actions | Quick fixes and refactorings |
| `F8` or `Alt+E` | Diagnostics Panel | Show all errors and warnings |
| `F4` or `Alt+O` | Document Symbols | Navigate symbols in current file |
| `F6` or `Alt+P` | Workspace Symbols | Search symbols across all files |
| `Shift+Alt+F` | Format Document | Auto-format current file |
| `Ctrl+Space` | Completion | Ask the server for completions here |
| `Ctrl+H` | Hover | Show type and documentation for the symbol under the caret |
| `Alt+M` | Server Manager | Open the language-server installer panel |
| `Alt+,` | Jump Back | Return to previous location (jump stack) |

---

## Panel Navigation

When any panel is open (Diagnostics, References, Symbols, etc.):

| Keybinding | Command | Description |
|------------|---------|-------------|
| `↑` `↓` or `j` `k` | Navigate Items | Move selection up/down |
| `Enter` | Select/Jump | Jump to selected item |
| `Esc` | Close Panel | Close the panel |
| Type characters | Filter/Search | Narrow down results (in symbols panels) |

---

## Special Modes

### Find Bar
Active when `Ctrl+F` is pressed. See the Search & Replace section above for
the full key list. In short: arrows, page keys, `Enter`, `Tab` and `Space`
step through the matches; add `Shift` to go back; typing edits the pattern
and re-searches live; `Ctrl+F` or `Esc` puts it away.

### Fuss (File Tree) Mode
Active when `Ctrl+B` or `F3` is pressed. See the File Tree section above for
the keys. In short: arrows move and descend, `Enter` opens, `Space` expands,
typing filters, and everything git is behind `Ctrl+G`.

**Git operations in the tree** — all behind a `Ctrl+G` prefix, pressed first.
Bare letters are fuzzy-search input, so the prefix is what keeps both usable:

- `Ctrl+G` `a` - Stage file
- `Ctrl+G` `u` - Unstage file
- `Ctrl+G` `d` - Diff file
- `Ctrl+G` `m` - Commit with message
- `Ctrl+G` `p` - Push to remote
- `Ctrl+G` `f` - Fetch from remote
- `Ctrl+G` `l` - Pull from remote
- `Ctrl+G` `t` - Create and push tag

`Esc` cancels the prefix if you change your mind. Right-clicking a tree row
offers the same actions without it.

---

## Mouse

| Gesture | Action |
|---------|--------|
| Click | Position the cursor. With the file tree open this also closes it and takes focus, since the tree owns the keyboard while it is up |
| Drag | Select text. Left button only — a right- or middle-drag does nothing |
| Drag the terminal's top edge | Resize the panel. The separator bar carries a `⇕` marker to say it can be grabbed |
| `Alt+Click` | Add or remove a cursor |
| Right-click | Context menu at the pointer. Any modifier held still counts |
| `Ctrl+Click` | The same menu, for a one-button pointer |
| `Shift+F10` or `Alt+Z` | The same menu at the caret |
| Wheel | Scrolls whatever is under the pointer: the pane, an inactive pane, or the terminal panel's scrollback. Over the tab bar or status bar it does nothing |
| Click a tab | Switch to it |
| Click a tree row | Open a file, or expand a directory |
| Right-click a tree row | Open in a split, or stage / unstage / diff |
| Click the `»` / `«` chevron | Toggle the file tree. Bottom-left corner; it points the way the tree will move |

Right-clicking **inside a selection** keeps that selection, so Cut and Copy act
on it. Right-clicking anywhere else moves the caret there first, as most
editors do.

The context menu greys out what cannot work rather than hiding it: Go to
Definition and Find References are dim without a language server, and a tree
row's git actions are dim outside a repository or when the file's status makes
them meaningless. Cut and Copy are never dim — with no selection they act on
the whole line, and the label says so.

`F10` is frequently claimed by the terminal or desktop for its own menubar,
which is why `Alt+Z` exists. `python3 tools/keycap.py` shows what your terminal
actually delivers.

---

For detailed feature explanations, see [LSP_GUIDE.md](LSP_GUIDE.md)

---

## Doing things

**Reading unfamiliar code.** `Alt+P` finds any symbol in the project, `Alt+G`
jumps to where it is defined, `Alt+R` shows everywhere it is used, and `Alt+,`
walks back out along the trail you came in on.

**Working through errors.** `Alt+E` lists every diagnostic in the file;
`Enter` on one jumps there; `Alt+.` offers whatever fixes the server has.

**Renaming something.** `Alt+R` first, to see what you are about to change.
`Alt+N` then renames it across the project in one edit, which the undo stack
treats as one step.

**Several files at once.** `Ctrl+B` for the tree, `Enter` to open, `Alt+V` or
`Alt+S` to split, `Alt+H/J/K/L` between panes. For more than a handful, open
the directory as a tab group instead — `Enter` on it in the tree — and the
whole set travels as one tab-bar entry.

---

## Tips

### When a key does nothing

It is usually not the editor. A terminal emulator, a multiplexer or the desktop
can all take a chord before any program sees it, and none of them says so.
`python3 tools/keycap.py` prints what your terminal actually delivers, which
answers the question directly.

The usual suspects: F-keys claimed for media or menubars, `Ctrl+W` closing a
terminal tab, tmux eating `Ctrl+A` as its prefix, and window managers taking
`Ctrl+Alt+Arrow` and `Super+Arrow`.

Every F-key command has an `Alt` alternative for this reason — `Alt+G` for
`F12`, `Alt+E` for `F8`, `Alt+N` for `F2`, and so on down the LSP table. Where
two chords do the same thing in this document, the `Alt` one is the one that
works everywhere.

### Discovering commands
- `Ctrl+P` opens the command palette, which lists everything the editor can do
- `Ctrl+?` or `F1` opens the help modal

### Vim-flavoured keys
- `Alt+H` / `Alt+J` / `Alt+K` / `Alt+L` move between panes
- `j` / `k` move the selection in panels (diagnostics, references, symbols)

In the FILE TREE `j` and `k` are search input, not motion — typing filters the
tree. Use the arrows there.

---
