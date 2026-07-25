# fac Keybindings Reference

Complete keyboard shortcut reference for `fac` editor.

---

## 🔍 LSP Features

| Keybinding | Command | Description |
|------------|---------|-------------|
| `F12` or `Ctrl+\` or `Alt+G` | Go to Definition | Jump to where a symbol is defined |
| `Shift+F12` or `Alt+R` | Find References | Find all usages of a symbol |
| `F2` or `Alt+N` | Rename Symbol | Rename symbol across entire project |
| `F10` or `Alt+.` | Code Actions | Quick fixes and refactorings |
| `F8` or `Alt+E` | Diagnostics Panel | Show all errors and warnings |
| `F4` or `Alt+O` | Document Symbols | Navigate symbols in current file |
| `F6` or `Alt+P` | Workspace Symbols | Search symbols across all files |
| `Ctrl+P` | Command Palette | Search and execute any command |
| `F5` or `Alt+T` | Integrated Terminal | Toggle the terminal panel |
| `Shift+Alt+F` | Format Document | Auto-format current file |
| `Alt+,` | Jump Back | Return to previous location (jump stack) |

---

## 📁 File Operations

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+S` | Save | Save current file |
| `Ctrl+Q` | Quit | Exit editor (prompts if unsaved) |
| `Ctrl+O` | Open File | Open file browser (Fortress mode) |
| `Ctrl+N` | New File | Create new untitled buffer |
| `Ctrl+T` | New Tab | Create new empty tab |

---

## 📝 Editing

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+D` | Select Next Match | Add cursor at next occurrence of selection |
| `Ctrl+Z` | Undo | Undo last change |
| `Ctrl+Y` | Redo | Redo last undone change |
| `Ctrl+X` | Cut | Cut selection to clipboard |
| `Ctrl+C` | Copy | Copy selection to clipboard |
| `Ctrl+V` | Paste | Paste from clipboard |
| `Ctrl+A` | Select All | Select entire file |
| `Tab` | Accept Suggestion | Accept the whole inline suggestion |
| `Ctrl+Right` | Accept One Word | Accept a single word of an inline suggestion, keeping the rest offered |
| `Alt+Right` | Accept One Line | Accept one line of a multi-line suggestion, keeping the rest offered |
| `Alt+I` / `Alt+Shift+I` | Toggle AI Completion | Turn inline completion on or off. Off is instant; on probes the backend and reports what it found |
| `Alt+\` | Deep Completion | Request a large block from the deep model (see docs/AI_COMPLETION.md) |
| `Ctrl+/` | Toggle Line Comment | Comment/uncomment the cursor's lines or the selection |
| `Ctrl+Shift+K` | Delete Line | Delete the cursor's lines outright — nothing is copied or yanked (needs a terminal supporting the kitty keyboard protocol; otherwise use the command palette) |
| `Alt+Backspace` | Delete Word | Delete word backward; at column 1 it deletes the line break, so blank lines are eaten one press at a time |
| `Backspace` | Delete Char | Delete character backward |
| `Delete` | Delete Forward | Delete character forward |

---

## 🔎 Search & Replace

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+F` | Search | Open unified search/replace prompt |
| `Ctrl+H` | Replace | Open unified search/replace (same as Ctrl+F) |
| `Ctrl+G` | Go to Line | Jump to specific line number |
| **In Search Mode:** | | |
| `n` or `Ctrl+N` | Next Match | Jump to next search result |
| `N` or `Ctrl+P` | Previous Match | Jump to previous search result |
| `r` | Replace One | Replace current match |
| `a` | Replace All | Replace all matches |
| `Ctrl+R` | Toggle Regex | Enable/disable regex search |
| `Ctrl+C` | Toggle Case | Toggle case-sensitive search |
| `Ctrl+W` | Toggle Whole Word | Toggle whole-word matching |
| `Alt+S` | Search in Selection | Limit search to selected text |
| `↑` / `↓` | History | Navigate search history |
| `Esc` | Exit Search | Close search prompt |

---

## 🗂️ Tabs & Windows

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+T` | New Tab | Create new empty tab |
| `Ctrl+W` | Close Tab/Pane | Close current pane (then tab if last pane) |
| `Ctrl+PageDown` or `Ctrl+Alt+Right` | Next Tab | Switch to next tab |
| `Ctrl+PageUp` or `Ctrl+Alt+Left` | Previous Tab | Switch to previous tab |
| `Alt+1` to `Alt+9` (or `Ctrl+1` to `Ctrl+9`) | Jump to Tab | Switch to specific tab number |
| `Alt+V` | Split Vertical | Split current pane vertically |
| `Alt+S` | Split Horizontal | Split current pane horizontally |
| `Alt+Q` | Close Pane | Close current pane only |
| `Alt+H` or `Ctrl+Shift+Left` | Navigate Left | Move to pane on the left |
| `Alt+L` or `Ctrl+Shift+Right` | Navigate Right | Move to pane on the right |
| `Alt+K` or `Ctrl+Shift+Up` | Navigate Up | Move to pane above |
| `Alt+J` or `Ctrl+Shift+Down` | Navigate Down | Move to pane below |

---

## 🧭 Navigation

| Keybinding | Command | Description |
|------------|---------|-------------|
| `↑` `↓` `←` `→` | Move Cursor | Move cursor one character/line |
| `Alt+↑` | Move Line Up | Move current line up |
| `Alt+↓` | Move Line Down | Move current line down |
| `Home` or `Ctrl+A` | Line Start | Jump to beginning of line (smart toggle) |
| `End` or `Ctrl+E` | Line End | Jump to end of line |
| `Ctrl+Home` | File Start | Jump to beginning of file |
| `Ctrl+End` | File End | Jump to end of file |
| `Alt+←` / `Alt+→` | Word Jump | Move cursor by word |
| `Page Up` / `Page Down` | Page Scroll | Move up/down one screen |
| `Alt+[` / `Alt+]` | Match Bracket | Jump to matching bracket/paren |

---

## ✂️ Selection

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Shift+↑↓←→` | Select | Extend selection with arrow keys |
| `Ctrl+Shift+Home` | Select to Start | Select from cursor to file start |
| `Ctrl+Shift+End` | Select to End | Select from cursor to file end |
| `Ctrl+A` | Select All | Select entire file |
| `Ctrl+D` | Select Next | Add cursor at next match of selection |
| `Esc` | Clear Selection | Deselect and return to single cursor |

---

## 🎨 Command & Utility

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+P` | Command Palette | Search and execute any command |
| `F5` or `Alt+T` | Integrated Terminal | Toggle the terminal panel |
| `Ctrl+?` or `F1` | Help | Show help screen (`Ctrl+?` needs a terminal that supports the kitty keyboard protocol; `F1` always works) |
| `Ctrl+B` or `F3` | File Tree | Toggle file explorer (Fuss mode) |
| `Ctrl+L` | Redraw Screen | Clear and redraw the screen |
| `Esc` | Cancel/Close | Close panels, cancel operations. In the terminal panel it closes the panel only from a bare shell prompt — with text on the line, or inside a full-screen program, the shell gets it |

---

## 📋 Clipboard & Yank Stack

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+X` | Cut | Cut selection and add to yank stack |
| `Ctrl+C` | Copy | Copy selection to clipboard |
| `Ctrl+V` | Paste | Paste from clipboard |
| `Ctrl+Shift+V` | Yank Cycle | Cycle through clipboard history |

---

## 🌲 File Tree (Fuss Mode)

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+B` or `F3` | Toggle Tree | Show/hide file tree |
| **In Tree:** | | |
| `j` `k` or `↑` `↓` | Navigate | Move to previous/next sibling |
| `→` / `←` | Enter/Exit | Enter directory or exit to parent |
| `Enter` or `o` | Open | Open file in editor |
| `Space` | Toggle Expand | Expand/collapse directory |
| `?` | Show Hints | Display fuss mode keybindings |
| `Esc` | Close Tree | Hide file tree |

---

## 🎯 Multiple Cursors

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+D` | Select Word & Next | Select word under cursor, add cursor at next match |
| `Alt+Click` | Add/Remove Cursor | Add or remove cursor at mouse position |
| `Ctrl+Alt+↑` | Add Cursor Above | Add cursor on line above |
| `Ctrl+Alt+↓` | Add Cursor Below | Add cursor on line below |
| `Esc` | Single Cursor | Return to single cursor mode |
| *(while multiple cursors active)* | | |
| Type normally | Edit All | Type at all cursor positions |
| Arrow keys | Move All | Move all cursors together |
| `Backspace`/`Delete` | Delete All | Delete at all cursors |

---

## 📦 Panel Navigation

When any panel is open (Diagnostics, References, Symbols, etc.):

| Keybinding | Command | Description |
|------------|---------|-------------|
| `↑` `↓` or `j` `k` | Navigate Items | Move selection up/down |
| `Enter` | Select/Jump | Jump to selected item |
| `Esc` | Close Panel | Close the panel |
| Type characters | Filter/Search | Narrow down results (in symbols panels) |

---

## ⚙️ Special Modes

### Search/Replace Mode
Active when `Ctrl+F` (search) or `Ctrl+R` (replace) is pressed:
- `n` - Next match
- `N` - Previous match
- `Alt+C` - Toggle case sensitive
- `Alt+W` - Toggle whole word match
- `↑` / `↓` - Navigate search history
- `Esc` - Exit search mode

### Fuss (File Tree) Mode
Active when `Ctrl+B` or `F3` is pressed:
- `j` / `k` - Move to previous/next sibling
- `→` / `←` - Enter directory / exit to parent
- `Enter` or `o` - Open file
- `Space` - Toggle expand/collapse
- `?` - Show hints
- `Esc` - Exit Fuss mode

**Git operations in Fuss mode:**
- `a` - Stage file
- `u` - Unstage file
- `d` - Diff file
- `m` - Commit with message
- `p` - Push to remote
- `f` - Fetch from remote
- `l` - Pull from remote
- `t` - Create and push tag

---

## 💡 Tips

### Keybinding Conflicts
- Some keybindings may conflict with terminal emulator shortcuts
- If a key doesn't work, check your terminal's keyboard settings
- Common conflicts: F-keys (media controls), `Ctrl+W` (close terminal tab)
- All LSP features have Alt+key alternatives that work better in terminals

### Terminal Compatibility
Most terminal emulators don't pass Ctrl+Shift combinations reliably. That's why `fac` uses Alt+key alternatives:
- `Alt+E` instead of Ctrl+Shift+D for diagnostics
- `Alt+O` instead of Ctrl+Shift+O for document symbols
- `Alt+P` instead of Ctrl+Shift+T for workspace symbols
- `Alt+R` instead of Ctrl+Shift+R for references

### Discovering Commands
- Use `Ctrl+P` (Command Palette) to see all available commands
- Press `Ctrl+?` or `F1` to see the help screen

### Vim Users
Some vim-style keybindings work:
- `j` / `k` - Up/down in panels and file tree
- `Alt+H/J/K/L` - Navigate between panes
- `o` - Open file in file tree
- Navigation in Fuss mode is sibling-based like vim's file explorers

---

## 🚀 Most Useful Combos

**Exploring code:**
1. `F6` or `Alt+P` - Find any symbol in project
2. `F12` or `Alt+G` - Jump to definition
3. `Shift+F12` or `Alt+R` - See all usages
4. `Alt+,` - Jump back

**Fixing errors:**
1. `F8` or `Alt+E` - See all errors
2. Navigate to error line
3. `F10` or `Alt+.` - Apply quick fix
4. `Ctrl+S` - Save

**Refactoring:**
1. `Shift+F12` or `Alt+R` - See all references
2. `F2` or `Alt+N` - Rename everywhere
3. `Shift+Alt+F` - Format code
4. `Ctrl+S` - Save

**Working with multiple files:**
1. `Ctrl+B` or `F3` - Browse file tree
2. `Enter` - Open in new tab
3. `Ctrl+PageDown/Up` - Switch between tabs
4. `Alt+V` / `Alt+S` - Split panes vertical/horizontal
5. `Alt+H/J/K/L` - Navigate panes

---

For detailed feature explanations, see [LSP_GUIDE.md](LSP_GUIDE.md)
