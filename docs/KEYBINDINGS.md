# fac Keybindings Reference

Complete keyboard shortcut reference for `fac` editor.

---

## 🔍 LSP Features

| Keybinding | Command | Description |
|------------|---------|-------------|
| `F12` or `Ctrl+\` | Go to Definition | Jump to where a symbol is defined |
| `Shift+F12` or `Ctrl+Shift+R` | Find References | Find all usages of a symbol |
| `F2` | Rename Symbol | Rename symbol across entire project |
| `Ctrl+.` | Code Actions | Quick fixes and refactorings |
| `F8` or `Ctrl+Shift+D` | Diagnostics Panel | Show all errors and warnings |
| `F4` or `Ctrl+Shift+O` | Document Symbols | Navigate symbols in current file |
| `F6` or `Ctrl+Shift+T` | Workspace Symbols | Search symbols across all files |
| `Ctrl+P` | Command Palette | Search and execute any command |
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
| `Alt+Backspace` | Delete Word | Delete word backward |
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
| `Ctrl+W` | Close Tab | Close current tab |
| `Ctrl+Tab` | Next Tab | Switch to next tab |
| `F6ab` | Previous Tab | Switch to previous tab |
| `Alt+1` to `Alt+9` | Jump to Tab | Switch to specific tab number |
| `Ctrl+\` | Split Vertical | Split current pane vertically |
| `Ctrl+Shift+\` | Split Horizontal | Split current pane horizontally |
| `Ctrl+Shift+W` | Close Pane | Close current pane |
| `Ctrl+H` | Navigate Left | Move to pane on the left |
| `Ctrl+L` | Navigate Right | Move to pane on the right |
| `Ctrl+K` | Navigate Up | Move to pane above |
| `Ctrl+J` | Navigate Down | Move to pane below |

---

## 🧭 Navigation

| Keybinding | Command | Description |
|------------|---------|-------------|
| `↑` `↓` `←` `→` | Move Cursor | Move cursor one character/line |
| `Ctrl+↑` | Move Line Up | Move current line up |
| `Ctrl+↓` | Move Line Down | Move current line down |
| `Home` or `Ctrl+A` | Line Start | Jump to beginning of line |
| `End` or `Ctrl+E` | Line End | Jump to end of line |
| `Ctrl+Home` | File Start | Jump to beginning of file |
| `Ctrl+End` | File End | Jump to end of file |
| `Page Up` | Scroll Up | Move up one screen |
| `Page Down` | Scroll Down | Move down one screen |
| `Ctrl+U` | Half Page Up | Scroll up half screen (vim-style) |
| `Ctrl+B` | Full Page Up | Scroll up full screen (vim-style) |
| `Ctrl+D` | Half Page Down | Scroll down half screen (vim-style) |
| `Ctrl+F` | Full Page Down | Scroll down full screen (vim-style) |
| `%` | Match Bracket | Jump to matching bracket/paren |

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
| `Ctrl+?` or `F1` | Help | Show help screen |
| `Ctrl+Shift+F` | File Tree | Toggle file explorer (Fortress mode) |
| `Esc` | Cancel/Close | Close panels, cancel operations |

---

## 📋 Clipboard & Yank Stack

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+X` | Cut | Cut selection and add to yank stack |
| `Ctrl+C` | Copy | Copy selection to clipboard |
| `Ctrl+V` | Paste | Paste from clipboard |
| `Ctrl+Shift+V` | Yank Cycle | Cycle through clipboard history |

---

## 🌲 File Tree (Fortress Mode)

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+Shift+F` | Toggle Tree | Show/hide file tree |
| **In Tree:** | | |
| `↑` `↓` or `j` `k` | Navigate | Move selection up/down |
| `Enter` or `l` | Open/Expand | Open file or expand directory |
| `h` | Collapse | Collapse current directory |
| `Space` | Toggle Expand | Expand/collapse directory |
| `/` | Search Files | Filter files by name |
| `g` | Go to Top | Jump to first item |
| `G` | Go to Bottom | Jump to last item |
| `Esc` | Close Tree | Hide file tree |

---

## 🎯 Multiple Cursors

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Ctrl+D` | Add Next Match | Add cursor at next match of selection |
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
Active when `Ctrl+F` is pressed:
- `n` / `Ctrl+N` - Next match
- `N` / `Ctrl+P` - Previous match
- `r` - Replace current
- `a` - Replace all
- `Ctrl+R` - Toggle regex
- `Ctrl+C` - Toggle case sensitive
- `Ctrl+W` - Toggle whole word
- `Alt+S` - Search in selection
- `↑` / `↓` - Navigate search history
- `Esc` - Exit search mode

### Fortress (File Tree) Mode
Active when `Ctrl+Shift+F` is pressed:
- `j` / `k` or `↑` / `↓` - Navigate
- `Enter` or `l` - Open/Expand
- `h` - Collapse
- `/` - Filter files
- `Esc` - Exit Fortress mode

---

## 💡 Tips

### Keybinding Conflicts
- Some keybindings may conflict with terminal emulator shortcuts
- If a key doesn't work, check your terminal's keyboard settings
- Common conflicts: `F6` (new terminal tab), `Ctrl+W` (close terminal tab)

### Customization
- Keybindings are currently hardcoded
- Future versions will support custom keybindings

### Discovering Commands
- Use `Ctrl+P` (Command Palette) to see all available commands
- Commands show their keybindings in the palette

### Vim Users
Some vim-style keybindings work:
- `j` / `k` - Up/down in panels
- `g` / `G` - Top/bottom in file tree
- `h` / `l` - Collapse/expand in file tree
- `Ctrl+U` / `Ctrl+D` - Half-page scroll
- `Ctrl+B` / `Ctrl+F` - Full-page scroll
- `%` - Match bracket

---

## 🚀 Most Useful Combos

**Exploring code:**
1. `F6` - Find any symbol in project
2. `F12` - Jump to definition
3. `Shift+F12` - See all usages
4. `Alt+,` - Jump back

**Fixing errors:**
1. `F8` - See all errors
2. Navigate to error
3. `Ctrl+.` - Apply quick fix
4. `Ctrl+S` - Save

**Refactoring:**
1. `Shift+F12` - See all references
2. `F2` - Rename everywhere
3. `Shift+Alt+F` - Format code
4. `Ctrl+S` - Save

**Working with multiple files:**
1. `Ctrl+Shift+F` - Browse file tree
2. `Enter` - Open in new tab
3. `Ctrl+Tab` - Switch between tabs
4. `Ctrl+\` - Split panes
5. `Ctrl+H/J/K/L` - Navigate panes

---

For detailed feature explanations, see [LSP_GUIDE.md](LSP_GUIDE.md)
