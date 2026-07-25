# Language Server Protocol (LSP) Guide

## What is LSP?

**Language Server Protocol (LSP)** is like having a smart assistant that understands your programming language. It provides IDE-like features such as:

- **Error detection** as you type
- **Jump to definition** of functions/variables
- **Find all references** to a symbol
- **Auto-completion** suggestions
- **Rename** symbols across your entire project
- **Code formatting** to keep style consistent
- **Quick fixes** for common errors

Instead of building these features separately for each language, LSP uses a standardized protocol. This means `fac` can support Python, JavaScript, Fortran, Rust, and 100+ other languages using the same system!

## How Does It Work?

```
┌─────────────┐          LSP Protocol          ┌──────────────────┐
│             │ ◄────────────────────────────► │  Language Server │
│  fac Editor │   (JSON messages over stdio)   │  (e.g., pylsp)   │
│             │                                 │                  │
└─────────────┘                                 └──────────────────┘
     │                                                    │
     │ You edit code                                     │ Analyzes code
     │ Send changes                                      │ Finds errors
     │ Request features                                  │ Provides completions
```

When you open a file in `fac`:
1. `fac` starts the appropriate language server (e.g., `pylsp` for Python)
2. `fac` tells the server what file you're editing
3. As you type, `fac` sends your changes to the server
4. The server analyzes your code and sends back diagnostics (errors/warnings)
5. When you press F12, `fac` asks the server "where is this defined?"
6. The server responds with the location, and `fac` jumps there

## Quick Start

### Step 1: Install a Language Server

Language servers are separate programs you install once. Here are the most common ones:

**Python:**
```bash
pip install python-lsp-server
```

**JavaScript/TypeScript:**
```bash
npm install -g typescript-language-server typescript
```

**Rust:**
```bash
rustup component add rust-analyzer
```

**Fortran:**
```bash
pip install fortls
```

**Go:**
```bash
go install golang.org/x/tools/gopls@latest
```

**C/C++:**
```bash
# On macOS with Homebrew
brew install llvm
# clangd comes with LLVM

# On Linux
sudo apt install clangd
```

See the **Language Server Setup** section below for detailed instructions.

### Step 2: Open a File

Just open `fac` with a source code file:

```bash
fac my_script.py
```

That's it! If you have `pylsp` installed, `fac` will automatically:
- Start the language server
- Begin analyzing your code
- Show errors/warnings with red/yellow markers in the gutter
- Enable all LSP features

### Step 3: Try It Out

1. **See errors**: Look for `E` (error) or `W` (warning) in the left gutter
2. **View all diagnostics**: Press `F8` to open the diagnostics panel
3. **Jump to definition**: Put cursor on a function name and press `F12`
4. **Find references**: Press `Shift+F12` to see everywhere a symbol is used
5. **Rename**: Press `F2` (or `Alt+N`) to rename a variable across all files
6. **Format code**: Press `Shift+Alt+F` to auto-format

---

## LSP Features Reference

### 1. Diagnostics (Errors & Warnings)

**What it does:** Shows syntax errors, type errors, and warnings as you code.

**How to use:**
- Errors appear with `E` in the gutter (red)
- Warnings appear with `W` in the gutter (yellow)
- Put your cursor on a line with an error to see the message in the status bar
- Press `F8` to open the **Diagnostics Panel** showing all issues

**Example:**
```python
def greet(name):
    print("Hello, " + nme)  # Error: 'nme' is not defined
    ^^^^^^^^^^^^^^^^^^^^
    E: Undefined name 'nme'
```

**Keybinding:**
- `F8` or `Alt+E` - Open/close diagnostics panel
- `j`/`k` or `↑`/`↓` - Navigate issues in panel
- `Enter` - Jump to selected issue
- `Esc` - Close panel

---

### 2. Go to Definition (F12)

**What it does:** Jump to where a function, variable, or class is defined.

**How to use:**
1. Put your cursor on a symbol (function name, variable, class, etc.)
2. Press `F12`
3. `fac` jumps to the definition (opens file in new tab if needed)
4. Press `Alt+,` to jump back to where you were

**Example:**
```python
# In main.py
result = calculate_total(items)
         ^^^^^^^^^^^^^^^
         Put cursor here, press F12

# Jumps to utils.py:
def calculate_total(items):  # ← You land here
    return sum(item.price for item in items)
```

**Keybindings:**
- `F12` or `Ctrl+\` or `Alt+G` - Go to definition
- `Alt+,` - Jump back (navigate backward in jump history)

**Cross-file navigation:** If the definition is in another file, `fac` automatically opens it in a new tab!

---

### 3. Find References (Shift+F12)

**What it does:** Find all places where a symbol is used across your entire project.

**How to use:**
1. Put cursor on a symbol
2. Press `Shift+F12`
3. Browse references in the panel
4. Press `Enter` to jump to a reference

**Example:**
```python
# You want to find everywhere `calculate_total` is called
def calculate_total(items):
    ...

# Press Shift+F12 shows:
References to 'calculate_total':
  main.py:15:12  - result = calculate_total(items)
  test.py:23:8   - assert calculate_total([]) == 0
  utils.py:45:3  - total = calculate_total(cart.items)
```

**Keybindings:**
- `Shift+F12` or `Alt+R` - Find all references
- `j`/`k` or `↑`/`↓` - Navigate references
- `Enter` - Jump to selected reference
- `Esc` - Close panel

---

### 4. Code Actions & Quick Fixes (Alt+.)

**What it does:** Get quick fixes for errors and refactoring suggestions.

**How to use:**
1. Put cursor on a line with a diagnostic (error/warning)
2. Press `F10` or `Alt+.`
3. Select an action from the menu
4. Press `Enter` to apply it

**Example:**
```python
# Error: Missing import
result = json.loads(data)
         ^^^^
         Error: 'json' is not defined

# Press Ctrl+. shows:
Code Actions:
  → Import 'json'
    Ignore undefined name 'json'

# Select "Import 'json'" and fac adds:
import json  # ← Automatically added!
```

**Common actions:**
- Add missing imports
- Fix spelling errors in variable names
- Extract method/function
- Organize imports
- Add type hints

**Keybindings:**
- `F10` or `Alt+.` - Open code actions menu
- `j`/`k` or `↑`/`↓` - Navigate actions
- `1`-`9` - Quick select action by number
- `Enter` - Apply selected action
- `Esc` - Close menu

**Note:** Code actions are context-sensitive. On a line without diagnostics, you'll only see global actions like "Organize imports" and "Fix all". Position cursor on a line with an error/warning to see specific fixes.

---

### 5. Rename Symbol (F2 / Alt+N)

**What it does:** Rename a variable, function, or class everywhere it's used.

**How to use:**
1. Put cursor on a symbol
2. Press `F2` (or `Alt+N`)
3. Type the new name
4. Press `Enter`
5. The symbol is renamed everywhere across all files!

**Example:**
```python
# Before:
def calc(x, y):
    return x + y

result = calc(5, 3)
total = calc(10, 20)

# Put cursor on 'calc', press F2 (or Alt+N), type 'calculate'
# After:
def calculate(x, y):
    return x + y

result = calculate(5, 3)
total = calculate(10, 20)
```

**Safe renaming:** The language server understands your code's structure, so it won't accidentally rename:
- String contents: `"call calc function"` stays unchanged
- Comments: `# calc is fast` stays unchanged
- Unrelated variables with the same name in different scopes

**Keybinding:**
- `F2` / `Alt+N` - Rename symbol
- Type new name, press `Enter` to confirm
- Press `Esc` to cancel

---

### 6. Document Symbols Outline (F4)

**What it does:** See an outline of all functions, classes, and variables in the current file.

**How to use:**
1. Press `F4` to open the symbols panel
2. Type to filter symbols (fuzzy search)
3. Press `Enter` to jump to a symbol

**Example:**
```
Document Symbols:
  [Class] User
    [Method] __init__
    [Method] validate
    [Property] full_name
  [Function] create_user
  [Function] delete_user
  [Variable] DEFAULT_TIMEOUT
```

**Keybindings:**
- `F4` or `Alt+O` - Open document symbols panel
- Type to search (fuzzy matching)
- `j`/`k` or `↑`/`↓` - Navigate symbols
- `Enter` - Jump to selected symbol
- `Esc` - Close panel

---

### 7. Workspace Symbols (F6)

**What it does:** Search for any symbol across your **entire project** (all files).

**How to use:**
1. Press `F6` to open workspace symbols
2. Type part of a symbol name (fuzzy search)
3. Navigate and press `Enter` to jump to it

**Example:**
```
# Type "calc"
Workspace Symbols:
  [Function] calculate_total      in utils.py
  [Function] calculator_init      in calculator.py
  [Class] Calculator              in calculator.py
  [Method] recalculate            in models.py
  [Variable] CALC_PRECISION       in config.py
```

**Fuzzy search:** You don't need to type the exact name:
- `calc` matches `calculate_total`
- `ct` matches `calculate_total` (first letters)
- `calctot` matches `calculate_total` (consecutive)

**Keybindings:**
- `F6` or `Alt+P` - Open workspace symbols
- Type to search across all files
- `j`/`k` or `↑`/`↓` - Navigate results
- `Enter` - Jump to symbol (opens file if needed)
- `Esc` - Close panel

**Pro tip:** This is incredibly fast for navigating large codebases!

---

### 8. Signature Help

**What it does:** Shows function parameters and types as you're typing a function call.

**How to use:**
Just start typing a function call with `(`, and a tooltip appears:

**Example:**
```python
# You type:
result = calculate_total(
                        ^
                        Cursor here

# Tooltip shows:
calculate_total(items: list[Item], tax_rate: float = 0.0) -> float
                ^^^^^
                Current parameter highlighted
```

**Keybindings:**
- Appears automatically when you type `(`
- Type `,` to move to next parameter
- Auto-dismisses when you type `)`

**Works with:**
- Regular functions
- Class constructors
- Methods
- Overloaded functions (shows all signatures)

---

### 9. Document Formatting (Shift+Alt+F)

**What it does:** Automatically formats your code according to style guidelines.

**How to use:**
1. Press `Shift+Alt+F`
2. Your code is instantly formatted

**Example:**
```python
# Before:
def greet(  name,age  ):
  if age>18:
        print(  "Hello, "+name )
  else:print("Hi, "+name)

# After (Shift+Alt+F):
def greet(name, age):
    if age > 18:
        print("Hello, " + name)
    else:
        print("Hi, " + name)
```

**Respects:**
- `.editorconfig` settings
- Language-specific style guides (PEP 8 for Python, etc.)
- Project configuration files (`.pylintrc`, `pyproject.toml`, etc.)

**Keybinding:**
- `Shift+Alt+F` - Format entire document

**Formatters by language:**
- Python: `black`, `autopep8`, or `yapf` (configured in `pylsp`)
- JavaScript: `prettier`
- Rust: `rustfmt`
- Go: `gofmt`

---

### 10. Command Palette (Ctrl+P)

**What it does:** Quick access to all LSP commands without remembering keybindings.

**How to use:**
1. Press `Ctrl+P`
2. Type to search commands (fuzzy search)
3. Press `Enter` to execute

**Available commands:**
```
Command Palette:
  [LSP] Go to Definition
  [LSP] Find References
  [LSP] Rename Symbol
  [LSP] Code Actions
  [LSP] Document Symbols
  [LSP] Workspace Symbols
  [LSP] Format Document
  [File] Save File
  [Navigate] Jump Back
  ... and 40+ more!
```

**Keybindings:**
- `Ctrl+P` - Open command palette
- Type to search commands
- `Enter` - Execute selected command
- `Esc` - Close palette

---

## Language Server Setup

### Python (pylsp)

**Install:**
```bash
pip install python-lsp-server

# Optional plugins for more features:
pip install python-lsp-black      # Black formatting
pip install pylsp-mypy            # Type checking
pip install python-lsp-ruff       # Fast linting
```

**What you get:**
- Error detection (syntax, undefined variables)
- Auto-completion
- Go to definition / Find references
- Rename symbol
- Code formatting (with black)
- Type checking (with mypy)

**Configuration:**
Create `~/.config/pylsp/config.json`:
```json
{
  "plugins": {
    "black": {"enabled": true},
    "mypy": {"enabled": true}
  }
}
```

---

### JavaScript/TypeScript (typescript-language-server)

**Install:**
```bash
npm install -g typescript-language-server typescript
```

**What you get:**
- Error detection (TypeScript type errors)
- Auto-completion with type information
- Refactoring (extract function, rename, etc.)
- Import management
- JSDoc support

**Works with:**
- JavaScript (`.js`)
- TypeScript (`.ts`, `.tsx`)
- JSX/React files

---

### Rust (rust-analyzer)

**Install:**
```bash
rustup component add rust-analyzer
```

**What you get:**
- Instant error detection
- Powerful type inference
- Macro expansion
- Cargo integration
- Inline type hints

**Note:** Works best with `Cargo.toml` in your project root.

---

### Fortran (fortls)

**Install:**
```bash
pip install fortls
```

**What you get:**
- Syntax error detection
- Module/subroutine navigation
- Variable completion
- Signature help
- Hover documentation

**Configuration:**
Create `.fortls` in your project root:
```json
{
  "lowercase_intrinsics": true,
  "hover_signature": true,
  "use_signature_help": true
}
```

---

### Go (gopls)

**Install:**
```bash
go install golang.org/x/tools/gopls@latest
```

**What you get:**
- Error detection
- Auto-imports
- Refactoring tools
- Test integration
- Fast navigation

---

### C/C++ (clangd)

**Install:**
```bash
# macOS
brew install llvm

# Ubuntu/Debian
sudo apt install clangd

# Arch
sudo pacman -S clang
```

**What you get:**
- Real-time error checking
- Include path completion
- Cross-reference navigation
- Refactoring support

**Configuration:**
Create `compile_commands.json` in project root:
```bash
# With CMake:
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .

# With Make:
bear -- make
```

---

## Troubleshooting

### "No language server found"

**Problem:** `fac` can't find the language server executable.

**Solution:**
1. Make sure the language server is installed:
   ```bash
   which pylsp          # For Python
   which rust-analyzer  # For Rust
   which gopls          # For Go
   ```

2. Make sure the executable is in your `PATH`:
   ```bash
   echo $PATH
   ```

3. Try running the language server manually:
   ```bash
   pylsp --help
   ```

---

### "LSP features not working"

**Problem:** File opens but no diagnostics/features appear.

**Solution:**
1. Check if the server started:
   - Look in `fac`'s logs (if available)
   - The server might have crashed on startup

2. Check file extension matches language:
   - `.py` → Python
   - `.rs` → Rust
   - `.f90`, `.f95` → Fortran

3. Make sure the file is in a proper project structure:
   - Python: Have a `setup.py` or `pyproject.toml`?
   - Rust: Have a `Cargo.toml`?
   - Some servers need project metadata

---

### "Diagnostics are slow/delayed"

**Problem:** Errors appear several seconds after typing.

**Explanation:** This is normal! Language servers analyze your code:
- Simple syntax errors: ~100ms
- Type checking: 500ms-2s
- Full project analysis: 2-10s

**Tips:**
- `fac` debounces changes (waits 500ms after you stop typing)
- Large projects take longer
- First-time analysis is slower (builds cache)

---

### "Server keeps crashing"

**Problem:** LSP features work then suddenly stop.

**Solution:**
1. Check server version:
   ```bash
   pylsp --version
   ```

2. Update the language server:
   ```bash
   pip install --upgrade python-lsp-server
   ```

3. Check for conflicting plugins/extensions

4. Restart `fac`

---

### "Formatting changes my code unexpectedly"

**Problem:** `Shift+Alt+F` makes unwanted changes.

**Solution:**
1. Check your formatter configuration
2. Many formatters respect configuration files:
   - Python: `pyproject.toml` or `.pylintrc`
   - JavaScript: `.prettierrc`
   - Rust: `rustfmt.toml`

3. Configure line length, indentation, etc. in those files

---

## Tips & Tricks

### 1. Use Jump Stack for Navigation

After using F12 to jump to a definition:
- Press `Alt+,` to jump back
- Works across multiple jumps (maintains history)
- Great for exploring unfamiliar codebases

### 2. Combine Search Features

- `Ctrl+F` - Search text in current file
- `F4` - Search symbols in current file
- `F6` - Search symbols across project
- `Shift+F12` - Find all usages of current symbol

### 3. Fix Errors Faster

When you see an error:
1. Press `F8` to see all errors
2. Navigate to each error
3. Press `Ctrl+.` to see quick fixes
4. Apply fixes with `Enter`

### 4. Rename Safely

Before renaming:
- Press `Shift+F12` to see all references
- Verify it's safe to rename
- Press `F2` (or `Alt+N`) to rename everywhere at once

### 5. Format on Save

Many language servers support format-on-save:
- Press `Ctrl+S` to save
- File is automatically formatted
- Keeps your code style consistent

---

## LSP Keybindings Quick Reference

| Feature | Keybinding | What It Does |
|---------|-----------|--------------|
| **Diagnostics Panel** | `F8` or `Alt+E` | Show all errors/warnings |
| **Go to Definition** | `F12` or `Ctrl+\` or `Alt+G` | Jump to where symbol is defined |
| **Find References** | `Shift+F12` or `Alt+R` | Find all usages of symbol |
| **Code Actions** | `F10` or `Alt+.` | Quick fixes and refactorings |
| **Rename Symbol** | `F2` or `Alt+N` | Rename across entire project |
| **Document Symbols** | `F4` or `Alt+O` | Outline of current file |
| **Workspace Symbols** | `F6` or `Alt+P` | Search symbols across project |
| **Format Document** | `Shift+Alt+F` | Auto-format code |
| **Command Palette** | `Ctrl+P` | Access all commands |
| **Jump Back** | `Alt+,` | Return to previous location |

**Note:** Alt+key combinations work better in terminals than Ctrl+Shift combinations.

---

## Supported Languages

`fac` can work with **any** language that has an LSP server. Here are popular ones:

| Language | Server | Installation |
|----------|--------|--------------|
| Python | `pylsp` | `pip install python-lsp-server` |
| JavaScript/TypeScript | `typescript-language-server` | `npm install -g typescript-language-server` |
| Rust | `rust-analyzer` | `rustup component add rust-analyzer` |
| Go | `gopls` | `go install golang.org/x/tools/gopls@latest` |
| C/C++ | `clangd` | `brew install llvm` or `apt install clangd` |
| Java | `jdtls` | Download from Eclipse |
| Ruby | `solargraph` | `gem install solargraph` |
| PHP | `intelephense` | `npm install -g intelephense` |
| Bash | `bash-language-server` | `npm install -g bash-language-server` |
| Fortran | `fortls` | `pip install fortls` |
| Lua | `lua-language-server` | Download from GitHub |
| HTML/CSS | `vscode-html-languageserver` | `npm install -g vscode-html-languageserver-bin` |

For more language servers, visit: https://microsoft.github.io/language-server-protocol/implementors/servers/

---

## What's Next?

Now that you understand LSP in `fac`:

1. **Install language servers** for your languages
2. **Practice the keybindings** - they'll become second nature
3. **Explore your codebase** with `F6` and `F12`
4. **Let LSP catch errors** before you run your code
5. **Refactor confidently** with `F2`/`Alt+N` and `Ctrl+.`

Welcome to IDE-level coding in the terminal! 🚀
