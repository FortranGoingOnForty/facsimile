# LSP Feature Test Files

These files are designed to test all LSP features in `fac`. Each file contains intentional errors and cross-references for comprehensive testing.

## Files Overview

### Python Files
- **calculator.py** - Math functions with intentional errors
  - Undefined variable errors (`reslt` instead of `result`)
  - Missing import errors (`math` module)
  - Multiple functions for testing navigation

- **main.py** - Main program importing calculator
  - Cross-file references for testing F12
  - Class structure for Document Symbols
  - Missing import error (`divide` function)

### Fortran Files
- **math_utils.f90** - Math utilities module
  - Vector operations (add, dot product, magnitude)
  - Matrix multiplication
  - Intentional errors (uninitialized variables)

- **test_program.f90** - Program using math_utils
  - Cross-file references to test F12
  - Undefined variable error

### C Files
- **utils.c** - Utility functions
  - Factorial, fibonacci, array operations
  - Intentional errors (undefined variables, type mismatches)

- **main.c** - Main program using utils
  - Cross-file function calls
  - Structure definition
  - Multiple intentional errors

## Testing LSP Features

### 1. Diagnostics (F8)

**Test:** Open any file and check for error markers in the gutter.

Expected errors:
- **calculator.py**: `reslt` undefined, `math` not imported
- **main.py**: `divide` not imported
- **math_utils.f90**: `undefined_var` in `broken_routine`
- **test_program.f90**: `undefined_result` not declared
- **utils.c**: `undefined_var`, type mismatch, missing return
- **main.c**: `undefined_function`, type mismatch, `undeclared_var`

**How to test:**
```bash
fac calculator.py
# Press F8 to open diagnostics panel
# Navigate with j/k or ↑/↓
# Press Enter to jump to error
```

### 2. Go to Definition (F12)

**Test:** Cross-file navigation between importing files.

**In main.py:**
1. Put cursor on `add` on line ~15
2. Press `F12`
3. Should jump to `def add()` in calculator.py

**In test_program.f90:**
1. Put cursor on `vector_add` on line ~23
2. Press `F12`
3. Should jump to `subroutine vector_add` in math_utils.f90

**In main.c:**
1. Put cursor on `factorial` on line ~28
2. Press `F12`
3. Should jump to `int factorial()` in utils.c

**Return:** Press `Alt+,` to jump back!

### 3. Find References (Shift+F12)

**Test:** Find all usages of a function.

**In calculator.py:**
1. Put cursor on `calculate_total` function name
2. Press `Shift+F12`
3. Should show:
   - Definition in calculator.py
   - Usage in main.py (imported and called)

**In math_utils.f90:**
1. Put cursor on `vector_dot`
2. Press `Shift+F12`
3. Should show usage in test_program.f90

### 4. Code Actions (Ctrl+.)

**Test:** Quick fixes for errors.

**In calculator.py line ~48:**
1. Put cursor on `reslt` (undefined error)
2. Press `Ctrl+.`
3. Should offer to fix the typo or define `reslt`

**In main.py line ~45:**
1. Put cursor on `divide` (not imported error)
2. Press `Ctrl+.`
3. Should offer to add `divide` to imports

### 5. Rename Symbol (F2)

**Test:** Rename across files.

**In calculator.py:**
1. Put cursor on `add` function name
2. Press `F2`
3. Type new name (e.g., `add_numbers`)
4. Press Enter
5. Check:
   - Function renamed in calculator.py
   - Import statement updated in main.py
   - Function call updated in main.py

**Undo:** Press `Ctrl+Z` repeatedly to revert

### 6. Document Symbols (F4)

**Test:** See file outline.

**In calculator.py:**
1. Press `F4`
2. Should see:
   - `add` function
   - `subtract` function
   - `multiply` function
   - `divide` function
   - etc.
3. Type "broken" to filter
4. Press Enter to jump to function

**In main.py:**
1. Press `F4`
2. Should see:
   - `DataProcessor` class
   - `process_data` function
   - `main` function

### 7. Workspace Symbols (F6)

**Test:** Search symbols across ALL files.

1. Press `F6`
2. Type "add"
3. Should see:
   - `add` function in calculator.py
   - `vector_add` in math_utils.f90
   - Maybe `sum_array` in utils.c (fuzzy match)
4. Navigate with j/k
5. Press Enter to jump (opens file if needed)

**Try different searches:**
- "calc" → finds `calculate_total`, `calculator`, etc.
- "vector" → finds all vector functions
- "fact" → finds `factorial`

### 8. Signature Help

**Test:** Parameter hints while typing.

**In any Python file:**
1. Type: `calculate_total(`
2. Should see tooltip: `calculate_total(items)`
3. As you type, current parameter is highlighted

**In Fortran:**
1. Type: `call vector_add(`
2. Should see signature with parameters

### 9. Document Formatting (Shift+Alt+F)

**Test:** Auto-format code.

**In calculator.py:**
1. Mess up the formatting (add extra spaces, wrong indentation)
2. Press `Shift+Alt+F`
3. Code should be auto-formatted to PEP 8 style

**In utils.c:**
1. Mess up braces and indentation
2. Press `Shift+Alt+F`
3. Code should be formatted with clang-format

### 10. Command Palette (Ctrl+P)

**Test:** Access commands without keybindings.

1. Press `Ctrl+P`
2. Type "format"
3. Select "Format Document"
4. Same as Shift+Alt+F

Or:
1. Press `Ctrl+P`
2. Type "def"
3. Select "Go to Definition"
4. Same as F12

## Language Server Setup

To test these files, you need language servers installed:

**Python:**
```bash
pip install python-lsp-server
```

**Fortran:**
```bash
pip install fortls
```

**C/C++:**
```bash
# macOS
brew install llvm

# Linux
sudo apt install clangd
```

## Expected Behavior

### With Language Server Installed:
- ✅ Red/yellow error markers in gutter
- ✅ F12 jumps to definitions (even across files)
- ✅ Shift+F12 shows all references
- ✅ Ctrl+. offers quick fixes
- ✅ F2 renames across files
- ✅ F4 shows file outline
- ✅ F6 searches all symbols
- ✅ Shift+Alt+F formats code

### Without Language Server:
- ❌ No error markers
- ❌ LSP features won't work
- ✅ Basic editing still works
- ✅ Syntax highlighting may work (if implemented separately)

## Known Issues to Test

1. **Syntax highlighting for Fortran** - Check if keywords are colored
2. **F8 vs Ctrl+D** - Check if modifier keys work correctly
3. **Cross-file tabs** - Check if F12 opens new tabs correctly
4. **Jump stack** - Check if Alt+, returns to previous location

## Quick Test Checklist

- [ ] Open calculator.py and see diagnostics (red/yellow markers)
- [ ] Press F8 and see error list
- [ ] F12 on `add` in main.py → jumps to calculator.py
- [ ] Alt+, → jumps back
- [ ] Shift+F12 on `calculate_total` → shows references
- [ ] F2 on `multiply` → rename and see it update in both files
- [ ] F4 → see file outline
- [ ] F6, type "vector" → find Fortran functions
- [ ] Ctrl+. on an error → see quick fixes
- [ ] Shift+Alt+F → format code
