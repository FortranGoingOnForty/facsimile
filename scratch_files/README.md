# Scratch Files Directory

This directory contains test files and examples used during development of the Facsimile editor.

## Regex Testing

- **`regex_test_examples.txt`** - Comprehensive test file with realistic examples for regex pattern matching
- **`regex_test_patterns.md`** - Documentation of regex patterns to test with the examples file

## Test Files

Various test files used during feature development:

- `ctrl_d_demo.txt` - Ctrl-D functionality testing
- `cursor_test.txt` - Cursor movement and positioning tests
- `demo.txt` - General demo file
- `help_test.txt` - Help menu testing
- `pane_nav_test.txt` - Pane navigation testing
- `pane_test.txt` - Multi-pane functionality tests
- `test_*.txt` - Various feature-specific test files

## Test Scripts

Shell scripts for automated testing:

- `test_cursor_fixes.sh` - Cursor behavior tests
- `test_help_menu.sh` - Help menu automated tests
- `test_new_pane_selection.sh` - Pane selection tests
- `test_pane_navigation.sh` - Pane navigation automation
- `test_panes.sh` - Multi-pane feature tests
- `test_shift_selection.sh` - Text selection tests

## Usage

### Testing Regex Functionality

1. Open the regex test file:
   ```bash
   ./fac scratch_files/regex_test_examples.txt
   ```

2. Press `Ctrl-F` to open unified search

3. Press `Alt-R` to toggle regex mode ON (indicator shows `[R]EGEX`)

4. Try patterns from `regex_test_patterns.md`, such as:
   - `[0-9]+` - Find all numbers
   - `[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}` - Find email addresses
   - `https?://[a-zA-Z0-9./-]+` - Find URLs
   - `\[(ERROR|WARN|INFO|DEBUG)\]` - Find log levels

5. Test search navigation with `Ctrl-N` (next) and `Ctrl-P` (previous)

6. Test replace functionality with regex patterns

### General Testing

The other test files can be opened directly to test specific features:
```bash
./fac scratch_files/cursor_test.txt
./fac scratch_files/pane_test.txt
# etc.
```

## Notes

- These files are for testing and development purposes
- Feel free to modify or add new test cases
- Automated test scripts may require `expect` to be installed
