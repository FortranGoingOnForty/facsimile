# UTF-8 Migration Progress

**Goal:** Make facsimile fully UTF-8 aware so box-drawing characters (├─│└) and other multi-byte UTF-8 sequences display and edit correctly.

## Problem
Fortran's string operations work on bytes, not characters. A UTF-8 character like `├` is 3 bytes but should be treated as 1 character and displayed as 1 column.

**Example:**
- `"Hello"` → 5 bytes, 5 chars, 5 display columns ✓ (works)
- `"├──"` → 9 bytes, 3 chars, 3 display columns ✗ (broken before migration)

## ✅ Completed

### 1. Core UTF-8 Infrastructure
- **`src/utils/utf8_module.f90`** - COMPLETE
  - ✅ `utf8_char_count()` - Count UTF-8 characters
  - ✅ `utf8_char_at()` - Extract character at position
  - ✅ `utf8_char_to_byte_index()` - Convert char pos → byte pos
  - ✅ `utf8_byte_to_char_index()` - Convert byte pos → char pos
  - ✅ `utf8_display_width()` - Calculate screen columns needed
  - ✅ `utf8_char_byte_length()` - Get byte length of UTF-8 char
  - ✅ Handles 1-4 byte UTF-8 sequences
  - ✅ Handles wide characters (CJK = 2 columns)
  - ✅ Handles combining characters (0 width)

### 2. Cursor Semantics
- **`src/editor_state_module.f90`** - COMPLETE
  - ✅ Documented: `cursor%column` = UTF-8 character position (NOT byte index)
  - ✅ Added detailed comments explaining the semantics
  - ✅ Example: In `"├──"`, column=2 refers to second `─` (byte 4)

### 3. Text Buffer UTF-8 Helpers
- **`src/buffer/text_buffer_module.f90`** - COMPLETE
  - ✅ Added `use utf8_module`
  - ✅ `buffer_get_line_char_count()` - Get character count of line
  - ✅ `buffer_char_at()` - Get character at char position in line
  - ✅ `buffer_byte_to_char_col()` - Convert byte col → char col
  - ✅ `buffer_char_to_byte_col()` - Convert char col → byte col

### 4. Basic Cursor Movement
- **`src/commands/command_handler_module.f90`** - PARTIAL
  - ✅ `move_cursor_left()` - Uses `buffer_get_line_char_count()`
  - ✅ `move_cursor_right()` - Uses `buffer_get_line_char_count()`
  - ✅ Both functions now work with character positions

### 5. Module Imports
- **`src/terminal/renderer_module.f90`** - PARTIAL
  - ✅ Added `use utf8_module`
  - ✅ Added `buffer_get_line_char_count` to imports

### 6. Renderer Display (HIGH PRIORITY)
- **`src/terminal/renderer_module.f90`** - COMPLETE
  - ✅ `render_line()` - Uses UTF-8 character positions and display width
  - ✅ Converts character positions to byte positions for slicing
  - ✅ Uses `utf8_display_width()` for padding calculations
  - ✅ Cursor screen positioning uses display width calculations
  - ✅ Both active and inactive cursors positioned correctly

**Impact:** UTF-8 characters now display correctly!

## 📋 TODO (Remaining Work)

### HIGH PRIORITY - Renderer Fixes
Files: `src/terminal/renderer_module.f90`

**Specific locations that need fixing:**
- Line 83: `len(line_content)` → needs UTF-8 char count
- Line 208: `len(line)` → needs UTF-8 char count
- Line 219-220: Padding calculation needs display width
- Line 245: `len(line)` → needs UTF-8 char count
- Line 480, 487, 504, 517: Cursor screen position calculations
- Line 570-573, 597-600: Viewport scrolling with character positions
- Line 754: `len(line_content)` → needs UTF-8 char count
- Line 959-960: Viewport range calculation
- Line 1036, 1129, 1136, 1156, 1197: More cursor positioning

### MEDIUM PRIORITY - Word Movement
Files: `src/commands/command_handler_module.f90`

Functions to update:
- `move_cursor_word_left()` (line ~1105)
- `move_cursor_word_right()` (line ~1176)
- `extend_selection_word_left()` (line ~3447)
- `extend_selection_word_right()` (line ~3521)
- `delete_word_backward()` (line ~3680)
- `delete_word_forward()` (line ~690)

**Issue:** Word boundaries detected by byte operations, breaks on UTF-8

### MEDIUM PRIORITY - Editing Operations
Files: `src/commands/command_handler_module.f90`

Functions to update:
- `insert_char()` - Insert at character position
- `delete_char()` - Delete character (not byte)
- `delete_selection()` - Use character positions
- `insert_newline()` - Character position aware
- All text manipulation that uses `line(i:i)` slicing

**Issue:** Inserting/deleting can break UTF-8 sequences

### MEDIUM PRIORITY - Selection Operations
Files: `src/commands/command_handler_module.f90`

Functions to update:
- `extend_selection_left/right/up/down()` - Character boundaries
- `select_word_at_cursor()` - UTF-8 word boundaries
- `get_selected_text()` - Extract text by character positions
- Selection rendering in renderer_module

**Issue:** Selection ranges use byte positions, breaks UTF-8

### LOWER PRIORITY - Search & Find
Files: `src/prompts/*.f90`, `src/commands/command_handler_module.f90`

Functions to update:
- `find_next_occurrence()` - Search with UTF-8 awareness
- `select_next_match()` - Match by characters
- Search prompt operations

**Issue:** Pattern matching needs UTF-8 awareness

### LOWER PRIORITY - Other Operations
Various files:

- Smart home: Character-based indentation detection
- Go to column: User enters character position
- Transpose characters: Swap UTF-8 characters
- Bracket matching: Find brackets in UTF-8 text
- Line operations (move, duplicate): Should already work

## Testing Strategy

### Test Files
- `/tmp/test_unicode.txt` - Box drawing characters
- `/tmp/ctrl_d_pagination_test.txt` - For ctrl-d testing

### Test Cases
1. **Display:** Open UTF-8 file, verify box chars show correctly
2. **Cursor Movement:** Arrow keys move by character (not byte)
3. **Editing:** Type at UTF-8 char boundaries
4. **Selection:** Select text containing UTF-8 chars
5. **Search:** Find UTF-8 characters with ctrl-d
6. **Word Movement:** Alt-left/right across UTF-8 words

### Success Criteria
- Box drawing characters (├─│└) display correctly
- Cursor doesn't get "stuck" in middle of UTF-8 sequence
- Typing doesn't corrupt UTF-8 sequences
- Selections work across UTF-8 boundaries
- File saves/loads preserve UTF-8 content

## Notes

### Design Decisions
1. **Cursor column = character position** (not byte position)
   - More intuitive for users
   - Matches behavior of other editors

2. **Display width vs character count**
   - Most chars: 1 char = 1 column
   - CJK chars: 1 char = 2 columns
   - Combining: 1 char = 0 columns

3. **Viewport in character positions**
   - Viewport uses character positions
   - Converted to byte positions when rendering

### Performance Considerations
- UTF-8 operations have overhead vs byte operations
- Caching line char counts could help
- Most operations stay O(n) in line length

### Edge Cases to Handle
- Cursor at end of line (column = char_count + 1)
- Empty lines (char_count = 0)
- Files with invalid UTF-8 (treat as bytes)
- Mixed width characters (CJK)
- Combining characters

## Current Build Status
✅ Builds successfully
✅ UTF-8 module complete and tested (10/10 tests passing)
✅ Basic cursor movement works (character-based, not byte-based)
✅ Display rendering works (box chars render correctly)
✅ Character insertion works at UTF-8 boundaries
⏳ Remaining: viewport, word movement, editing ops, selections

## Test Results

### Unit Tests
Created `test/test_utf8_integration.f90` with 10 comprehensive tests:
- ✅ All 10 tests passing
- Covers: char counting, byte↔char conversion, display width, buffer integration

### Manual Testing
Tested with `/tmp/test_utf8_simple.txt` containing box-drawing chars (├──):
- ✅ Box characters display correctly in editor
- ✅ Cursor moves by CHARACTER positions (not bytes)
  - Moving right through `├` (3 bytes) increments column by 1
  - Moving right through `─` (3 bytes) increments column by 1
- ✅ Character insertion works at correct UTF-8 boundaries

Last updated: 2025-11-04
