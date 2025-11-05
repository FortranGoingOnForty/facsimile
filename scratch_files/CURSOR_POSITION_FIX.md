# Cursor Position Fix for Enter Key

## Issue

When pressing Enter after finding a search match, the cursor was positioned incorrectly (line 3 col 0 instead of line 4 at the match).

### Expected Behavior
1. **Ctrl-F + Enter**: Cursor should be at the **START** of the matched text
2. **Ctrl-R**: Cursor should be at the **END** of the replacement text

### Actual Behavior (Before Fix)
- When Enter was pressed, the cursor remained at the **END** of the match
- This is because during search, the cursor is positioned at the end to show the selection
- The Enter key handler just exited without repositioning the cursor

## Root Cause

When a match is found, the code creates a selection like this:
```fortran
! Start of match
editor%cursors(editor%active_cursor)%selection_start_line = found_line
editor%cursors(editor%active_cursor)%selection_start_col = found_col

! End of match (cursor positioned here to show selection)
editor%cursors(editor%active_cursor)%column = found_col + match_length
```

The cursor was left at `found_col + match_length` (the end), but users expect it at `found_col` (the start) when pressing Enter.

## Solution

Updated the Enter key handler (lines 210-224) to:
1. Move cursor to the **START** of the selection
2. Clear the selection (so it's just a cursor, not a highlight)
3. Render the screen to show the new cursor position

```fortran
else if (ch == 13 .or. ch == 10) then  ! Enter - accept current match and exit
    ! Move cursor to START of match (not end)
    if (editor%cursors(editor%active_cursor)%has_selection) then
        editor%cursors(editor%active_cursor)%line = &
            editor%cursors(editor%active_cursor)%selection_start_line
        editor%cursors(editor%active_cursor)%column = &
            editor%cursors(editor%active_cursor)%selection_start_col
        editor%cursors(editor%active_cursor)%desired_column = &
            editor%cursors(editor%active_cursor)%selection_start_col
        ! Clear selection so cursor is at start, not selecting
        editor%cursors(editor%active_cursor)%has_selection = .false.
        ! Render to show cursor at correct position
        call render_screen(buffer, editor)
    end if
    exit
```

## Behavior Summary

### Enter Key (RET:go)
1. Moves cursor to START of match
2. Clears the selection highlight
3. Re-renders to show cursor at correct position
4. Exits search prompt

**Example:**
```
Search for "Email" → finds and highlights "Email"
Press Enter → cursor positioned on the "E" of "Email"
Ready to edit from the start of the match
```

### Ctrl-R (Replace)
1. Replaces the selected text with replacement text
2. Cursor positioned at END of replacement (after last character)
3. Clears selection
4. Re-renders to show the replacement

**Example:**
```
Find "test" → highlights "test"
Replace with "example"
Cursor positioned after "example" (at the 'e' position)
```

## Testing

### Test Case 1: Basic Enter
```bash
./fac scratch_files/regex_test_examples.txt
```
1. Ctrl-F → type "Email"
2. Ctrl-F → highlights first "Email" on line 4
3. Press Enter
4. **Expected:** Cursor on line 4, column at "E" of "Email" ✅
5. **Expected:** No selection/highlight ✅

### Test Case 2: Regex Match + Enter
```
1. Ctrl-F → Alt-R → type "[0-9]+"
2. Ctrl-F → highlights "123"
3. Press Enter
4. **Expected:** Cursor at the "1" of "123" ✅
```

### Test Case 3: Replace Behavior
```
1. Ctrl-F → type "foo"
2. Tab → type "bar"
3. Ctrl-F → highlights first "foo"
4. Ctrl-R → replaces with "bar"
5. **Expected:** Cursor after "bar" (at position where next char would go) ✅
```

### Test Case 4: Multiple Matches + Enter
```
1. Ctrl-F → type "test"
2. Ctrl-F → first "test" highlighted
3. Ctrl-F → second "test" highlighted
4. Ctrl-F → third "test" highlighted
5. Press Enter
6. **Expected:** Cursor at start of third "test" ✅
```

## Design Rationale

### Why START for Enter?
- User is saying "take me to this match so I can work with it"
- Starting from the beginning is most natural for editing
- Allows typing immediately to replace the text
- Consistent with "go to" semantics

### Why END for Replace?
- After replacement, user likely wants to continue editing forward
- Cursor at end allows immediate typing to append
- Standard behavior in most editors
- Allows chaining edits naturally

### Why Clear Selection?
- Enter means "accept and exit search mode"
- Keeping the highlight is confusing when not in search mode
- User can easily re-select if needed (Shift+arrows, etc.)
- Clean visual state after exiting search

## Files Modified

- `src/ui/unified_search_module.f90` (lines 210-224)
  - Enter key handler now repositions cursor to selection start
  - Clears selection after repositioning
  - Renders screen to show new cursor position

## Verification

The `perform_replacement` function already correctly positions the cursor at the end of the replacement text (line 551), so no changes were needed there.

## Build

```bash
make clean && make
```

Build completed successfully with no errors.
