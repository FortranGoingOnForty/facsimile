# Search and Replace Bug Fixes

## Issues Fixed

### Issue 1: Found matches not highlighted
**Problem:** When pressing Ctrl-F to search, matches were found but not visually highlighted.

**Root Cause:** The `search_forward` function was using `len(current_search_pattern)` to calculate the selection end position, which doesn't work correctly for regex patterns where the match length can differ from the pattern length.

**Fix:** Updated `search_forward` to check if regex mode is enabled and use `last_match_length` (which is set by `find_next_match`) instead of the pattern length.

**File:** `src/ui/unified_search_module.f90:382-387`

```fortran
! Use last_match_length for regex (which was set by find_next_match)
if (use_regex .and. last_match_length > 0) then
    editor%cursors(editor%active_cursor)%column = found_col + last_match_length
else
    editor%cursors(editor%active_cursor)%column = found_col + len(current_search_pattern)
end if
```

### Issue 2: Ctrl-R replace behavior
**Problem:** After pressing Ctrl-R to replace current match, the editor would:
- Keep the selection/highlight active
- Immediately jump to the next match

**Expected Behavior:** After replacement:
- Clear the selection/highlight  
- Leave cursor at the end of the replacement text
- Don't automatically jump to next match

**Fix:** Modified `replace_current_and_advance` to:
1. Perform the replacement
2. Clear the selection (`has_selection = .false.`)
3. Re-count matches (to update the match counter)
4. Removed the call to `search_forward` that was jumping to next match

**File:** `src/ui/unified_search_module.f90:396-414`

```fortran
subroutine replace_current_and_advance(editor, buffer)
    type(editor_state_t), intent(inout) :: editor
    type(buffer_t), intent(inout) :: buffer

    if (.not. allocated(current_search_pattern)) return
    if (.not. allocated(current_replace_text)) return

    ! If cursor has selection, replace it
    if (editor%cursors(editor%active_cursor)%has_selection) then
        call perform_replacement(buffer, editor%cursors(editor%active_cursor), &
                                current_replace_text, last_match_length)

        ! Clear selection after replacement
        editor%cursors(editor%active_cursor)%has_selection = .false.

        ! Re-count matches after replacement
        call count_all_matches(buffer, current_search_pattern)
    end if
end subroutine replace_current_and_advance
```

## How to Test

### Test 1: Search Highlighting

```bash
./fac scratch_files/test_search_replace.txt
```

1. Press `Ctrl-F` to open search
2. Type `test` and press Enter
3. **Expected:** The first occurrence of "test" should be highlighted (reverse video)
4. Press `Ctrl-F` again to find next
5. **Expected:** Each match should be highlighted

### Test 2: Regex Search Highlighting

1. Press `Ctrl-F` to open search
2. Press `Alt-R` to enable regex mode
3. Type `[0-9]+` (finds number sequences)
4. Press Enter
5. **Expected:** "123" should be highlighted
6. Press `Ctrl-F` for next
7. **Expected:** "456" should be highlighted (not "23" or just "1")

### Test 3: Basic Replace

1. Press `Ctrl-F` to open search
2. Type `foo` in find field
3. Press Tab to switch to replace field
4. Type `bar` in replace field
5. Press Enter to search and highlight first "foo"
6. Press `Ctrl-R` to replace
7. **Expected:**
   - "foo" → "bar"
   - Highlight removed
   - Cursor at position after "bar"
   - Editor does NOT jump to next "foo"
8. Press `Ctrl-F` to manually find next "foo"
9. Press `Ctrl-R` to replace again

### Test 4: Regex Replace (Variable Length)

1. Press `Ctrl-F`, enable regex with `Alt-R`
2. Find field: `test[0-9]+`
3. Replace field: `REPLACED`
4. Press Enter to find "test123"
5. **Expected:** All 7 characters highlighted
6. Press `Ctrl-R` to replace
7. **Expected:**
   - "test123" → "REPLACED"
   - Cursor at end of "REPLACED"
   - No selection/highlight
   - Match count decreases

## Workflow Improvements

### Before Fix
```
Ctrl-F → find → [highlighted] → Ctrl-R → [jumps to next, still highlighted]
```
User has to manually clear selection to position cursor

### After Fix
```
Ctrl-F → find → [highlighted] → Ctrl-R → [no highlight, cursor at end]
```
Clean replacement, cursor ready for editing

### Replace Workflow
```
1. Ctrl-F → search for pattern
2. Tab → switch to replace field  
3. Type replacement text
4. Enter → find first match (highlighted)
5. Ctrl-R → replace (clears highlight, stays in place)
6. Ctrl-F → find next match
7. Ctrl-R → replace
8. Repeat 6-7 as needed
```

Or for replace all:
```
1. Ctrl-F → search for pattern
2. Tab → switch to replace field
3. Type replacement text
4. Ctrl-A → replace all matches at once
```

## Related Files

- `src/ui/unified_search_module.f90` - Main search/replace implementation
- `src/terminal/renderer_module.f90` - Handles selection rendering (reverse video)
- `src/utils/regex_module.f90` - Regex pattern matching
- `src/utils/regex_wrapper.c` - POSIX regex C bindings

## Build

```bash
make clean && make
```

Build completed successfully with no warnings or errors.
