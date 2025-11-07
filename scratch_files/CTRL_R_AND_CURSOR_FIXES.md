# Ctrl-R Replace and Cursor Visibility Fixes

## Issues Fixed

### Issue 1: Ctrl-R Replace Not Working

**Problem:** When using Ctrl-F to find a match, then Tab to enter replacement text, then Ctrl-R to replace, nothing happened except the highlight was removed.

**Test Case:**
```
1. Ctrl-F → type "Email" → Ctrl-F (finds "Email" on line 4)
2. Tab → type "GMAIL"
3. Ctrl-R
4. BUG: Nothing replaced, highlight removed, cursor invisible ❌
5. EXPECTED: "Email" → "GMAIL", cursor at end of "GMAIL" ✅
```

**Root Causes:**
1. **Missing pane sync**: The replacement modified `editor%cursors` but didn't sync to `pane%cursors`
2. **Match length calculation**: If `last_match_length` wasn't set, the replacement used 0 as the match length

**Fixes:**

**File:** `src/ui/unified_search_module.f90:440-474`

```fortran
subroutine replace_current_and_advance(editor, buffer)
    type(editor_state_t), intent(inout) :: editor
    type(buffer_t), intent(inout) :: buffer
    integer :: match_len

    if (.not. allocated(current_search_pattern)) return
    if (.not. allocated(current_replace_text)) return

    ! If cursor has selection, replace it
    if (editor%cursors(editor%active_cursor)%has_selection) then
        ! Calculate match length from selection if last_match_length not set
        if (last_match_length > 0) then
            match_len = last_match_length
        else
            ! Calculate from selection end - start
            match_len = editor%cursors(editor%active_cursor)%column - &
                       editor%cursors(editor%active_cursor)%selection_start_col
        end if

        call perform_replacement(buffer, editor%cursors(editor%active_cursor), &
                                current_replace_text, match_len)

        ! Clear selection after replacement
        editor%cursors(editor%active_cursor)%has_selection = .false.

        ! Sync cursor to pane (important!)   ← ADDED THIS
        call sync_editor_to_pane(editor)

        ! Re-count matches after replacement
        call count_all_matches(buffer, current_search_pattern)

        ! Render to show the replacement (without selection)
        call render_screen(buffer, editor)
    end if
end subroutine replace_current_and_advance
```

**Key changes:**
1. Added `match_len` local variable
2. Calculate match length from selection if `last_match_length` is 0
3. **Added `sync_editor_to_pane(editor)` call** after replacement
4. This ensures the replacement is visible in the pane system

### Issue 2: Cursor Invisible After Enter or Ctrl-R

**Problem:** After pressing Enter to jump to a match or Ctrl-R to replace, the cursor became invisible.

**Root Cause:**
The cleanup code at the end of `show_unified_search_prompt` was calling `terminal_hide_cursor()`, which hid the cursor before returning to the main editor loop.

**Fix:**

**File:** `src/ui/unified_search_module.f90:246-249`

**Before:**
```fortran
! Clean up
call terminal_hide_cursor()   ← BAD! Hides cursor permanently
call terminal_move_cursor(editor%screen_rows, 1)
call terminal_write(repeat(' ', editor%screen_cols))
```

**After:**
```fortran
! Clean up - clear the prompt line
call terminal_move_cursor(editor%screen_rows, 1)
call terminal_write(repeat(' ', editor%screen_cols))
! Don't hide cursor - let the main render loop handle cursor display
```

**Rationale:**
- The main editor render loop is responsible for cursor visibility
- Modal prompts should not permanently hide the cursor
- Other prompt modules follow this pattern (they hide during prompt but show when done)

## Testing

### Test 1: Basic Replace
```bash
./fac scratch_files/regex_test_examples.txt
```
1. Ctrl-F → type "Email" → Ctrl-F (finds first "Email")
2. Tab → type "GMAIL"
3. Press Ctrl-R
4. **Expected:**
   - ✅ "Email" replaced with "GMAIL"
   - ✅ Cursor visible at end of "GMAIL"
   - ✅ No highlight
   - ✅ Match count updated

### Test 2: Multiple Replacements
1. Ctrl-F → "Email" → Ctrl-F (finds first)
2. Tab → "GMAIL"
3. Ctrl-R (replaces first)
4. **Expected:** Cursor at end of "GMAIL", visible ✅
5. Ctrl-F (finds second "Email")
6. Ctrl-R (replaces second)
7. **Expected:** Cursor at end of second "GMAIL", visible ✅

### Test 3: Regex Replace
1. Ctrl-F → Alt-R → "[0-9]+" → Ctrl-F (finds "123")
2. Tab → "NUM"
3. Ctrl-R
4. **Expected:**
   - ✅ "123" replaced with "NUM"
   - ✅ Cursor visible at end of "NUM"
   - ✅ Variable-length match handled correctly

### Test 4: Enter Key Cursor
1. Ctrl-F → "Email" → Ctrl-F (finds match)
2. Press Enter
3. **Expected:**
   - ✅ Cursor visible at start of "Email"
   - ✅ No highlight
   - ✅ Cursor style unchanged

## Related Issues Fixed

These fixes also ensure:
1. ✅ Replacements work with both regex and literal patterns
2. ✅ Match length calculated correctly even if search state incomplete
3. ✅ Cursor remains visible throughout search/replace workflow
4. ✅ Pane system stays synchronized with editor state

## Technical Notes

### Why Two Match Length Calculations?

The `last_match_length` module variable is set by `find_next_match()`:
- For regex: The actual matched length (may differ from pattern)
- For literal: `len(pattern)`

But if the user presses Ctrl-R without searching first (or if state is corrupted), we fallback to calculating from the selection:
```fortran
match_len = cursor%column - cursor%selection_start_col
```

This makes the replacement more robust.

### Cursor Visibility Pattern

Modal prompts should:
1. ✅ Clear prompt area on exit
2. ❌ NOT hide cursor permanently
3. ✅ Let main render loop handle cursor display

The main editor loop calls `terminal_show_cursor()` as needed during rendering.

## Files Modified

1. `src/ui/unified_search_module.f90`:
   - Lines 440-474: Fixed `replace_current_and_advance` 
     - Added match length calculation
     - Added `sync_editor_to_pane` call
   - Lines 246-249: Removed permanent cursor hide

## Build

```bash
make clean && make
```

Build completed successfully with no errors.

## Summary

Both issues were manifestations of the same root problem: **incomplete state synchronization**. The editor has multiple layers (cursor state, pane state, terminal state) that must stay synchronized. These fixes ensure:

1. Replacements sync to pane system → visible changes
2. Cursor visibility managed correctly → visible cursor
3. Match lengths calculated robustly → correct replacements
