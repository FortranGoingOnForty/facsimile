# Pane Cursor Synchronization Fix

## Issue

When pressing Enter after finding a match with Ctrl-F, the cursor would jump to the wrong location (e.g., line 3 col 0 instead of line 4 where "Email" was found).

### Test Case
```
1. Open scratch_files/regex_test_examples.txt
2. Press Ctrl-F
3. Type "Email"
4. Press Ctrl-F to find first match on line 4
5. Press Enter
6. BUG: Cursor jumps to line 3 col 0 ❌
7. EXPECTED: Cursor on line 4 at the "E" of "Email" ✅
```

## Root Cause

The facsimile editor uses a **pane system** where cursor state is duplicated in two places:

1. **`editor%cursors`** - The "working" cursor used during editing operations
2. **`pane%cursors`** - The pane's copy of cursors for rendering

When the unified search modified the cursor position in the Enter handler:
```fortran
editor%cursors(editor%active_cursor)%line = selection_start_line
editor%cursors(editor%active_cursor)%column = selection_start_col
```

It was only updating `editor%cursors`. The pane system still had the old cursor position in `pane%cursors`, which is what gets used for rendering after the search function returns!

## Solution

Called `sync_editor_to_pane(editor)` after modifying the cursor in the Enter handler. This function copies the cursor state from `editor%cursors` to `pane%cursors`.

### Code Changes

**File:** `src/ui/unified_search_module.f90`

**Line 4:** Added import
```fortran
use editor_state_module, only: editor_state_t, cursor_t, sync_editor_to_pane
```

**Lines 210-224:** Updated Enter handler
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
        ! Sync cursor back to pane (important for pane system!)
        call sync_editor_to_pane(editor)
    end if
    exit
```

## What `sync_editor_to_pane` Does

From `src/editor_state_module.f90:767-800`:

```fortran
subroutine sync_editor_to_pane(editor)
    ! ...
    ! Copy editor state back to pane
    if (allocated(pane%cursors)) deallocate(pane%cursors)
    if (allocated(editor%cursors) .and. size(editor%cursors) > 0) then
        allocate(pane%cursors(size(editor%cursors)))
        pane%cursors = editor%cursors  ! <-- This is the key line!
        pane%active_cursor = min(editor%active_cursor, size(editor%cursors))
        ! ...
    end if
    pane%viewport_line = editor%viewport_line
    pane%viewport_column = editor%viewport_column
end subroutine
```

It copies:
- All cursor positions and states from `editor%cursors` → `pane%cursors`
- Active cursor index
- Viewport position

## Why This Matters

The pane system architecture means that:
1. Operations work on `editor%cursors` (the active working copy)
2. Rendering reads from `pane%cursors` (the pane's display copy)
3. These must be kept in sync!

Other parts of the codebase (like `command_handler_module.f90`) call `sync_editor_to_pane` after modifying cursor state. The unified search needed to do the same.

## Testing

### Test 1: Basic Enter
```bash
./fac scratch_files/regex_test_examples.txt
```
1. Ctrl-F → "Email" → Ctrl-F
2. Press Enter
3. **Expected:** Cursor at "E" of "Email" on line 4 ✅

### Test 2: Multiple Matches
1. Ctrl-F → "Email" → Ctrl-F (first match)
2. Ctrl-F (second match)
3. Ctrl-F (third match)
4. Press Enter
5. **Expected:** Cursor at start of third "Email" ✅

### Test 3: Regex Match
1. Ctrl-F → Alt-R → "[0-9]+" → Ctrl-F
2. Finds "123"
3. Press Enter
4. **Expected:** Cursor at "1" of "123" ✅

### Test 4: After Replacement
1. Ctrl-F → "test" → Tab → "example"
2. Ctrl-F (finds "test")
3. Ctrl-R (replaces with "example")
4. **Expected:** Cursor at end of "example" ✅
5. **Note:** Ctrl-R already calls sync internally

## Related Issues Fixed

This also ensures that:
- Selection state is properly synchronized
- Viewport adjustments work correctly
- Multiple cursors (if implemented) would sync properly
- Any future cursor modifications in search will work correctly

## Lessons Learned

When modifying cursor state in the editor:
1. ✅ Always check if pane system is active
2. ✅ Call `sync_editor_to_pane` after cursor modifications
3. ✅ Look for similar sync calls in other parts of the codebase as examples

## Build

```bash
make clean && make
```

Build completed successfully with no errors.
