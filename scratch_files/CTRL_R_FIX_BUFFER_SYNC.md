# Ctrl-R Replace Buffer Synchronization Fix

## The Bug

When using Ctrl-F to search for "Email" on line 4, then Ctrl-R to replace it with "GMAIL":
- Wrong text was replaced (e.g., "Regex" on line 1 instead of "Email" on line 4)
- Pressing ESC would cause the replacement to revert
- Selection would reappear on the original match

## Root Cause Analysis

The issue was a **buffer synchronization problem** between the pane system and the main event loop.

### The Flow That Caused the Bug:

1. **Main loop** (main.f90:106-108): Copy pane's buffer → `buffer` parameter
   ```fortran
   call copy_buffer(buffer, editor%tabs(...) %panes(status)%buffer)
   ```

2. **Ctrl-F search**: Finds "Email" on line 4 in `buffer` copy, sets selection

3. **User presses Ctrl-R**

4. **`replace_current_and_advance`** (unified_search_module.f90:515-518):
   - Modifies **pane's buffer directly**
   - Does NOT modify the `buffer` parameter
   ```fortran
   call perform_replacement(editor%tabs(tab_idx)%panes(pane_idx)%buffer, ...)
   ```

5. **Exit `show_unified_search_prompt`**

6. **Main loop** (main.f90:122-123): Copy `buffer` parameter → pane's buffer
   ```fortran
   call copy_buffer(editor%tabs(...) %panes(status)%buffer, buffer)
   ```
   - ❌ **This OVERWRITES the pane's buffer with the old, unmodified `buffer` parameter!**
   - Our replacement is lost!

## The Fix

**File:** `src/ui/unified_search_module.f90:520-522`

After performing the replacement on the pane's buffer, immediately copy it back to the parameter buffer:

```fortran
! Perform replacement on pane's buffer with pane's cursor
call perform_replacement(editor%tabs(tab_idx)%panes(pane_idx)%buffer, &
                        editor%tabs(tab_idx)%panes(pane_idx)%cursors(...), &
                        current_replace_text, match_len)

! CRITICAL: Copy pane buffer back to parameter buffer so main loop doesn't overwrite
! The main loop copies buffer -> pane buffer after we return, so we must sync them
call copy_buffer(buffer, editor%tabs(tab_idx)%panes(pane_idx)%buffer)
```

## Why This Works

Now both buffers are synchronized:
1. We modify pane's buffer (contains the replacement)
2. We copy pane's buffer → `buffer` parameter (now both have the replacement)
3. Main loop copies `buffer` → pane's buffer (no-op, both already in sync)
4. Replacement persists! ✅

## Testing

```bash
./fac scratch_files/regex_test_examples.txt
```

1. Ctrl-F → type "Email" → Ctrl-F (finds "Email" on line 4) ✅
2. Tab → type "GMAIL"
3. Ctrl-R
4. **Expected:**
   - ✅ "Email" on line 4 replaced with "GMAIL" (NOT wrong text on wrong line)
   - ✅ Cursor at end of "GMAIL"
   - ✅ Pressing ESC does NOT revert the change
   - ✅ No strange selection reappearing

## Related Architecture Notes

The main event loop uses a **buffer synchronization pattern**:
- Before each command: Copy active pane's buffer → main `buffer`
- Process command using `buffer`
- After command: Copy main `buffer` → active pane's buffer

This pattern ensures:
1. Commands can work with a single `buffer` parameter
2. Pane-specific state is preserved
3. Changes propagate to all instances of the same file

**However**, when commands modify the pane's buffer directly (bypassing the `buffer` parameter), they MUST sync back to the parameter buffer before returning, or the main loop will overwrite their changes.

## Lesson Learned

When working with multi-layered state systems (editor state → tab state → pane state → buffer state):
- Always identify which is the "source of truth" for each operation
- Ensure modifications propagate to all necessary layers
- Watch out for synchronization code that might overwrite your changes
- Add comments explaining critical sync points
