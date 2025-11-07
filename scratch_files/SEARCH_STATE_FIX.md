# Search State Pollution Bug Fix

## Issue
When changing search patterns or toggling search options (case sensitivity, whole word, regex), the search would use stale state from the previous search, causing matches to appear at incorrect locations.

### Reproduction Steps
1. Enable regex mode (Alt-R)
2. Search for a regex pattern (e.g., `[0-9]+`)
3. Press Ctrl-F to find first match
4. Backspace to clear pattern
5. Type a new literal pattern (e.g., "Email")
6. Press Ctrl-F to search
7. **Bug:** The highlight appears at the wrong location

### Root Cause
The code tracked whether a search was "active" with `search_mode_active`, which determined whether pressing Ctrl-F would:
- Start a new search (if `search_mode_active = .false.`)
- Find the next match (if `search_mode_active = .true.`)

**The problem:** When you changed the search pattern or options, `search_mode_active` stayed `true`, so pressing Ctrl-F would call `search_forward` (find next) instead of `perform_search` (new search).

`search_forward` doesn't recompile regex patterns or reset search state, leading to:
- Using the old compiled regex instead of the new pattern
- Incorrect match lengths from previous searches
- Highlights appearing at wrong positions

## Solution

Added change detection that resets `search_mode_active` when the search parameters change.

### New Module Variables

```fortran
! Track last search parameters to detect changes
character(len=:), allocatable :: last_search_pattern
logical :: last_case_sensitive = .false.
logical :: last_whole_word = .false.
logical :: last_use_regex = .false.
```

### Change Detection Logic

In the Ctrl-F handler (line 144-154):

```fortran
! Check if search parameters changed - if so, reset search mode
if (search_mode_active) then
    if (.not. allocated(last_search_pattern) .or. &
        current_search_pattern /= last_search_pattern .or. &
        case_sensitive .neqv. last_case_sensitive .or. &
        whole_word .neqv. last_whole_word .or. &
        use_regex .neqv. last_use_regex) then
        ! Parameters changed - treat as new search
        search_mode_active = .false.
    end if
end if
```

**Note:** Used `.neqv.` (not equivalent) for logical comparisons instead of `/=` (Fortran requirement).

### Save Parameters After Search

After performing a new search (line 162-168):

```fortran
! Save current parameters
if (allocated(last_search_pattern)) deallocate(last_search_pattern)
allocate(character(len=len(current_search_pattern)) :: last_search_pattern)
last_search_pattern = current_search_pattern
last_case_sensitive = case_sensitive
last_whole_word = whole_word
last_use_regex = use_regex
```

### Cleanup

Updated `clear_search_pattern()` to reset tracking variables (line 615-634).

## Testing

### Test Case 1: Pattern Change
```
1. Open scratch_files/test_search_state.txt
2. Ctrl-F → Alt-R (enable regex) → type "[0-9]+"
3. Ctrl-F → highlights "123"
4. Backspace all, type "Email"
5. Ctrl-F → should highlight first "Email" correctly ✅
```

### Test Case 2: Regex Toggle
```
1. Ctrl-F → type "test"
2. Ctrl-F → finds "test" (literal)
3. Alt-R (enable regex) → pattern unchanged but mode changed
4. Ctrl-F → should restart search from beginning ✅
```

### Test Case 3: Case Sensitivity Toggle
```
1. Ctrl-F → type "email"
2. Ctrl-F → finds "email" (case insensitive)
3. Alt-C (enable case sensitivity) → pattern unchanged but case changed
4. Ctrl-F → should restart search, not find "Email" ✅
```

### Test Case 4: No Change (Normal Cycling)
```
1. Ctrl-F → type "test"
2. Ctrl-F → finds first "test"
3. Ctrl-F → finds second "test" (no parameter change)
4. Ctrl-F → finds third "test" (cycling works) ✅
```

## Technical Notes

### Fortran Logical Comparison
Cannot use `/=` for logical values. Must use:
- `.eqv.` for equivalence (like `==`)
- `.neqv.` for non-equivalence (like `!=`)

### String Comparison
Can use standard `/=` for character strings.

### Allocation Checking
`allocated(last_search_pattern)` returns `.false.` on first search, which correctly triggers "treat as new search".

## Files Modified

- `src/ui/unified_search_module.f90`:
  - Added tracking variables (lines 37-41)
  - Added change detection (lines 144-154)
  - Added parameter saving (lines 162-168)
  - Updated cleanup (lines 615-634)

## Build

```bash
make clean && make
```

Build completed successfully with no errors.

## Impact

This fix ensures that:
1. ✅ Changing search patterns always starts a fresh search
2. ✅ Toggling search options (Alt-R, Alt-C, Alt-W) restarts search
3. ✅ Normal search cycling (Ctrl-F repeatedly) still works
4. ✅ Regex compilation happens when needed
5. ✅ Match highlights appear at correct positions
6. ✅ No state pollution between different searches
