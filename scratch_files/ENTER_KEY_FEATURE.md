# Enter Key Feature in Unified Search

## Feature: Press Enter to Jump to Match

### What It Does
When you have a highlighted search match, pressing **Enter** (Return) will:
1. Close the search prompt
2. Leave the cursor positioned at the current match
3. Keep the selection active (so you can see what was matched)
4. Allow you to immediately start editing at that location

### Usage

#### Basic Search and Jump
```
1. Press Ctrl-F to open search
2. Type your search pattern (e.g., "test")
3. Press Ctrl-F to find first match → [highlighted]
4. Press Ctrl-F again to find next match → [highlighted]
5. Press Enter → closes prompt, cursor at match
6. Start typing to replace the selection, or press arrow key to deselect
```

#### Regex Search and Jump
```
1. Press Ctrl-F to open search
2. Press Alt-R to enable regex mode
3. Type regex pattern (e.g., "[0-9]+")
4. Press Ctrl-F to find first match → [highlighted]
5. Press Enter → closes prompt, cursor at the number
6. Can now edit, delete, or replace the matched text
```

### Workflow Examples

#### Example 1: Find and Edit
```
Goal: Find "TODO" and change it to "DONE"

Ctrl-F → type "TODO" → Ctrl-F → [finds "TODO", highlighted]
Enter → [prompt closes, "TODO" still selected]
Type "DONE" → [replaces "TODO" with "DONE"]
```

#### Example 2: Browse Multiple Matches
```
Goal: Review all function calls to decide which to edit

Ctrl-F → type "calculate(" → Ctrl-F → [finds first call]
Ctrl-F → [finds second call]
Ctrl-F → [finds third call] ← This is the one I want!
Enter → [stays at third call]
Edit the function call...
```

#### Example 3: Regex Pattern Selection
```
Goal: Find an email address and copy it

Ctrl-F → Alt-R → type "[a-z]+@[a-z]+\.[a-z]+"
Ctrl-F → [finds "user@example.com", highlighted]
Enter → [closes prompt, email still selected]
Ctrl-C → [copy the email to clipboard]
```

### Key Behavior

**What happens to the selection?**
- The matched text remains **selected/highlighted** after pressing Enter
- This allows you to immediately:
  - Type to replace it
  - Ctrl-C to copy it
  - Delete to remove it
  - Arrow key to deselect and keep it

**Alternative: ESC vs Enter**
- **ESC**: Closes prompt, clears selection, returns to where you were before search
- **Enter**: Closes prompt, keeps selection, stays at current match

### UI Changes

The search prompt now shows:
```
[f]:pattern /[r]:replacement [options] (1/5) RET:go ESC:exit
                                               ^^^^^^
                                               NEW!
```

**RET:go** = Press Return/Enter to go to the current match

### Code Changes

**File:** `src/ui/unified_search_module.f90`

**Line 184-187:** Added Enter key handler
```fortran
else if (ch == 13 .or. ch == 10) then  ! Enter - accept current match and exit
    ! Keep the cursor at the current match position
    ! If there's a selection, keep it (user can clear with ESC or arrow keys)
    exit
```

**Line 265 & 270:** Updated prompt text to show "RET:go ESC:exit"

### Testing

#### Test 1: Basic Enter
```bash
./fac scratch_files/test_highlight.txt
```
1. Ctrl-F → type "test" → Ctrl-F (finds first)
2. Press Enter
3. **Expected:** Prompt closes, "test" is still selected, cursor ready

#### Test 2: Multiple Matches
1. Ctrl-F → type "test" → Ctrl-F (finds first)
2. Ctrl-F (finds second)
3. Ctrl-F (finds third)
4. Press Enter
5. **Expected:** Stays at third match, selected

#### Test 3: Regex Match
1. Ctrl-F → Alt-R → type "[0-9]+"
2. Ctrl-F (finds "123")
3. Press Enter
4. **Expected:** "123" selected, ready to edit

#### Test 4: Enter vs ESC
1. Ctrl-F → type "test" → Ctrl-F
2. Try ESC → **Expected:** Clears selection, returns cursor
3. Ctrl-F → type "test" → Ctrl-F  
4. Try Enter → **Expected:** Keeps selection, stays at match

### Benefits

1. **Faster editing**: Jump to a match and immediately start editing
2. **Visual confirmation**: Selection shows exactly what matched (important for regex!)
3. **Flexible workflow**: Choose whether to keep searching (Ctrl-F) or commit (Enter)
4. **Copy matches**: Select a regex match, press Enter, press Ctrl-C to copy

### Related Commands

- **Ctrl-F**: Search/Find next
- **Ctrl-R**: Replace current match
- **Ctrl-A**: Replace all matches
- **Alt-R**: Toggle regex mode
- **Alt-C**: Toggle case sensitivity
- **Alt-W**: Toggle whole word matching
- **Tab**: Switch between find/replace fields
- **Enter**: Jump to current match (close prompt)
- **ESC**: Exit search (clear selection)

## Build

```bash
make clean && make
```

Build completed successfully with no errors.
