# Fortress Integration Plan

**Version**: 1.0
**Last Updated**: 2025-01-05

---

## Overview

This document details how to integrate the Fortress file navigator into fac for workspace and file navigation. Fortress is a Fortran-based dual-pane file explorer located at `../fortress/`.

**Goal**: Embed Fortress functionality into fac as a modal UI component triggered by Ctrl-O.

**Approach**: Copy and adapt Fortress modules rather than shelling out to external process.

**Rationale**:
- Single binary (no external dependencies)
- Full control over UI integration
- Shared terminal state management
- Better performance (no process spawning)

---

## Fortress Repository Structure

```
../fortress/
├── app/
│   ├── main.f90                           # Main program (don't copy)
│   ├── filesystem/
│   │   ├── directory_module.f90           # ✅ Directory operations
│   │   └── path_utils_module.f90          # ✅ Path manipulation
│   ├── terminal/
│   │   ├── terminal_module.f90            # ⚠️  Adapt for fac's terminal
│   │   └── input_module.f90               # ⚠️  May need adaptation
│   └── ui/
│       ├── dual_pane_module.f90           # ✅ Dual-pane rendering
│       ├── navigation_module.f90          # ✅ Navigation logic
│       └── selection_module.f90           # ✅ File/dir selection
├── src/
│   └── (library code if any)
└── fpm.toml
```

---

## Modules to Copy

### Phase 1: Core Navigation (Minimum Viable)

#### 1. `filesystem/directory_module.f90`
**Purpose**: Directory listing, stat operations, file type detection

**Key Functions**:
- `list_directory(path, entries, count)` - Get directory contents
- `is_directory(path)` - Check if path is directory
- `is_file(path)` - Check if path is regular file
- `get_parent_directory(path)` - Navigate up
- `resolve_path(path)` - Canonicalize paths
- `path_exists(path)` - Check existence

**Changes Needed**: None (should be portable)

**Dependencies**: Standard Fortran, possibly POSIX C bindings

**Copy To**: `src/fortress/filesystem/directory_module.f90`

---

#### 2. `filesystem/path_utils_module.f90`
**Purpose**: Path string manipulation

**Key Functions**:
- `join_paths(base, relative)` - Combine paths
- `basename(path)` - Extract filename
- `dirname(path)` - Extract directory
- `normalize_path(path)` - Remove `.` and `..`
- `expand_tilde(path)` - Expand `~` to home directory

**Changes Needed**: Verify home directory detection works with fac's environment

**Dependencies**: None (pure Fortran string manipulation)

**Copy To**: `src/fortress/filesystem/path_utils_module.f90`

---

#### 3. `ui/dual_pane_module.f90`
**Purpose**: Render dual-pane display (parent 30% | current 70%)

**Key Functions**:
- `render_dual_pane(parent_entries, current_entries, selected_index, width, height)`
- `format_entry(entry, is_selected, is_directory)` - Format single line
- `calculate_layout(total_width)` - Determine pane widths

**Changes Needed**:
- Use fac's `terminal_io_module` for output instead of Fortress's
- Adapt to fac's color scheme constants
- Ensure coordinate system matches fac's (1-based)

**Dependencies**:
- `directory_module` (for entry types)
- fac's `terminal_io_module` (for output)

**Copy To**: `src/fortress/ui/dual_pane_module.f90`

---

#### 4. `ui/navigation_module.f90`
**Purpose**: Handle navigation logic and key input

**Key Functions**:
- `navigate_up()` - Move cursor up
- `navigate_down()` - Move cursor down
- `navigate_into()` - Enter directory (→ or Enter)
- `navigate_back()` - Go to parent (←)
- `jump_to_home()` - Navigate to home (~)
- `jump_to_root()` - Navigate to root (/)

**Changes Needed**:
- Use fac's `input_handler_module` for key input
- Adapt key codes to match fac's constants
- Integrate with fac's event loop

**Dependencies**:
- `directory_module` (for directory operations)
- fac's `input_handler_module` (for key input)

**Copy To**: `src/fortress/ui/navigation_module.f90`

---

#### 5. `ui/selection_module.f90`
**Purpose**: Handle selection and return value

**Key Functions**:
- `select_current()` - Return selected path
- `cancel_selection()` - Return empty (ESC pressed)
- `get_selection_type()` - Determine if file or directory

**Changes Needed**: None (pure logic)

**Dependencies**:
- `directory_module` (for type checking)

**Copy To**: `src/fortress/ui/selection_module.f90`

---

### Phase 1: Exclusions (For Now)

These Fortress features will NOT be included in Phase 1:

#### Git Integration
- `git_ops_module.f90` - Git status indicators
- **Rationale**: fac's fuss menu already has git integration; fortress doesn't need it
- **Future**: Maybe add git status to fortress in Phase 7

#### Fuzzy Search (fzf)
- Any fzf integration code
- **Rationale**: Adds complexity; not critical for MVP
- **Future**: Phase 7 polish

#### Multiselect
- Multiselect/bulk operations
- **Rationale**: Not needed for workspace switching
- **Future**: Maybe later for opening multiple files

#### Bookmarks (if separate from favorites)
- Fortress-specific bookmarks
- **Rationale**: fac has its own favorites system
- **Future**: Integrate with fac's favorites.json

---

## Integration Architecture

### New Module: `navigator_module.f90`

This is the main integration point that fac will call.

**Location**: `src/fortress/navigator_module.f90`

**Purpose**: Provide high-level API for fac to invoke fortress navigation

**API**:
```fortran
module navigator_module
    use directory_module
    use dual_pane_module
    use navigation_module
    use selection_module
    implicit none

contains
    ! Main entry point for Ctrl-O
    subroutine open_fortress_navigator(selected_path, selection_type, cancelled, &
                                       initial_path)
        character(len=:), allocatable, intent(out) :: selected_path
        character(len=*), intent(out) :: selection_type  ! 'file' or 'directory'
        logical, intent(out) :: cancelled
        character(len=*), intent(in), optional :: initial_path

        ! Implementation:
        ! 1. Save current terminal state
        ! 2. Initialize fortress UI
        ! 3. Enter navigation loop
        ! 4. On selection: populate selected_path and selection_type
        ! 5. On ESC: set cancelled = .true.
        ! 6. Restore terminal state
        ! 7. Return to fac
    end subroutine

    ! Entry point for welcome menu (favorites/recents) - Phase 5
    subroutine open_fortress_welcome(selected_path, selection_type, cancelled)
        character(len=:), allocatable, intent(out) :: selected_path
        character(len=*), intent(out) :: selection_type
        logical, intent(out) :: cancelled

        ! Implementation for Phase 5
    end subroutine
end module
```

---

## Terminal State Management

### Challenge
fac already manages terminal state (raw mode, cursor, alternate screen). Fortress needs to work within this context.

### Solution: Shared Terminal Module

**Strategy**: Make fortress use fac's existing terminal infrastructure.

**fac's Terminal Modules**:
- `src/terminal/raw_mode_module.f90` - Raw mode management
- `src/terminal/terminal_io_module.f90` - Output (colors, cursor, clear)
- `src/terminal/input_handler_module.f90` - Input (key codes, escape sequences)

**Fortress Adaptation**:
1. Replace fortress's terminal output calls with fac's `terminal_io_module`
2. Use fac's key code constants
3. No need to enter/exit raw mode (already in raw mode)
4. Save/restore cursor position before/after fortress UI

**Example Adaptation**:
```fortran
! Fortress original:
call fortress_terminal_write_at(row, col, text)

! Adapted for fac:
call terminal_move_cursor(row, col)
call terminal_write(text)
```

---

## Key Input Adaptation

### Fortress Key Codes → fac Key Codes

Map fortress input handling to fac's constants:

| Key | Fortress Code | fac Code | Notes |
|-----|---------------|----------|-------|
| ↑ | `KEY_UP` | Check `input_handler_module` | Arrow keys |
| ↓ | `KEY_DOWN` | Check `input_handler_module` | Arrow keys |
| → | `KEY_RIGHT` | Check `input_handler_module` | Arrow keys |
| ← | `KEY_LEFT` | Check `input_handler_module` | Arrow keys |
| Enter | `CHAR_NEWLINE` | `char(10)` or `CHAR_NEWLINE` | Standard |
| ESC | `CHAR_ESC` | `char(27)` | Standard |
| q | `'q'` | `'q'` | Standard |
| ~ | `'~'` | `'~'` | Jump to home |
| / | `'/'` | `'/'` | Jump to root |
| 8 | `'8'` | `'8'` | Toggle favorites/recents (Phase 5) |
| f | `'f'` | `'f'` | Add to favorites (Phase 5) |

**Action**: During copy, replace fortress key constants with fac equivalents.

---

## Color Scheme Integration

### Fortress Colors
Fortress likely has its own color definitions (e.g., selected = cyan, directory = blue).

### fac Colors
fac has colors defined in `terminal_io_module.f90`:
- Cursor line highlight
- Selection highlight
- Status bar colors
- Tab colors (normal vs orphan)

### Strategy
1. **Phase 1**: Use fortress's original color scheme (minimal changes)
2. **Phase 7**: Unify with fac's color palette for consistency

**Note**: Ensure colors are defined as named constants, not hardcoded ANSI codes.

---

## Directory Structure After Integration

```
src/
├── fortress/
│   ├── filesystem/
│   │   ├── directory_module.f90
│   │   └── path_utils_module.f90
│   └── ui/
│       ├── dual_pane_module.f90
│       ├── navigation_module.f90
│       ├── selection_module.f90
│       └── navigator_module.f90       # NEW: Integration layer
├── terminal/
│   ├── raw_mode_module.f90
│   ├── terminal_io_module.f90
│   └── input_handler_module.f90
├── commands/
│   └── command_handler_module.f90     # Add Ctrl-O handler here
├── editor_state_module.f90
└── ...
```

---

## Build Order (Makefile)

Module dependencies determine build order:

```makefile
# Fortress modules (no dependencies)
src/fortress/filesystem/path_utils_module.f90

# Fortress modules (depends on path_utils)
src/fortress/filesystem/directory_module.f90

# Fortress UI modules (depends on filesystem + fac terminal)
src/fortress/ui/dual_pane_module.f90
src/fortress/ui/navigation_module.f90
src/fortress/ui/selection_module.f90

# Navigator integration (depends on all fortress modules)
src/fortress/navigator_module.f90

# Command handler (depends on navigator)
src/commands/command_handler_module.f90

# Main (depends on everything)
app/main.f90
```

**Action**: Add these to `SOURCES` in Makefile in correct order.

---

## Integration Points in fac

### 1. Command Handler (Ctrl-O)

**File**: `src/commands/command_handler_module.f90`

**Add**:
```fortran
use navigator_module

! In handle_key function:
else if (key_input == CTRL_O) then
    call handle_fortress_navigator(editor, buffer)
end if

subroutine handle_fortress_navigator(editor, buffer)
    type(editor_state_t), intent(inout) :: editor
    type(text_buffer_t), intent(inout) :: buffer
    character(len=:), allocatable :: selected_path
    character(len=256) :: selection_type
    logical :: cancelled

    ! Get current directory as starting point
    ! (from workspace path or buffer's file directory)

    call open_fortress_navigator(selected_path, selection_type, cancelled)

    if (.not. cancelled) then
        if (trim(selection_type) == 'directory') then
            ! Switch to that workspace
            call switch_workspace(editor, selected_path)
        else if (trim(selection_type) == 'file') then
            ! Open as orphan tab
            call open_orphan_tab(editor, selected_path)
        end if
    end if

    ! Re-render main UI
    call render_screen(editor, buffer)
end subroutine
```

### 2. Workspace Switching (Phase 2)

**File**: `src/workspace/workspace_module.f90` (to be created)

**API**:
```fortran
subroutine switch_workspace(editor, new_workspace_path)
    type(editor_state_t), intent(inout) :: editor
    character(len=*), intent(in) :: new_workspace_path

    ! 1. Prompt to save current workspace dirty buffers
    ! 2. Save current workspace state
    ! 3. Load new workspace state
    ! 4. Restore tabs/panes/positions
end subroutine
```

### 3. Orphan Tab Creation (Phase 3)

**File**: `src/tabs/tab_manager_module.f90` (existing)

**Add**:
```fortran
subroutine create_orphan_tab(editor, file_path)
    type(editor_state_t), intent(inout) :: editor
    character(len=*), intent(in) :: file_path

    ! 1. Create new tab
    ! 2. Mark as orphan (orphan flag)
    ! 3. Load file
    ! 4. Set tab label to basename
    ! 5. Set tab color to greyish
end subroutine
```

---

## State Management

### Entering Fortress Navigator

**Before showing fortress UI**:
1. Save current cursor position: `call terminal_get_cursor(saved_row, saved_col)`
2. Clear screen or enter alternate screen: `call terminal_clear_screen()`
3. Hide fac's status bar
4. Initialize fortress state (current directory, selection index)

### Exiting Fortress Navigator

**After user selection or cancellation**:
1. Clear fortress UI
2. Restore fac's screen: `call render_screen(editor, buffer)`
3. Restore cursor position (if needed)
4. Return control to main loop

**No Need To**:
- Enter/exit raw mode (already in raw mode)
- Change terminal settings (fac already configured)

---

## Error Handling

### Directory Access Errors
- **Permission denied**: Show error message in fortress UI, stay in current directory
- **Directory deleted**: Fall back to parent or home directory

### File Open Errors
- **Permission denied**: Show error in fac status bar, don't create tab
- **File too large**: Prompt user to confirm before loading

### Path Resolution Errors
- **Symlink loops**: Detect and break, show error
- **Invalid paths**: Fallback to current directory

---

## Testing Strategy

### Unit Tests (Per Module)

**directory_module**:
- List directory with various paths (., .., ~, /)
- Handle nonexistent directories
- Detect file types correctly
- Resolve paths correctly

**navigation_module**:
- Navigate up/down with boundaries
- Enter directories
- Go to parent
- Jump to home/root

**dual_pane_module**:
- Render with various terminal widths
- Handle empty directories
- Handle directories with many entries (scrolling)
- Format entries correctly (colors, icons)

### Integration Tests

**Test 1: Basic Navigation**
```bash
./fac test.txt
# Press Ctrl-O
# Should see dual-pane navigator
# Navigate with arrows
# Press ESC
# Should return to editor
```

**Test 2: Directory Selection**
```bash
./fac test.txt
# Press Ctrl-O
# Navigate to a directory
# Press Enter on directory
# Should switch to workspace mode (Phase 2)
```

**Test 3: File Selection**
```bash
./fac test.txt  # In workspace mode
# Press Ctrl-O
# Navigate to file outside workspace
# Press Enter on file
# Should open as orphan tab
```

**Test 4: Edge Cases**
- Navigate to root (/)
- Navigate to home (~)
- Navigate to nonexistent directory (should handle gracefully)
- Cancel with ESC (should return to editor unchanged)

---

## Performance Considerations

### Directory Listing
- **Large directories**: Limit display to visible entries + buffer (e.g., 1000 entries max)
- **Lazy loading**: Only stat entries when needed (not all 10k files upfront)

### Rendering
- **Only redraw changed panes**: Track dirty state
- **Batch terminal output**: Use single write call per frame

### Path Operations
- **Cache directory listings**: Don't re-scan on every cursor move
- **Invalidate cache**: Only on directory change or explicit refresh

---

## Phase 1 Completion Criteria

Fortress integration is complete for Phase 1 when:

- ✅ All fortress modules copied and adapted
- ✅ `navigator_module.f90` created with clean API
- ✅ Ctrl-O opens dual-pane navigator
- ✅ Can navigate filesystem with arrow keys
- ✅ Can enter directories (→ or Enter)
- ✅ Can go to parent (←)
- ✅ Can jump to home (~) and root (/)
- ✅ Can cancel (ESC) and return to editor
- ✅ Selecting directory returns path correctly
- ✅ Selecting file returns path correctly
- ✅ No regressions in fac's existing features
- ✅ Build system updated (Makefile)
- ✅ Basic tests pass

**Not Required for Phase 1**:
- Favorites/recents (Phase 5)
- Git integration
- Fuzzy search
- Multiselect
- Workspace switching (that's Phase 2)
- Orphan tab creation (that's Phase 3)

---

## Debugging Tips

### Terminal State Debugging
- Use `/tmp/fortress_debug.txt` for logging (not stdout/stderr)
- Log cursor positions, key inputs, navigation state

### Visual Debugging
- Add visual markers for debugging (e.g., border characters)
- Temporarily show state in header line

### Common Issues
- **Cursor disappears**: Check if fortress is hiding cursor (should use fac's cursor management)
- **Colors wrong**: Verify ANSI codes match expectations
- **Keys not working**: Check key code constants match
- **Rendering artifacts**: Ensure screen clearing before redraw

---

## Future Enhancements (Post-Phase 1)

### Phase 5: Favorites & Recents
- Add `favorites_module.f90` to load/save favorites
- Integrate with navigator_module
- Add keybindings ('8' toggle, 'f' add favorite)

### Phase 7: Polish
- Add git status indicators (use fuss menu's git module)
- Add fuzzy search (optional fzf integration)
- Unify color scheme with fac
- Performance optimization
- Add icons for file types (if UTF-8 safe)

---

## Related Documents

- `WORKSPACE_VISION.md` - Overall design vision
- `workspace_spec.md` - JSON schema for workspace.json
- `config_spec.md` - favorites.json and recents.json formats (next doc)
- `WORKSPACE_ROADMAP.md` - Implementation phases

---

**End of Integration Plan**
