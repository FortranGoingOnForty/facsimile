# Tabs Implementation Plan for Facsimile

## Overview
Add full tab support to facsimile, bridging the gap between GUI and terminal editors. Each tab represents an independent file buffer with its own cursor state and undo history.

## Requirements

### Core Features
- [x] Multiple file buffers open simultaneously
- [x] Tab bar at top of screen showing all open tabs
- [x] Active tab highlighted visually
- [x] Each tab maintains independent state:
  - Buffer content
  - Cursor position(s)
  - Undo/redo history
  - Viewport position
  - File path

### Keybindings
- [x] `alt-1` through `alt-9`: Jump to tab 1-9
- [x] `ctrl-alt-left`: Previous tab (with wrap-around)
- [x] `ctrl-alt-right`: Next tab (with wrap-around)
- [x] Check for conflicts with existing bindings (DONE - no conflicts)

### Fuss Integration
- [x] Opening file in fuss mode creates new tab by default
- [x] New tab becomes active immediately
- [x] Fuss mode persists (already implemented)

### UI/UX
- [x] Tab bar shows: `[1: file1.txt] [2: file2.f90*] [3: README.md]`
- [x] Active tab uses reverse video (char(27) // '[7m')
- [x] Modified files indicated with `*` suffix
- [x] Tab bar takes 1 row at top
- [x] Adjust editor viewport to account for tab bar (starts at row 2)
- [x] Tab bar updates automatically when switching tabs

## Architecture Design

### Data Structures

#### Tab Type (new)
```fortran
type :: tab_t
    character(len=:), allocatable :: filename
    type(buffer_t) :: buffer
    type(cursor_t), allocatable :: cursors(:)
    integer :: active_cursor
    integer :: viewport_line
    integer :: viewport_column
    logical :: modified
end type tab_t
```

#### Editor State Updates
```fortran
type(tab_t), allocatable :: tabs(:)
integer :: active_tab_index
integer :: max_tabs = 10  ! Support up to 10 tabs initially
```

### File Changes

#### src/editor_state_module.f90
- Add tab_t type definition
- Add tabs array and active_tab_index to editor_state_t
- Add procedures: create_tab, switch_tab, close_tab

#### src/terminal/renderer_module.f90
- Add render_tab_bar() subroutine
- Adjust main viewport to start at row 2 instead of row 1
- Update render_screen() to call render_tab_bar()

#### src/commands/command_handler_module.f90
- Add alt-1 through alt-9 handlers
- Add ctrl-alt-left/right handlers
- Modify open_file_in_editor() to create new tab
- Add save/restore logic for tab switching

#### src/buffer_module.f90
- Ensure buffer can be deep-copied for tab state
- May need clone_buffer() function

## Implementation Phases

### Phase 1: Core Tab Infrastructure
1. Define tab_t type
2. Add tabs array to editor state
3. Create basic tab management functions:
   - `create_new_tab(editor, filename)`
   - `switch_to_tab(editor, tab_index)`
   - `get_current_tab(editor)`

### Phase 2: Tab Bar Rendering
1. Implement `render_tab_bar()`
2. Display tab index and filename
3. Highlight active tab
4. Show modification indicator
5. Adjust viewport for tab bar

### Phase 3: Navigation Keybindings
1. Parse alt-<number> key sequences
2. Implement tab switching logic
3. Add ctrl-alt-left/right navigation
4. Save/restore cursor and viewport state

### Phase 4: Fuss Integration
1. Modify `open_file_in_editor()` to create tab
2. Set new tab as active
3. Test opening multiple files from fuss

### Phase 5: Polish & Documentation
1. Update ctrl-? help menu
2. Test edge cases (max tabs, closing tabs, etc.)
3. Handle unsaved changes warnings
4. Performance testing with many tabs

## Edge Cases to Handle

- Opening same file in multiple tabs (allow or prevent?)
- Maximum tab limit (10 initially)
- Tab overflow (show scroll indicator if >10 tabs?)
- Closing active tab (switch to next/previous)
- Closing all tabs (keep at least one empty buffer?)
- Modified file indicator updates
- Tab bar width overflow (truncate long filenames)

## Testing Plan

### Manual Tests
1. Create 3 tabs, verify each has independent buffer
2. Switch between tabs with alt-1, alt-2, alt-3
3. Navigate with ctrl-alt-left/right
4. Open files from fuss, verify new tabs created
5. Modify files in different tabs, verify * indicator
6. Close tabs, verify proper cleanup

### Integration Tests
1. Tab switching preserves cursor position
2. Tab switching preserves undo history
3. Fuss mode works correctly with multiple tabs open
4. Tab bar updates when files modified
5. Keybindings don't conflict with existing shortcuts

## Success Criteria

- [ ] Can open 10 files in separate tabs
- [ ] Each tab maintains independent state
- [ ] Alt-<number> switches tabs instantly
- [ ] Ctrl-alt-left/right cycles through tabs
- [ ] Tab bar clearly shows which tab is active
- [ ] Opening file from fuss creates new tab
- [ ] Modified files show * indicator in tab bar
- [ ] Help menu documents all tab features
- [ ] No performance degradation with 10 tabs open

## Future Enhancements (Not in Scope)

- Tab reordering (drag/drop or keyboard)
- Split panes (horizontal/vertical)
- Tab groups or sessions
- Persistent tab state between sessions
- Tab close keybinding (ctrl-w?)
- New empty tab keybinding (ctrl-t?)
