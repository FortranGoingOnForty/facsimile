# Workspace Mode Implementation Roadmap

**Vision**: Transform fac into a workspace-aware editor with persistent state and Fortress-based navigation.

**Timeline**: 7 phases, estimated 15-20 sessions total

---

## Phase 0: Planning & Design
**Duration**: 1 session
**Objective**: Document architecture and create foundational specs

### Tasks
- [x] Define workspace data structures
- [ ] Design `.fac/` directory structure
- [ ] Design `~/.config/fac/` structure
- [ ] Create workspace.json schema
- [ ] Create favorites.json schema
- [ ] Document Fortress integration points
- [ ] Plan module dependencies

### Deliverables
```
docs/
  workspace_spec.md       # Workspace JSON format
  fortress_integration.md # How fortress modules integrate
  config_spec.md         # Config file formats
```

### Files to Create
- `docs/workspace_spec.md`
- `docs/fortress_integration.md`
- `docs/config_spec.md`

### Success Criteria
- ✅ Clear data structure specifications
- ✅ Agreed-upon file formats
- ✅ Module integration plan documented

---

## Phase 1: Fortress Navigator Foundation
**Duration**: 3-4 sessions
**Objective**: Build basic dual-pane file/directory navigator (Ctrl-O)

### Tasks
- [ ] Copy fortress filesystem modules into fac
  - `src/fortress/filesystem/` (directory reading, path handling)
- [ ] Copy fortress terminal modules
  - `src/fortress/terminal/` (rendering utilities)
- [ ] Adapt fortress UI for embedded mode
  - `src/fortress/ui/` (dual-pane display, navigation)
- [ ] Implement dual-pane rendering
  - Left pane: parent directory (30%)
  - Right pane: current directory (70%)
- [ ] Add navigation keybindings
  - ↑/↓: Move cursor
  - →: Enter directory
  - ←: Go to parent
  - ~/: Jump to home/root
  - q: Cancel/exit
  - Enter: Select (return selection)
- [ ] Add Ctrl-O keybinding to main editor
- [ ] Modal Fortress UI (takes over screen, returns selection)

### Files to Create
```
src/fortress/
  filesystem/
    directory_module.f90      # Read dirs, list files
    path_utils_module.f90     # Path manipulation
  terminal/
    fortress_render_module.f90 # Rendering utilities
  ui/
    dual_pane_module.f90      # Dual-pane display
    navigator_module.f90      # Main navigation logic
```

### Files to Modify
- `src/commands/command_handler_module.f90` (add Ctrl-O handler)
- `Makefile` (add fortress modules to build)

### Success Criteria
- ✅ Ctrl-O opens dual-pane navigator
- ✅ Can navigate filesystem
- ✅ Selecting directory returns path
- ✅ Selecting file returns path
- ✅ ESC/q cancels and returns to editor
- ✅ Visual styling matches fac aesthetic

### Testing Checkpoints
- Navigate to various directories
- Test with long file lists (scrolling)
- Test with deep directory trees
- Test cancellation (ESC)

---

## Phase 2: Workspace Detection & Configuration
**Duration**: 2-3 sessions
**Objective**: Detect workspace directories, create/load workspace configs

### Tasks
- [ ] Create workspace detection logic
  - Check for `.fac/workspace.json` in directory
  - Hash path for config file lookup
- [ ] Implement workspace config directory structure
  ```
  ~/.config/fac/
    favorites.json
    recents.json
  .fac/                    # In workspace root
    workspace.json
    backups/
  ```
- [ ] Create workspace module
  - `workspace_init()` - detect or create workspace
  - `workspace_load()` - load workspace.json
  - `workspace_save()` - save current state
  - `workspace_exists()` - check if path has workspace
- [ ] Command line argument parsing
  - `fac` (no args) → open Fortress welcome menu
  - `fac .` → load/create workspace for cwd
  - `fac /path/to/dir` → load/create workspace
  - `fac file.txt` → check parent for workspace, else single-file mode
- [ ] Create empty workspace with default state
- [ ] Basic save/load (no tabs/panes yet, just metadata)

### Files to Create
```
src/workspace/
  workspace_module.f90       # Core workspace logic
  workspace_config_module.f90 # JSON serialization
  workspace_detection_module.f90 # Path checking
```

### Files to Modify
- `app/main.f90` (parse args, detect workspace mode)
- `src/editor_state_module.f90` (add workspace_path field)

### Success Criteria
- ✅ `fac .` creates `.fac/workspace.json`
- ✅ `fac /existing/workspace` loads existing config
- ✅ `fac file.txt` works as before (single-file mode)
- ✅ `fac` (no args) opens Fortress welcome (placeholder for now)
- ✅ Workspace state persists across runs

### Testing Checkpoints
- Create workspace in empty directory
- Load existing workspace
- Verify single-file mode unchanged
- Check `.fac/` directory creation

---

## Phase 3: State Serialization (Tabs & Panes)
**Duration**: 3-4 sessions
**Objective**: Persist and restore all editor state

### Tasks
- [ ] Design workspace.json schema
  ```json
  {
    "version": "1.0",
    "workspace_path": "/home/user/project",
    "last_opened": "2025-01-05T10:30:00Z",
    "tabs": [
      {
        "label": "main.f90",
        "panes": [
          {
            "file": "src/main.f90",
            "cursor": {"line": 42, "col": 10},
            "viewport": {"line": 30, "col": 1},
            "modified": false
          }
        ],
        "split_type": "none|vertical|horizontal",
        "active_pane": 0
      }
    ],
    "orphan_tabs": [
      {
        "file": "/etc/hosts",
        "cursor": {"line": 1, "col": 1},
        "viewport": {"line": 1, "col": 1}
      }
    ],
    "active_tab": 0,
    "fuss_mode": {
      "active": true,
      "width": 30
    }
  }
  ```
- [ ] Implement tab serialization
  - Serialize tab list, labels, active index
- [ ] Implement pane serialization
  - Serialize pane splits, layout, active pane
- [ ] Implement cursor/viewport serialization
  - Save position per file
- [ ] Implement deserialization (reverse of above)
- [ ] Add orphan tab tracking
  - Mark tabs as orphan vs workspace
  - Different color in tab bar (greyish)
- [ ] Call save on quit
- [ ] Call load on workspace open
- [ ] Handle relative vs absolute paths
  - Store workspace files as relative paths
  - Store orphan files as absolute paths

### Files to Create
```
src/workspace/
  tab_serializer_module.f90   # Tab state save/load
  pane_serializer_module.f90  # Pane state save/load
```

### Files to Modify
- `src/workspace/workspace_module.f90` (save/load implementation)
- `src/editor_state_module.f90` (add orphan flag to tabs)
- `src/terminal/renderer_module.f90` (orphan tab color)
- `app/main.f90` (call save on quit, load on open)

### Success Criteria
- ✅ Quit fac with 3 tabs → reopen → 3 tabs restored
- ✅ Split panes → quit → reopen → splits preserved
- ✅ Cursor at line 50 → quit → reopen → cursor at line 50
- ✅ Orphan tabs show in different color
- ✅ Workspace files use relative paths
- ✅ Orphan files use absolute paths

### Testing Checkpoints
- Create complex workspace (multiple tabs, splits)
- Edit files, move cursors
- Quit and reopen
- Verify exact state restoration
- Test orphan tab creation via Fortress

---

## Phase 4: Backup System
**Duration**: 2 sessions
**Objective**: Auto-backup dirty buffers, restore with diff option

### Tasks
- [ ] Create backup directory structure
  ```
  .fac/
    backups/
      main.f90.bak
      test.f90.bak
      .backup-metadata.json
  ```
- [ ] Implement backup on quit
  - Detect modified buffers
  - Prompt per file: "Save main.f90? [y/n/c]"
  - If 'n', create backup file
  - Write backup with timestamp in metadata
- [ ] Implement backup detection on open
  - Check for backup files in `.fac/backups/`
  - Compare timestamps (backup vs disk file)
- [ ] Implement restore prompt
  - For each backup: "[r]estore / [i]gnore / [d]iff?"
  - 'd' shows diff (use system diff or built-in)
- [ ] Implement diff viewer
  - Simple side-by-side or unified diff
  - Or shell out to `diff` command
- [ ] Backup cleanup
  - Delete backup after successful restore
  - Or keep for N days (config option)

### Files to Create
```
src/workspace/
  backup_module.f90          # Backup creation/restoration
  diff_viewer_module.f90     # Simple diff display (optional)
```

### Files to Modify
- `src/workspace/workspace_module.f90` (backup on quit/load)
- `app/main.f90` (call backup logic)

### Success Criteria
- ✅ Quit with unsaved file → prompted to save
- ✅ Choose 'n' → backup created
- ✅ Reopen workspace → prompted to restore backup
- ✅ 'd' shows diff between backup and disk
- ✅ 'r' restores backup
- ✅ 'i' ignores backup
- ✅ Metadata tracks backup timestamps

### Testing Checkpoints
- Edit file, don't save, quit
- Verify backup created
- Modify file on disk (external editor)
- Reopen workspace
- Verify diff shows both changes
- Test restore, test ignore

---

## Phase 5: Favorites & Recents
**Duration**: 2 sessions
**Objective**: Track favorite/recent workspaces, Fortress welcome menu

### Tasks
- [ ] Create favorites system
  ```
  ~/.config/fac/
    favorites.json
      {
        "favorites": [
          {"path": "/home/user/project", "label": "My Project"},
          {"path": "/home/user/scripts", "label": "Scripts"}
        ]
      }
  ```
- [ ] Create recents system
  ```
  ~/.config/fac/
    recents.json
      {
        "recents": [
          {"path": "/home/user/project", "last_opened": "2025-01-05T10:30:00Z"},
          {"path": "/home/user/other", "last_opened": "2025-01-04T09:15:00Z"}
        ]
      }
  ```
- [ ] Update recents on workspace open
  - Add current workspace to recents
  - Keep most recent 10-20 entries
- [ ] Implement Fortress welcome menu
  - Press '8' to toggle between favorites/recents view
  - Two separate panes/dialogs
  - Favorites pane: list favorites, keybind to add/remove ('f')
  - Recents pane: list by most recent first
  - Navigate with ↑/↓, Enter to select
- [ ] Implement `fac` (no args) behavior
  - Launch Fortress welcome menu
  - Select favorite/recent → load workspace
  - Or navigate filesystem to find new workspace
- [ ] Add keybind in Fortress to add current dir to favorites ('f')
- [ ] Add keybind to remove favorite ('r' or 'x')

### Files to Create
```
src/workspace/
  favorites_module.f90       # Manage favorites
  recents_module.f90         # Manage recents
src/fortress/ui/
  welcome_menu_module.f90    # Favorites/recents display
```

### Files to Modify
- `src/fortress/ui/navigator_module.f90` (add '8' toggle, 'f' favorite)
- `src/workspace/workspace_module.f90` (update recents on open)
- `app/main.f90` (no args → welcome menu)

### Success Criteria
- ✅ `fac` opens welcome menu with favorites/recents
- ✅ Press '8' toggles between favorites and recents views
- ✅ Selecting from list opens that workspace
- ✅ 'f' in Fortress adds directory to favorites
- ✅ Recents automatically updated on workspace open
- ✅ Most recent workspaces appear first

### Testing Checkpoints
- Run `fac` with no args
- Verify favorites/recents display
- Add favorite, verify persists
- Open workspace, verify appears in recents
- Toggle between views with '8'

---

## Phase 6: Workspace Switching & Integration
**Duration**: 2-3 sessions
**Objective**: Connect all pieces, handle workspace switching

### Tasks
- [ ] Implement workspace switching flow
  - Ctrl-O in workspace → Fortress navigator
  - Select directory → save current workspace
  - Load new workspace (or create if new)
  - Restore tabs/panes/state
- [ ] Handle orphan tab creation
  - Fortress select file → open in new tab
  - Mark tab as orphan
  - Style with grey/subtle color
  - Don't persist orphan tabs in workspace.json
  - OR persist in separate "orphan_tabs" array (decided in Phase 3)
- [ ] Implement save prompts on switch
  - Before switching workspace
  - For each dirty buffer: "Save file? [y/n/c]"
  - 'c' cancels workspace switch
- [ ] Handle edge cases
  - Missing files in workspace.json
    - Show warning: "File not found: src/missing.f90"
    - Skip that tab, continue loading others
  - Deleted workspace directory
    - Detect and remove from recents
  - Corrupted workspace.json
    - Fallback to empty workspace
    - Log error
- [ ] Update file tree (Ctrl-B) for workspace mode
  - Show workspace root (not parent of first file)
  - Update tree when switching workspaces

### Files to Modify
- `src/fortress/ui/navigator_module.f90` (return file vs dir)
- `src/workspace/workspace_module.f90` (switch logic)
- `src/commands/command_handler_module.f90` (Ctrl-O handler)
- `src/workspace/file_tree_module.f90` (workspace root)
- `app/main.f90` (orchestrate switching)

### Success Criteria
- ✅ Ctrl-O from workspace opens Fortress
- ✅ Selecting new directory switches workspace
- ✅ Current workspace saved before switch
- ✅ New workspace loaded with all state
- ✅ Dirty buffers prompt to save before switch
- ✅ Orphan tabs work correctly
- ✅ Missing files handled gracefully
- ✅ File tree shows workspace root

### Testing Checkpoints
- Create two workspaces with different files
- Switch between them via Fortress
- Verify state saves/loads correctly
- Test with dirty buffers
- Test missing file handling

---

## Phase 7: Polish, Testing & Documentation
**Duration**: 2 sessions
**Objective**: Bug fixes, edge cases, user testing

### Tasks
- [ ] Performance optimization
  - Large workspaces (100+ files)
  - Deep directory trees in Fortress
- [ ] Visual polish
  - Orphan tab color (grey/subtle)
  - Fortress UI styling
  - Welcome menu appearance
- [ ] Error handling
  - Permissions errors
  - Disk full
  - Invalid JSON
- [ ] Edge case testing
  - Symlinks in workspace
  - Very long file paths
  - Unicode in filenames/paths
  - Binary files in backups
- [ ] Documentation
  - Update README.md
  - Update --help output
  - Create WORKSPACE.md guide
  - Add examples
- [ ] User testing
  - Real-world projects
  - Multiple sessions
  - Collect feedback
- [ ] Fix any cursor/rendering bugs that emerge
- [ ] Remove debug logging

### Files to Create
- `WORKSPACE.md` (user guide)

### Files to Modify
- `README.md` (document workspace features)
- `app/main.f90` (update --help)

### Success Criteria
- ✅ All features working smoothly
- ✅ No regressions in existing features
- ✅ Performance acceptable on large projects
- ✅ Documentation complete
- ✅ Ready for release

### Testing Checkpoints
- Test with Linux kernel source (large workspace)
- Test with multiple nested directories
- Test workspace switching under load
- Test all edge cases
- User acceptance testing

---

## Module Dependency Graph

```
app/main.f90
  ├── workspace_module.f90
  │   ├── workspace_config_module.f90
  │   ├── workspace_detection_module.f90
  │   ├── tab_serializer_module.f90
  │   ├── pane_serializer_module.f90
  │   ├── backup_module.f90
  │   ├── favorites_module.f90
  │   └── recents_module.f90
  │
  └── fortress/
      ├── filesystem/
      │   ├── directory_module.f90
      │   └── path_utils_module.f90
      ├── terminal/
      │   └── fortress_render_module.f90
      └── ui/
          ├── dual_pane_module.f90
          ├── navigator_module.f90
          └── welcome_menu_module.f90
```

---

## Risk Mitigation

### Cursor/Rendering Bugs
**Risk**: New pane/tab logic breaks cursor positioning
**Mitigation**:
- Test after each phase
- Keep parity tests from current build
- Incremental changes, test often

### State Corruption
**Risk**: Corrupted workspace.json breaks workspace
**Mitigation**:
- JSON validation on load
- Fallback to empty workspace
- Keep backup of last-good workspace.json

### Performance
**Risk**: Large workspaces slow down editor
**Mitigation**:
- Lazy load tabs (don't load all buffers at once)
- Profile and optimize
- Set reasonable limits (e.g., max 50 tabs)

### Backwards Compatibility
**Risk**: Breaking existing fac usage
**Mitigation**:
- Single-file mode must work unchanged
- Feature flags during development
- Extensive testing of non-workspace mode

---

## Success Metrics

**Phase Completion:**
- All tasks in phase completed
- Success criteria met
- Tests passing
- No regressions

**Overall Success:**
- `fac` launches welcome menu
- `fac .` creates/loads workspace
- Ctrl-O navigates to workspaces/files
- Tabs/panes/cursors persist across sessions
- Orphan tabs work as expected
- Favorites/recents work
- Backups restore correctly
- Documentation complete
- No bugs in existing features

---

## Timeline Estimate

- **Phase 0**: 1 session (4-6 hours)
- **Phase 1**: 3-4 sessions (12-16 hours)
- **Phase 2**: 2-3 sessions (8-12 hours)
- **Phase 3**: 3-4 sessions (12-16 hours)
- **Phase 4**: 2 sessions (8-10 hours)
- **Phase 5**: 2 sessions (8-10 hours)
- **Phase 6**: 2-3 sessions (8-12 hours)
- **Phase 7**: 2 sessions (8-10 hours)

**Total**: ~15-20 sessions (~60-80 hours)

---

## Next Steps

1. Review this roadmap
2. Make any adjustments
3. Begin Phase 0 (Planning & Design)
4. Create specification documents
5. Start Phase 1 (Fortress Navigator)

Ready to proceed? 🚀
