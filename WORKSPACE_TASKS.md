# Workspace Mode - Task Tracker

Quick reference for tracking progress through implementation phases.

## Current Phase: Phase 4 (Backup System) - Ready to begin!

---

## Phase 0: Planning & Design ✅
- [x] Define workspace data structures
- [x] Design `.fac/` directory structure
- [x] Design `~/.config/fac/` structure
- [x] Create workspace.json schema
- [x] Create favorites.json schema
- [x] Document Fortress integration points
- [x] Plan module dependencies

**Deliverables**: `workspace_spec.md`, `fortress_integration.md`, `config_spec.md` ✅

**Completed**: 2025-01-05
**Notes**: All three specification documents created with comprehensive detail:
- workspace_spec.md: Complete JSON schema with validation rules, examples, and test cases
- fortress_integration.md: Module-by-module integration plan with adaptation strategy
- config_spec.md: User config files (favorites/recents/backups) with XDG compliance

---

## Phase 1: Fortress Navigator Foundation ✅
- [x] Copy fortress filesystem modules
- [x] Copy fortress terminal modules
- [x] Adapt fortress UI for embedded mode
- [x] Implement dual-pane rendering
- [x] Add navigation keybindings
- [x] Add Ctrl-O keybinding to main editor
- [x] Modal Fortress UI

**Deliverables**: Ctrl-O opens dual-pane file/directory navigator ✅

**Completed**: 2025-11-05
**Notes**: Fortress navigator fully integrated with:
- Dual-pane layout (parent 30% | current 70%)
- Unicode separator (│)
- Smooth navigation (arrows, enter, esc)
- Optimized rendering (no flashing, cached terminal size, conditional redraws)
- Absolute cursor positioning for clean updates
- Scroll margin (3 lines) for better UX
- Files: `src/fortress/filesystem/fortress_fs_module.f90`, `src/fortress/ui/fortress_display_module.f90`, `src/fortress/fortress_navigator_module.f90`

---

## Phase 2: Workspace Detection & Configuration ✅
- [x] Create workspace detection logic
- [x] Implement workspace config directory structure
- [x] Create workspace module (init/load/save/exists)
- [x] Command line argument parsing
- [x] Create empty workspace with default state
- [x] Basic save/load (metadata only)

**Deliverables**: `fac .` creates workspace, `fac file.txt` still works ✅

**Completed**: 2025-11-05
**Notes**: Basic workspace infrastructure complete:
- workspace_module.f90 created with detection, init, load, save functions
- Command line parsing updated to detect workspace vs single-file mode
- `fac .` creates `.fac/workspace.json` with proper structure
- `fac /path/to/dir` works for workspace mode
- Single-file mode (`fac file.txt`) unchanged
- Workspace detection searches parent directories for existing workspaces

---

## Phase 3: State Serialization ✅
- [x] Design workspace.json schema
- [x] Implement tab serialization
- [x] Implement pane serialization
- [x] Implement cursor/viewport serialization
- [x] Implement deserialization
- [x] Add orphan tab tracking
- [x] Call save on quit
- [x] Call load on workspace open
- [x] Handle relative vs absolute paths

**Deliverables**: Full state persistence across sessions ✅

**Completed**: 2025-11-05
**Notes**: Complete workspace state persistence implemented:
- workspace_save_state() serializes tabs with all panes to JSON
- workspace_restore_state() parses JSON and restores tabs with cursor/viewport
- Panes array saved with coordinates (x_start, y_start, x_end, y_end)
- Orphan tabs styled in gray (ESC[90m)
- Relative paths for workspace files, absolute for orphans
- Modified flag synced from buffer to tab (asterisk display)
- Files: `src/workspace/workspace_module.f90`, `app/main.f90:207`

---

## Phase 4: Backup System ⏸️
- [ ] Create backup directory structure
- [ ] Implement backup on quit
- [ ] Implement backup detection on open
- [ ] Implement restore prompt with diff
- [ ] Implement diff viewer
- [ ] Backup cleanup

**Deliverables**: Auto-backup dirty buffers, restore with diff option

---

## Phase 5: Favorites & Recents ⏸️
- [ ] Create favorites system
- [ ] Create recents system
- [ ] Update recents on workspace open
- [ ] Implement Fortress welcome menu
- [ ] Implement `fac` (no args) behavior
- [ ] Add favorite keybinds in Fortress

**Deliverables**: `fac` opens welcome menu with favorites/recents

---

## Phase 6: Workspace Switching & Integration ⏸️
- [ ] Implement workspace switching flow
- [ ] Handle orphan tab creation
- [ ] Implement save prompts on switch
- [ ] Handle edge cases (missing files, etc.)
- [ ] Update file tree for workspace mode

**Deliverables**: Full workspace switching via Fortress

---

## Phase 7: Polish & Testing ⏸️
- [ ] Performance optimization
- [ ] Visual polish
- [ ] Error handling
- [ ] Edge case testing
- [ ] Documentation
- [ ] User testing
- [ ] Fix cursor/rendering bugs
- [ ] Remove debug logging

**Deliverables**: Production-ready workspace mode

---

## Legend
- ⏳ In Progress
- ⏸️ Not Started
- ✅ Complete
- ❌ Blocked

## Notes

### 2025-01-05: Phase 0 Complete
- Created comprehensive specification documents for all workspace components
- workspace_spec.md: 565 lines, complete JSON schema with examples and validation
- fortress_integration.md: Detailed module-by-module integration plan
- config_spec.md: User config files specification with XDG compliance
- Ready to begin Phase 1: Fortress Navigator implementation

---

### 2025-11-05: Phase 3 Complete - Pane Serialization Added
- Completed full pane serialization for split views
- workspace_save_state() now saves all panes with coordinates, filenames, cursors
- workspace_restore_state() parses panes array and restores (first pane for now)
- JSON format includes panes array with x_start, y_start, x_end, y_end coordinates
- Modified flag properly synced from buffer to tab (fixed asterisk bug)
- Phase 3 fully complete - ready for Phase 4: Backup System

---

Last Updated: 2025-11-05
