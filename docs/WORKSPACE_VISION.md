# Workspace Mode - Vision & Design Decisions

This document captures the key design decisions and vision for workspace mode.

---

## Vision Summary

Transform `fac` into a workspace-aware editor where:
- **Workspace** = directory + persistent editor state (tabs, panes, positions)
- **Fortress** = navigator UI for finding workspaces and files
- State persists across sessions
- Single-file mode still works unchanged

---

## Key Concepts

### Workspace
A workspace is created when opening a directory:
```bash
fac .                    # Open cwd as workspace
fac ~/projects/myapp     # Open that directory as workspace
fac                      # Opens Fortress welcome menu
```

**Workspace contains:**
- Tabs and which files are open
- Pane splits and layouts
- Cursor positions per file
- Viewport scroll positions
- Fuss mode state (open/closed)
- Dirty buffer backups

**Persisted in:** `.fac/workspace.json` in workspace root

### Fortress Navigator (Ctrl-O)
Dual-pane file/directory browser for:
1. **Opening new workspaces** - navigate to directory, press Enter
2. **Opening orphan files** - navigate to file (outside workspace), press Enter

**Features:**
- Dual-pane display (parent 30% | current 70%)
- Navigate with arrow keys
- Enter on directory → switch to that workspace
- Enter on file → open as orphan tab in current workspace
- '8' key → toggle between favorites and recents view
- 'f' key → add current directory to favorites
- ESC/q → cancel and return

### Orphan Tabs
Files opened from outside the workspace via Fortress:
- Displayed in **greyish/subtle color** in tab bar
- NOT persisted in workspace state
- If closed, hard to reopen (need Fortress again)
- Similar to VSCode's non-workspace files

### Favorites & Recents
Stored in `~/.config/fac/`:
- **favorites.json** - manually marked favorite workspaces
- **recents.json** - automatically tracked recent workspaces (last 10-20)
- Press '8' in Fortress to toggle between favorites/recents view

---

## File Structure

### Workspace Root
```
~/projects/myapp/
  .fac/
    workspace.json       # Editor state (tabs, panes, positions)
    backups/             # Dirty buffer backups
      main.f90.bak
      test.f90.bak
      .backup-metadata.json
  src/
    main.f90
    ...
```

### User Config
```
~/.config/fac/
  favorites.json         # User-marked favorites
  recents.json          # Auto-tracked recent workspaces
```

---

## Command Line Behavior

| Command | Behavior |
|---------|----------|
| `fac` | Open Fortress welcome menu (favorites/recents) |
| `fac .` | Open cwd as workspace (create if new) |
| `fac ~/dir` | Open that directory as workspace |
| `fac file.txt` | Single-file mode (check parent for workspace) |

---

## Key Workflows

### 1. Start Working on Project
```bash
cd ~/projects/myapp
fac .
# Opens workspace (or creates if first time)
# Restores tabs/panes from last session
```

### 2. Switch to Another Project
While in fac:
```
Ctrl-O
# Navigate to ~/projects/other-project
# Press Enter
# Saves current workspace state
# Opens new workspace with its state
```

### 3. Open Orphan File
While in fac workspace:
```
Ctrl-O
# Navigate to /etc/hosts (outside workspace)
# Press Enter
# Opens in new tab (grey color, not persisted)
```

### 4. Quick Edit Single File
```bash
fac /etc/hosts
# Single-file mode (no workspace)
# Works like current fac
```

---

## Design Decisions

### 1. Orphan Tabs
- **Visual**: Greyish/subtle color in tab bar
- **Persistence**: NOT saved in workspace.json
- **Rationale**: Similar to VSCode - you can open any file, but it won't be restored next session

### 2. Favorites vs Recents
- **Both available**, like VSCode
- **Toggle with '8' key** between views
- **Favorites**: Manual (user adds with 'f' key)
- **Recents**: Automatic (last 10-20 opened workspaces)

### 3. Workspace Discovery
- `fac` with no args → Fortress welcome menu
- `fac path` → Try to load workspace from that path
- Silent creation (no prompt) when creating new workspace

### 4. Backup Strategy
- **On quit**: Prompt to save dirty buffers
- **If 'n'**: Create backup in `.fac/backups/`
- **On open**: Detect backups, prompt to restore
- **Options**: [r]estore / [i]gnore / [d]iff
- **Always show diff option** for safety

### 5. Storage Location
- **Workspace state**: `.fac/workspace.json` in workspace root
  - Rationale: Self-contained, obvious location, easy to delete
- **User config**: `~/.config/fac/`
  - Rationale: Standard XDG location for user preferences

### 6. File Paths in workspace.json
- **Workspace files**: Relative paths (e.g., `src/main.f90`)
- **Orphan files**: Absolute paths (e.g., `/etc/hosts`)
- **Rationale**: Workspace portable if directory moves

### 7. Missing Files
- **Behavior**: Show warning, skip that tab, continue loading
- **Warning**: "File not found: src/missing.f90"
- **Rationale**: Don't block workspace load due to one missing file

### 8. Fortress Integration
- **Copy Fortress modules into fac** (not shell out)
- **Use**: Filesystem, terminal, UI modules
- **Omit**: Git integration (fuss menu has this), multiselect (for now)
- **Rationale**: Single binary, no Python dependency, full control

---

## State Persistence

### What Gets Saved in workspace.json
✅ Tab list with file paths
✅ Pane splits and layouts
✅ Cursor positions per file
✅ Viewport scroll positions per file
✅ Active tab index
✅ Active pane per tab
✅ Fuss mode state (open/closed, width)

❌ Orphan tabs (not persisted)
❌ Undo history (too large, unnecessary)
❌ Search history (not critical)
❌ Yank stack (session-specific)

### workspace.json Schema
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
  "active_tab": 0,
  "fuss_mode": {
    "active": true,
    "width": 30
  }
}
```

---

## Fortress Keybindings

### Navigation
- `↑/↓`: Move cursor
- `→`: Enter directory
- `←`: Go to parent
- `~`: Jump to home directory
- `/`: Jump to root directory
- `Enter`: Select (directory = switch workspace, file = open as orphan)
- `q/ESC`: Cancel and return to editor

### Favorites & Recents
- `8`: Toggle between favorites and recents view
- `f`: Add current directory to favorites
- `r/x`: Remove favorite (in favorites view)

### Future (not Phase 1)
- `s`: Fuzzy search with fzf (maybe later)
- `.`: Toggle dotfiles (maybe later)

---

## Backwards Compatibility

**Critical**: Single-file mode must work unchanged
- `fac file.txt` behaves exactly as before
- Ctrl-B shows parent directory tree
- No workspace created
- No persistence

**Testing**: Extensive regression tests for non-workspace mode

---

## Open Questions / Future Enhancements

### Not in Initial Implementation
- Fuzzy search within workspace (fzf integration)
- Multiselect in Fortress
- Git indicators in Fortress (fuss menu has this)
- Multiple workspaces open simultaneously
- Workspace templates
- Per-workspace editor settings

### Maybe Later
- Workspace sharing (commit .fac/ to git?)
- Remote workspaces
- Workspace import/export
- Workspace groups

---

## Success Criteria

The implementation is successful when:
- ✅ `fac` launches Fortress welcome menu
- ✅ `fac .` creates/loads workspace seamlessly
- ✅ Ctrl-O navigates to workspaces and files
- ✅ All tabs, panes, positions persist across sessions
- ✅ Orphan tabs work and are visually distinct
- ✅ Favorites and recents track correctly
- ✅ Backups save/restore with diff option
- ✅ Single-file mode works unchanged
- ✅ No regressions in existing features
- ✅ Documentation is complete

---

## References

- See `WORKSPACE_ROADMAP.md` for implementation phases
- See `WORKSPACE_TASKS.md` for task tracking
- See `workspace_spec.md` for technical specifications (Phase 0 deliverable)

---

Last Updated: 2025-01-05
