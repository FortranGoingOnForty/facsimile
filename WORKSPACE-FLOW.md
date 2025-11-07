# Workspace Mode: User Guide

**Version**: 1.0  
**Last Updated**: November 5, 2025  
**Status**: Production Ready ✅

---

## 🎯 What is Workspace Mode?

Workspace mode transforms `fac` from a single-file editor into a full project workspace manager. When you work in workspace mode, `fac` remembers:

- All open tabs and files
- Your cursor positions
- Window splits and panes
- File tree state
- Recently opened workspaces
- Your favorite workspaces

Everything is automatically saved and restored when you return to a workspace.

---

## 🚀 Getting Started

### Creating a Workspace

**Method 1: From a Directory**
```bash
# Navigate to your project directory
cd /path/to/my-project

# Open fac in workspace mode
fac .
```

This creates a `.fac/` directory with workspace configuration:
```
my-project/
  .fac/
    workspace.json      # Workspace state
    backups/            # Backup files
  src/
  README.md
```

**Method 2: From the Welcome Menu**
```bash
# Launch fac with no arguments
fac

# You'll see:
# - Your favorite workspaces
# - Recently opened workspaces
# - Option to browse filesystem
```

---

## 📂 Workspace Operations

### Opening a Workspace

**From Command Line:**
```bash
fac /path/to/workspace
```

**From Welcome Menu:**
1. Run `fac` (no arguments)
2. Navigate with ↑/↓ or j/k
3. Press `8` to toggle between favorites and recents
4. Press Enter to open selected workspace
5. Press `b` to browse filesystem with Fortress

**From Within fac:**
- Press `Ctrl-O` to open Fortress Navigator
- Navigate to a directory
- Press Enter to switch to that workspace

### Switching Workspaces

While working in a workspace:

1. Press `Ctrl-O` to open Fortress Navigator
2. Navigate to a different directory
3. Press Enter on the directory

What happens automatically:
- ✅ Current workspace state is saved
- ✅ You're prompted to save any modified files
- ✅ New workspace is loaded (or created if new)
- ✅ Tabs and cursor positions are restored
- ✅ File tree updates to new workspace
- ✅ Recent workspaces list is updated

**Save Prompts:**
```
Save main.f90? [y/n/c]
  y - Save and continue
  n - Don't save, continue
  c - Cancel workspace switch
```

---

## ⭐ Favorites & Recents

### Favorites System

**Adding a Favorite:**
1. Press `Ctrl-O` to open Fortress Navigator
2. Navigate to the directory you want to favorite
3. Press `f` to add to favorites
4. You'll see: "Added to favorites: project-name"

**Using Favorites:**
1. Run `fac` (no arguments) to open welcome menu
2. Press `8` if not already on favorites view
3. Navigate to desired favorite
4. Press Enter to open

**Favorites are stored in:** `~/.config/fac/favorites.json`

### Recents System

Workspaces are automatically added to recents when you:
- Open them from command line
- Open them from welcome menu
- Switch to them via Fortress

**Viewing Recents:**
1. Run `fac` (no arguments)
2. Press `8` to toggle to recents view
3. Most recently used appears first
4. Shows up to 20 recent workspaces

**Recents are stored in:** `~/.config/fac/recents.json`

### Self-Cleaning Lists

If a workspace directory is deleted:
- You'll see: "Warning: Workspace no longer exists: /path"
- It's automatically removed from the list
- No manual cleanup needed

---

## 🌲 File Tree (FUSS Mode)

### Opening the File Tree

Press `Ctrl-B` to toggle the file tree sidebar (FUSS mode).

**What it shows:**
- Current workspace root directory
- All files and subdirectories
- Git status indicators (modified, staged, etc.)
- Current git branch

**File tree always shows the current workspace root**, even after switching workspaces.

### Navigating the File Tree

```
↑/↓  - Move cursor up/down
k/j  - Vim-style navigation
→    - Expand directory / Open file
←    - Collapse directory / Go to parent
Enter - Open file in new tab
g    - Stage file for commit (git)
u    - Unstage file
Ctrl-B - Toggle file tree off
```

### Git Integration

When in a git repository, the file tree shows:
- Modified files (red M)
- Staged files (green A)
- Branch name in header
- Quick staging with `g` key

---

## 🗂️ Tab Management in Workspaces

### Regular Tabs (Workspace Files)

Files within the workspace directory:
- Appear with normal tab styling
- Are saved to workspace.json
- Restore when you reopen workspace
- Use relative paths in storage

### Orphan Tabs (External Files)

Files outside the workspace directory:
- Appear with grey/dimmed styling
- Are saved to workspace.json
- Also restore when you reopen workspace
- Use absolute paths in storage

**Creating orphan tabs:**
1. Press `Ctrl-O` to open Fortress
2. Navigate to a file outside workspace
3. Press Enter to open

Example:
```
Workspace: /home/user/my-project/
Regular tab:   src/main.f90      (normal style)
Orphan tab:    /etc/hosts        (grey style)
```

### Tab Operations

```
Ctrl-N     - Next tab
Ctrl-P     - Previous tab
Ctrl-W     - Close current tab
:e <file>  - Open file in new tab
:q         - Quit (saves workspace state)
```

---

## 💾 State Persistence

### What Gets Saved

When you quit `fac` in workspace mode:

**Per Tab:**
- Filename (relative or absolute)
- Cursor position (line and column)
- Viewport position (what's visible)
- Modified flag
- Orphan flag
- Pane layout (if split)

**Workspace Level:**
- Active tab index
- All tabs (regular and orphan)
- File tree state (open/closed)
- Git repository info

**Where it's saved:**
```
<workspace-dir>/.fac/workspace.json
```

### What Gets Restored

When you open a workspace:
- All tabs recreate in same order
- Cursor jumps to saved position
- Viewport scrolls to saved position
- Active tab is selected
- Pane splits restore (if any)
- File tree initializes to workspace root

---

## 🛡️ Error Handling

### Missing Files

**Scenario:** A file was in workspace.json but no longer exists

**What happens:**
```
Warning: File not found (skipping): /path/to/missing/file.txt
```
- Warning displays for 0.8 seconds
- File is skipped
- Other files load normally
- Editor continues functioning

**Why this happens:**
- File was deleted outside fac
- File was moved/renamed
- Drive not mounted (network shares)

**What to do:**
- Nothing! The workspace adapts automatically
- Use `:e <path>` to re-add the file if it's elsewhere
- Or just continue with available files

### Corrupted Workspace

**Scenario:** workspace.json is corrupted or unreadable

**What happens:**
```
Warning: Could not open workspace.json - using empty workspace
```
- Warning displays for 1.0 seconds
- Fresh workspace is created automatically
- You start with a clean slate
- Old (corrupted) file is not deleted

**Why this happens:**
- Disk error during save
- Manual editing gone wrong
- Filesystem corruption

**What to do:**
- Nothing! Start fresh automatically
- Check `.fac/workspace.json` if you want to recover manually
- Your actual source files are unaffected

### Deleted Workspace

**Scenario:** Workspace directory was deleted

**What happens (in welcome menu):**
```
Warning: Workspace no longer exists: /path/to/workspace
Removing from list...
```
- Warning displays for 1.0 seconds
- Workspace removed from favorites/recents
- List reloads automatically
- You can select another workspace

**Why this happens:**
- Directory deleted manually
- Drive unmounted
- Network share disconnected

**What to do:**
- Nothing! Lists clean themselves automatically
- Select a different workspace

---

## 🎮 Complete Keybinding Reference

### Global (Any Mode)

```
Ctrl-O      - Open Fortress Navigator
Ctrl-B      - Toggle file tree (FUSS mode)
Ctrl-N      - Next tab
Ctrl-P      - Previous tab
Ctrl-W      - Close tab
Ctrl-/      - Help menu
:q          - Quit (saves workspace)
:e <file>   - Open file in new tab
```

### Welcome Menu

```
↑/↓  or k/j - Navigate list
Enter       - Open selected workspace
8           - Toggle favorites/recents view
b           - Browse filesystem
ESC  or q   - Quit
```

### Fortress Navigator

```
↑/↓         - Navigate files/directories
→           - Enter directory
←           - Go to parent directory
Enter       - Open file or switch to workspace
f           - Add current directory to favorites
ESC  or q   - Cancel and return
~           - Jump to home directory
/           - Jump to root directory
```

### File Tree (FUSS Mode)

```
↑/↓  or k/j - Navigate tree
→           - Expand directory / Open file
←           - Collapse directory
Enter       - Open file in new tab
g           - Stage file (git)
u           - Unstage file (git)
Ctrl-B      - Close file tree
```

---

## 🔄 Common Workflows

### Starting a New Project

```bash
# 1. Create project directory
mkdir my-project
cd my-project

# 2. Create some files
echo "program main" > main.f90

# 3. Open in workspace mode
fac .

# 4. Add to favorites (optional)
# Press Ctrl-O, navigate to current dir, press 'f'

# 5. Work on your project
# All state saves automatically on :q
```

### Switching Between Projects

```bash
# Method 1: From terminal
fac /path/to/project-a
# Work...
# :q to save and quit

fac /path/to/project-b
# Continue working...

# Method 2: From within fac
# Press Ctrl-O
# Navigate to project-b
# Press Enter
# fac saves project-a, loads project-b

# Method 3: From welcome menu
fac
# Select from favorites or recents
```

### Opening External Files While in Workspace

```bash
# You're in workspace: /home/user/my-project/

# Press Ctrl-O
# Navigate to /etc/hosts
# Press Enter on hosts

# Now you have:
# - Tab 1: src/main.f90 (regular tab, normal style)
# - Tab 2: /etc/hosts (orphan tab, grey style)

# Both tabs save to workspace.json
# Both restore when you reopen workspace
```

### Using Git with File Tree

```bash
# 1. Open workspace with Ctrl-B (file tree visible)

# 2. Edit some files
# File tree shows "M" next to modified files

# 3. Stage files for commit
# Navigate to file, press 'g'
# File shows "A" (added/staged)

# 4. Commit from command mode
:!git commit -m "My commit message"

# File tree updates to show clean state
```

### Working with Multiple Workspaces Daily

**Morning:**
```bash
fac  # Launch welcome menu
# Select "work-project" from recents
# Continue where you left off yesterday
```

**Midday:**
```bash
# Press Ctrl-O
# Navigate to "personal-scripts"
# Press Enter to switch
# work-project saves automatically
```

**Evening:**
```bash
# Press Ctrl-O
# Navigate to "learning-rust"
# Press Enter to switch
# All workspaces saved
```

**Anytime:**
```bash
fac  # Shows all recent workspaces sorted by last-used
# Most recent appears first
```

---

## 📁 File Locations

### User Configuration

```
~/.config/fac/
  favorites.json    - Your favorite workspaces
  recents.json      - Recently opened workspaces (max 20)
```

**Fallback location** (if `~/.config/fac/` doesn't exist):
```
~/.fac/
  favorites.json
  recents.json
```

### Workspace Configuration

```
<workspace-dir>/.fac/
  workspace.json    - Workspace state (tabs, cursors, etc.)
  backups/          - Backup files (when you quit without saving)
```

### What to Commit to Git

**Do commit:**
- `.fac/` directory (so team shares workspace setup)
- `workspace.json` (optional, team preference)

**Don't commit:**
- `.fac/backups/` (personal backup files)

**Add to .gitignore** (if you prefer personal workspaces):
```gitignore
.fac/
```

---

## 🎨 Visual Indicators

### Tab Bar

```
┌─────────────────────────────────────────┐
│ main.f90  │  module.f90  │  /etc/hosts │
│  (white)     (white)        (grey)      │
└─────────────────────────────────────────┘
   ↑            ↑                ↑
   Regular      Regular          Orphan tab
   tab          tab              (outside workspace)
```

### File Tree (FUSS Mode)

```
┌─────────────────────────┐
│ my-project (main)       │ ← Git branch
├─────────────────────────┤
│ [>] src/                │
│   [M] main.f90          │ ← Modified
│   [A] module.f90        │ ← Staged
│   [ ] utils.f90         │ ← Clean
│ [ ] README.md           │
│ [ ] Makefile            │
└─────────────────────────┘
```

### Status Bar

```
┌─────────────────────────────────────────┐
│ ctrl-b:fuss | main.f90    Ln 42, Col 10 │
│                           ↑             │
│                    Workspace indicator  │
└─────────────────────────────────────────┘
```

---

## 💡 Tips & Best Practices

### Workspace Organization

**✅ Good:**
- One workspace per project
- Keep related files in workspace directory
- Use descriptive project directory names
- Add active projects to favorites

**❌ Avoid:**
- Creating workspaces in home directory
- Mixing unrelated projects in one workspace
- Too many orphan tabs (reduces workspace benefits)

### Performance

**For large projects:**
- Workspaces with 50+ files work fine
- File tree may be slow with 1000+ files
- Consider excluding build directories from workspace

**Optimization:**
```bash
# Exclude build artifacts
echo "build/" >> .gitignore
echo "*.o" >> .gitignore

# fac respects .gitignore in file tree
```

### Backup Strategy

`fac` creates backups automatically when you:
- Quit without saving modified files
- Choose "don't save" at quit prompt

**Restore backups:**
- Backups in `.fac/backups/`
- Prompts on next workspace open
- Can view diff before restoring

### Team Collaboration

**If sharing workspaces:**
```bash
# Commit workspace config
git add .fac/workspace.json
git commit -m "Add fac workspace config"

# Teammates get same tab layout
```

**If keeping personal:**
```bash
# Add to .gitignore
echo ".fac/" >> .gitignore

# Each person has their own workspace state
```

---

## 🐛 Troubleshooting

### "Workspace.json" keeps resetting

**Cause:** Permissions issue or disk full

**Solution:**
```bash
# Check permissions
ls -la .fac/

# Should be writable by you
chmod -R u+w .fac/

# Check disk space
df -h .
```

### Tabs don't restore

**Cause:** Paths changed or files moved

**Solution:**
- Check warning messages on startup
- Files outside workspace may have moved
- Use `:e <path>` to re-add files
- Check `.fac/workspace.json` for old paths

### File tree doesn't show git info

**Cause:** Not a git repository

**Solution:**
```bash
# Initialize git if needed
git init

# Or just use file tree without git features
# It still shows files, just no status indicators
```

### Welcome menu shows old workspaces

**Cause:** Directories were moved/deleted

**Solution:**
- Try to open them (will auto-remove if gone)
- Or manually edit `~/.config/fac/recents.json`

### Workspace switch seems slow

**Cause:** Many tabs or large files

**Solution:**
- Normal for first load after switch
- Subsequent switches are cached
- Close unused tabs to speed up

---

## 🆘 Getting Help

### In-Editor Help

```
Ctrl-/    - Show help menu with keybindings
```

### Configuration Files

```
~/.config/fac/favorites.json    - Edit favorites manually
~/.config/fac/recents.json      - View/edit recent workspaces
<workspace>/.fac/workspace.json - Workspace state (advanced)
```

### Resetting Everything

**If things get weird:**
```bash
# Backup first!
cp -r ~/.config/fac ~/.config/fac.backup

# Remove config
rm -rf ~/.config/fac

# Workspaces remain, but favorites/recents reset
# Workspace state in .fac/ is preserved
```

**If a specific workspace is broken:**
```bash
cd /path/to/workspace

# Backup
cp -r .fac .fac.backup

# Remove workspace state
rm -rf .fac

# Next open will create fresh workspace
fac .
```

---

## 🎓 Advanced Topics

### Workspace Path Resolution

`fac` uses absolute paths internally:
```bash
fac .              → /home/user/project
fac ..             → /home/user
fac ~/code/app     → /home/user/code/app
```

Relative paths in `workspace.json` are relative to workspace root:
```json
{
  "filename": "src/main.f90"
  // Resolves to: /workspace-root/src/main.f90
}
```

### Multiple Panes (Advanced)

When you split windows (future feature):
- Each pane saved with coordinates
- Cursor position per pane
- All panes restore on workspace load

### Workspace JSON Schema

```json
{
  "version": "1.0",
  "workspace_path": "/absolute/path/to/workspace",
  "last_opened": "2025-01-05T10:30:00Z",
  "tabs": [
    {
      "filename": "relative/or/absolute/path.f90",
      "is_orphan": false,
      "modified": false,
      "panes": [
        {
          "x_start": 0.0,
          "y_start": 0.0,
          "x_end": 1.0,
          "y_end": 1.0,
          "filename": "path.f90",
          "cursor_line": 42,
          "cursor_column": 10,
          "viewport_line": 30,
          "viewport_column": 1
        }
      ],
      "active_pane": 1
    }
  ],
  "active_tab": 1,
  "fuss_mode": {
    "active": true,
    "width": 30
  }
}
```

---

## 🚀 Quick Reference Card

```
╔══════════════════════════════════════════════════════════════╗
║                   FAC WORKSPACE MODE                         ║
║                   Quick Reference                            ║
╚══════════════════════════════════════════════════════════════╝

OPEN WORKSPACE:
  fac .                        Current directory
  fac /path/to/workspace       Specific directory
  fac                          Welcome menu

NAVIGATION:
  Ctrl-O                       Fortress Navigator
  Ctrl-B                       Toggle file tree
  Ctrl-N / Ctrl-P             Next/previous tab

FAVORITES:
  Ctrl-O, navigate, 'f'        Add to favorites
  fac, press '8'               View favorites

SWITCHING:
  Ctrl-O, select dir, Enter    Switch workspace

FILE TREE:
  Ctrl-B                       Toggle
  ↑↓ or kj                    Navigate
  → Enter                      Open file
  g / u                        Stage/unstage (git)

SAVE & QUIT:
  :w                           Save file
  :q                           Quit (saves workspace)

ORPHAN TABS:
  Ctrl-O, navigate outside     Open external file
  (shows in grey)              Visual indicator

ERROR RECOVERY:
  Automatic!                   Missing files handled
  No manual steps              Corrupted JSON recovered
  Self-cleaning                Deleted workspaces removed
```

---

## 📚 See Also

- `PHASE7_COMPLETE.md` - Implementation details
- `WORKSPACE_ROADMAP.md` - Original design document
- `README.md` - Main fac documentation
- `:help` in fac - Built-in help

---

**Enjoy your new workspace powers!** 🎉

*Happy coding with fac!* 🚀
