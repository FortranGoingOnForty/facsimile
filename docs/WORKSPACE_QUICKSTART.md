# Workspace Mode - Quick Start Guide

Ready to start implementing? Follow this guide.

---

## Before You Begin

### Read These Documents (in order)
1. **WORKSPACE_VISION.md** - Understand the vision and key decisions
2. **WORKSPACE_ROADMAP.md** - Full implementation plan
3. **WORKSPACE_TASKS.md** - Task tracker (update as you go)

### Prerequisites
- All existing fac features working
- Clean build: `make clean && make`
- All tests passing
- Git working tree clean (commit current work)

---

## Phase 0: Planning & Design (START HERE)

**Goal**: Create specification documents before writing code

### Step 1: Create Directory Structure
```bash
mkdir -p docs
# (already done if you're reading this)
```

### Step 2: Create workspace_spec.md
Document the workspace.json format in detail:
- JSON schema
- Field descriptions
- Example workspaces
- Validation rules

**Template**:
```markdown
# Workspace Configuration Specification

## Overview
workspace.json stores the complete editor state for a workspace.

## Schema
...

## Example
...

## Validation
...
```

### Step 3: Create fortress_integration.md
Document how Fortress modules integrate:
- Which modules to copy from ../fortress
- How to adapt them for fac
- Module dependencies
- API contracts

**Template**:
```markdown
# Fortress Integration Plan

## Modules to Copy
1. filesystem/directory_module.f90
   - Functions: ...
   - Changes needed: ...

## Integration Points
...
```

### Step 4: Create config_spec.md
Document config file formats:
- favorites.json format
- recents.json format
- backup metadata format
- File locations (XDG paths)

**Template**:
```markdown
# Configuration Files Specification

## favorites.json
Location: ~/.config/fac/favorites.json
Format:
...

## recents.json
...
```

### Step 5: Review & Finalize
- Review all specs
- Check for gaps
- Verify against WORKSPACE_VISION.md
- Get approval before coding

**Phase 0 Complete When**:
- ✅ All spec documents created
- ✅ Specs are detailed and clear
- ✅ No open questions remain
- ✅ Ready to write code

---

## Phase 1: Fortress Navigator

**Goal**: Build basic dual-pane file/directory navigator

### Step 1: Copy Fortress Code
```bash
# From fortress repo
cd ../fortress
ls -la app/
# Identify modules to copy

cd ../facsimile
mkdir -p src/fortress/{filesystem,terminal,ui}

# Copy files (example)
cp ../fortress/app/filesystem/*.f90 src/fortress/filesystem/
cp ../fortress/app/terminal/*.f90 src/fortress/terminal/
cp ../fortress/app/ui/*.f90 src/fortress/ui/
```

### Step 2: Adapt for fac
- Remove main program
- Change module names if conflicts
- Adapt to fac's terminal API
- Remove fortress-specific features (git, fzf for now)

### Step 3: Create Navigator Module
```fortran
! src/fortress/ui/navigator_module.f90
module navigator_module
    implicit none
contains
    subroutine open_fortress_navigator(selected_path, selection_type)
        ! Returns selected path and whether it's a file or directory
    end subroutine
end module
```

### Step 4: Add to Build
```makefile
# Makefile
SOURCES = ... \
          src/fortress/filesystem/directory_module.f90 \
          src/fortress/terminal/fortress_render_module.f90 \
          src/fortress/ui/navigator_module.f90 \
          ...
```

### Step 5: Add Ctrl-O Keybinding
```fortran
! src/commands/command_handler_module.f90
else if (key_input == 'CTRL_O') then
    call handle_fortress_navigator(editor, buffer)
end if
```

### Step 6: Test
```bash
make clean && make
./fac test.txt
# Press Ctrl-O
# Should see dual-pane navigator
# Navigate, select, verify return
```

**Phase 1 Complete When**:
- ✅ Ctrl-O opens Fortress navigator
- ✅ Can navigate filesystem
- ✅ Selecting directory returns path
- ✅ Selecting file returns path
- ✅ ESC cancels
- ✅ No regressions in existing features

---

## Phase 2: Workspace Detection

**Goal**: Detect/create workspace directories and configs

### Step 1: Create Workspace Module
```fortran
! src/workspace/workspace_module.f90
module workspace_module
    implicit none

    type :: workspace_t
        character(len=:), allocatable :: path
        logical :: is_workspace_mode
    end type

contains
    subroutine workspace_init(workspace, path)
        ! Detect or create workspace
    end subroutine

    function workspace_exists(path) result(exists)
        ! Check for .fac/workspace.json
    end function
end module
```

### Step 2: Update main.f90
```fortran
! app/main.f90
type(workspace_t) :: workspace

if (argc == 0) then
    ! Open Fortress welcome (Phase 5)
else if (is_directory(arg)) then
    call workspace_init(workspace, arg)
else
    ! Single file mode
end if
```

### Step 3: Create .fac/ Structure
```fortran
subroutine create_workspace_structure(path)
    ! Create .fac/ directory
    ! Create .fac/workspace.json
    ! Create .fac/backups/
end subroutine
```

### Step 4: Test
```bash
cd /tmp/test-workspace
../facsimile/fac .
# Should create .fac/workspace.json
ls -la .fac/
```

**Phase 2 Complete When**:
- ✅ `fac .` creates workspace
- ✅ `.fac/workspace.json` created
- ✅ `fac file.txt` still works (single-file mode)
- ✅ Command line parsing correct

---

## Development Guidelines

### Commit Often
Commit after each major task:
```bash
git add .
git commit -m "Implement fortress navigator dual-pane display"
```

### Test After Each Change
```bash
make clean && make
./fac test.txt
# Test new feature
# Test existing features (regression check)
```

### Update Task Tracker
Mark tasks complete in WORKSPACE_TASKS.md as you go.

### Debug Logging
Add debug logging for complex logic:
```fortran
! DEBUG: Remove before final release
open(unit=99, file='/tmp/fac_workspace_debug.txt', position='append')
write(99, '(A,A)') 'Loading workspace: ', trim(path)
close(99)
```

### Handle Errors Gracefully
```fortran
if (error) then
    ! Show error to user
    ! Fallback to safe state
    ! Don't crash
end if
```

### Keep Backwards Compatibility
Always test single-file mode:
```bash
./fac README.md
# Should work exactly as before
```

---

## When Things Go Wrong

### Build Fails
```bash
make clean
# Check for syntax errors
# Check module dependencies (order matters)
# Verify all files in SOURCES
make
```

### Feature Not Working
1. Add debug logging
2. Check file permissions
3. Verify paths are correct
4. Test with simple case first
5. Compare with spec documents

### Cursor Bugs Appear
1. Check if new code modified cursor state
2. Verify sync_editor_to_pane calls
3. Test with single pane/tab first
4. Add debug output for cursor positions

### State Not Persisting
1. Verify workspace.json being written
2. Check file permissions
3. Verify JSON format is valid
4. Test load/save separately

---

## Communication

### Document Decisions
If you make a design decision not in the specs:
1. Add it to WORKSPACE_VISION.md
2. Note why in commit message
3. Update WORKSPACE_ROADMAP.md if it affects plan

### Ask Questions
If unclear about a requirement:
1. Check WORKSPACE_VISION.md first
2. Check WORKSPACE_ROADMAP.md
3. Ask before proceeding

---

## Completion Checklist

### Each Phase
- [ ] All tasks completed
- [ ] Tests passing
- [ ] No regressions
- [ ] Code committed
- [ ] Documentation updated
- [ ] WORKSPACE_TASKS.md updated

### Final Release (Phase 7)
- [ ] All phases complete
- [ ] Performance acceptable
- [ ] User testing done
- [ ] Documentation complete
- [ ] README.md updated
- [ ] --help updated
- [ ] No debug logging
- [ ] Clean git history
- [ ] Version bumped
- [ ] Tagged release

---

## Useful Commands

```bash
# Clean build
make clean && make

# Test single file mode
./fac README.md

# Test workspace mode
cd /tmp/test && ../facsimile/fac .

# Check for .fac directory
ls -la .fac/

# View workspace config
cat .fac/workspace.json | python -m json.tool

# Debug
tail -f /tmp/fac_workspace_debug.txt

# Remove workspace (clean slate)
rm -rf .fac/
```

---

## Ready?

1. ✅ Read all documentation
2. ✅ Understand the vision
3. ✅ Environment ready
4. ✅ Start Phase 0!

Let's build this thing! 🚀
