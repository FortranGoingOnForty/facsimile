# Configuration Files Specification

**Version**: 1.0
**Last Updated**: 2025-01-05

---

## Overview

This document specifies the format and location of fac's user configuration files. These files are stored in the user's config directory and track global state across all workspaces.

**Purpose**:
- Track recently opened workspaces
- Store user-marked favorite workspaces
- Manage backup metadata for dirty buffers

**Location**: Follows XDG Base Directory Specification
- **Linux/macOS**: `~/.config/fac/`
- **Fallback**: `~/.fac/` if XDG not available

---

## settings.json

Added in 0.19.0. User preferences, as a **flat** JSON object with dotted keys:

```json
{
  "ai.enabled": false,
  "ai.model": "qwen2.5-coder:1.5b-base",
  "ai.remote.enabled": false
}
```

Flat dotted keys are deliberate: the reader stays a line scanner, so settings
need neither a nested parser nor `json_module`. Nesting the objects would buy
nothing and cost a dependency.

- Missing file is the normal first-run case, not an error. Every key falls back
  to a default supplied at its call site, so there is no defaults table to drift
  out of step with the code that reads it.
- A file that cannot be parsed is moved to `settings.json.corrupted` and
  defaults are used, rather than being half-read and then saved over.
- Writes go to `settings.json.tmp` and are `rename()`d into place, per the
  atomic-write requirement below.

Keys are documented with their feature. See `docs/AI_COMPLETION.md` for the
`ai.*` group.

| Key | Default | Meaning |
|---|---|---|
| `tabs.group_hover_preview` | `true` | Preview a tab group's members on hover. Off stops the editor asking the terminal to report every pointer movement |
| `terminal.height_percent` | `30` | How tall the terminal panel opens, as a percentage of the screen. Only a starting point: resizing it stores the new size per workspace, which then wins |

## Directory Structure

```
~/.config/fac/
├── favorites.json           # User-marked favorite workspaces
├── recents.json            # Auto-tracked recent workspaces
└── README.txt              # Optional: explain what this directory is
```

---

## favorites.json Specification

### Purpose
Store user-manually marked favorite workspaces for quick access via Fortress welcome menu.

### Location
`~/.config/fac/favorites.json`

### Schema

```json
{
  "version": "1.0",
  "favorites": [
    {
      "path": "/absolute/path/to/workspace",
      "label": "My Project",
      "added": "2025-01-05T10:30:00Z",
      "pinned": false
    }
  ]
}
```

### Field Descriptions

#### Root Object

##### `version` (string, required)
- Schema version for compatibility
- Format: "major.minor"
- Current: "1.0"

##### `favorites` (array of objects, required)
- Array of favorite workspace objects
- Order matters (display order in Fortress)
- Can be empty

#### Favorite Object

##### `path` (string, required)
- Absolute path to workspace root
- Must be a directory (verified on load)
- Example: "/home/user/projects/myapp"

##### `label` (string, required)
- User-friendly display name
- Defaults to directory basename
- User can customize
- Example: "My Fortran Project"

##### `added` (string, required)
- ISO 8601 timestamp when favorite was added
- Format: "YYYY-MM-DDTHH:MM:SSZ"
- Used for sorting (if needed)
- Example: "2025-01-05T10:30:00Z"

##### `pinned` (boolean, required)
- Whether this favorite should stay at top
- Pinned favorites shown first (Phase 7 feature)
- Default: false

### Example favorites.json

```json
{
  "version": "1.0",
  "favorites": [
    {
      "path": "/home/user/projects/facsimile",
      "label": "fac Editor",
      "added": "2025-01-01T08:00:00Z",
      "pinned": true
    },
    {
      "path": "/home/user/projects/fortress",
      "label": "Fortress Navigator",
      "added": "2025-01-02T09:15:00Z",
      "pinned": false
    },
    {
      "path": "/home/user/Documents/thesis",
      "label": "PhD Thesis",
      "added": "2025-01-03T14:30:00Z",
      "pinned": false
    }
  ]
}
```

### Operations

#### Add Favorite
- **Trigger**: Press 'f' in Fortress navigator
- **Action**: Add current directory to favorites list
- **Duplicate Check**: If path already exists, don't add again (show message)
- **Label**: Default to basename, allow edit later (Phase 7)

#### Remove Favorite
- **Trigger**: Press 'r' or 'x' in Fortress favorites view
- **Action**: Remove selected favorite from list
- **Confirmation**: Prompt "Remove favorite? [y/n]"

#### Reorder Favorites (Phase 7)
- **Trigger**: Drag/drop or move commands
- **Action**: Change order in array
- **Persistence**: Save immediately

---

## recents.json Specification

### Purpose
Automatically track recently opened workspaces for quick access. Similar to "Recent Files" in most editors.

### Location
`~/.config/fac/recents.json`

### Schema

```json
{
  "version": "1.0",
  "max_recents": 20,
  "recents": [
    {
      "path": "/absolute/path/to/workspace",
      "label": "Auto-generated label",
      "last_opened": "2025-01-05T14:45:00Z",
      "open_count": 5
    }
  ]
}
```

### Field Descriptions

#### Root Object

##### `version` (string, required)
- Schema version
- Current: "1.0"

##### `max_recents` (integer, required)
- Maximum number of recents to track
- Default: 20
- When exceeded, oldest entries are removed
- User configurable (Phase 7)

##### `recents` (array of objects, required)
- Array of recent workspace objects
- Sorted by `last_opened` (most recent first)
- Can be empty

#### Recent Object

##### `path` (string, required)
- Absolute path to workspace root
- Example: "/home/user/projects/myapp"

##### `label` (string, required)
- Display name (typically basename)
- Auto-generated from path
- Example: "myapp"

##### `last_opened` (string, required)
- ISO 8601 timestamp of last open
- Format: "YYYY-MM-DDTHH:MM:SSZ"
- Updated every time workspace is opened
- Example: "2025-01-05T14:45:00Z"

##### `open_count` (integer, required)
- Number of times this workspace has been opened
- Used for statistics (Phase 7)
- Incremented on each open
- Default: 1

### Example recents.json

```json
{
  "version": "1.0",
  "max_recents": 20,
  "recents": [
    {
      "path": "/home/user/projects/facsimile",
      "label": "facsimile",
      "last_opened": "2025-01-05T15:30:00Z",
      "open_count": 47
    },
    {
      "path": "/home/user/projects/fortress",
      "label": "fortress",
      "last_opened": "2025-01-05T10:15:00Z",
      "open_count": 23
    },
    {
      "path": "/tmp/scratch",
      "label": "scratch",
      "last_opened": "2025-01-04T16:45:00Z",
      "open_count": 3
    }
  ]
}
```

### Operations

#### Add/Update Recent
- **Trigger**: Every time a workspace is opened
- **Action**:
  1. If path exists in recents: update `last_opened`, increment `open_count`
  2. If path doesn't exist: add new entry
  3. Re-sort by `last_opened` (most recent first)
  4. If count > `max_recents`: remove oldest entry

#### Clear Recents
- **Trigger**: User command (Phase 7)
- **Action**: Empty recents array
- **File**: Keep file, just empty the array

#### Remove Recent
- **Trigger**: Press 'r' or 'x' in Fortress recents view
- **Action**: Remove selected entry
- **No confirmation**: Recents are auto-tracked, safe to remove

---

## Backup Metadata Specification

### Purpose
Track backup files for dirty buffers when user quits without saving.

### Location
`<workspace_root>/.fac/backups/.backup-metadata.json`

### Schema

```json
{
  "version": "1.0",
  "backups": [
    {
      "original_file": "src/main.f90",
      "backup_file": "src/main.f90.bak",
      "timestamp": "2025-01-05T10:30:00Z",
      "reason": "quit_without_save"
    }
  ]
}
```

### Field Descriptions

#### Root Object

##### `version` (string, required)
- Schema version
- Current: "1.0"

##### `backups` (array of objects, required)
- Array of backup entries
- Can be empty
- Cleaned up after restore or ignore

#### Backup Object

##### `original_file` (string, required)
- Relative path to original file (within workspace)
- Example: "src/main.f90"

##### `backup_file` (string, required)
- Relative path to backup file
- Typically `{original_file}.bak`
- Example: "src/main.f90.bak"

##### `timestamp` (string, required)
- ISO 8601 timestamp when backup was created
- Format: "YYYY-MM-DDTHH:MM:SSZ"
- Example: "2025-01-05T10:30:00Z"

##### `reason` (string, required)
- Why backup was created
- Values:
  - `"quit_without_save"` - User quit with dirty buffer
  - `"crash"` - Editor crashed (Phase 7)
  - `"switch_workspace"` - Switched workspace with dirty buffer (Phase 6)

### Example .backup-metadata.json

```json
{
  "version": "1.0",
  "backups": [
    {
      "original_file": "src/main.f90",
      "backup_file": "src/main.f90.bak",
      "timestamp": "2025-01-05T10:30:00Z",
      "reason": "quit_without_save"
    },
    {
      "original_file": "tests/test.f90",
      "backup_file": "tests/test.f90.bak",
      "timestamp": "2025-01-05T10:30:00Z",
      "reason": "quit_without_save"
    }
  ]
}
```

### Operations

#### Create Backup
- **Trigger**: Quit with dirty buffers, user chooses 'n' (don't save)
- **Action**:
  1. For each dirty buffer, copy to `{filename}.bak`
  2. Add entry to `.backup-metadata.json`
  3. Write metadata atomically

#### Detect Backups
- **Trigger**: Open workspace
- **Action**:
  1. Check if `.fac/backups/.backup-metadata.json` exists
  2. If exists and has entries, prompt user

#### Restore Backup
- **Trigger**: User selects 'r' (restore) in backup prompt
- **Action**:
  1. Copy backup file to original location (overwrite)
  2. Remove backup file
  3. Remove entry from metadata
  4. Load file in editor

#### Ignore Backup
- **Trigger**: User selects 'i' (ignore) in backup prompt
- **Action**:
  1. Keep backup file (don't delete)
  2. Remove entry from metadata
  3. Load original file in editor

#### Show Diff
- **Trigger**: User selects 'd' (diff) in backup prompt
- **Action**:
  1. Run `diff` command (or internal diff)
  2. Display in pager or split view
  3. Return to prompt (user can then choose r/i)

#### Cleanup
- **Trigger**: After restore or ignore all backups
- **Action**:
  1. If metadata.backups array is empty, delete `.backup-metadata.json`
  2. If `.fac/backups/` directory is empty, delete directory

---

## File Operations

### Creating Config Directory

**On first run**:
1. Check if `~/.config/fac/` exists
2. If not, create directory with 0755 permissions
3. Create empty `favorites.json` and `recents.json`
4. Optionally create `README.txt` explaining purpose

**Fortran Example**:
```fortran
subroutine ensure_config_directory()
    character(len=:), allocatable :: config_path
    logical :: exists

    config_path = get_config_directory()  ! Returns ~/.config/fac/
    inquire(file=config_path, exist=exists)

    if (.not. exists) then
        call create_directory(config_path)
        call create_empty_config_files(config_path)
    end if
end subroutine
```

### Atomic Writes

**Critical**: Always use atomic writes for config files to avoid corruption.

**Pattern**:
1. Write to temporary file: `favorites.json.tmp`
2. Verify write succeeded (check iostat)
3. Rename temp to final: `rename(tmp, final)`
4. Rename is atomic on POSIX systems

**Fortran Example**:
```fortran
subroutine save_favorites(favorites)
    type(favorites_t), intent(in) :: favorites
    character(len=:), allocatable :: path, tmp_path
    integer :: iostat

    path = get_config_directory() // 'favorites.json'
    tmp_path = path // '.tmp'

    ! Write to temp file
    call write_json_to_file(tmp_path, favorites, iostat)

    if (iostat == 0) then
        ! Rename temp to final (atomic)
        call rename_file(tmp_path, path)
    else
        ! Handle error
        call delete_file(tmp_path)
    end if
end subroutine
```

### Reading Config Files

**Strategy**: Graceful fallback on errors

```fortran
subroutine load_favorites(favorites)
    type(favorites_t), intent(out) :: favorites
    character(len=:), allocatable :: path
    logical :: exists
    integer :: iostat

    path = get_config_directory() // 'favorites.json'
    inquire(file=path, exist=exists)

    if (.not. exists) then
        ! Create empty favorites
        call initialize_empty_favorites(favorites)
        call save_favorites(favorites)
        return
    end if

    call parse_json_file(path, favorites, iostat)

    if (iostat /= 0) then
        ! Corrupted file: backup and recreate
        call backup_corrupt_file(path)
        call initialize_empty_favorites(favorites)
        call save_favorites(favorites)
    end if
end subroutine
```

---

## Error Handling

### File Not Found
- **Action**: Create with default/empty content
- **Log**: No error message (expected on first run)

### Permission Denied
- **Action**: Warn user, run in read-only mode
- **Message**: "Cannot write to config directory. Favorites/recents will not persist."

### Corrupted JSON
- **Action**: Backup corrupt file, create new empty config
- **Backup**: Rename to `{file}.corrupted.{timestamp}`
- **Log**: Warning message with backup location

### Disk Full
- **Action**: Warn user, skip update
- **Message**: "Cannot save config: disk full"

### Invalid Data
- **Action**: Skip invalid entries, log warning
- **Example**: Path doesn't exist → skip that favorite
- **Message**: "Skipped invalid favorite: /nonexistent/path"

---

## Validation Rules

### On Load

#### favorites.json
1. Verify `version` is supported
2. For each favorite:
   - Verify `path` is absolute
   - Verify `path` exists (if not, skip with warning)
   - Verify `added` is valid ISO 8601
   - Clamp `pinned` to boolean

#### recents.json
1. Verify `version` is supported
2. Verify `max_recents` is positive integer (default 20 if invalid)
3. For each recent:
   - Verify `path` is absolute
   - If `path` doesn't exist, remove entry (cleaned up automatically)
   - Verify `last_opened` is valid ISO 8601
   - Verify `open_count` is non-negative integer

#### .backup-metadata.json
1. Verify `version` is supported
2. For each backup:
   - Verify `backup_file` exists (if not, skip entry)
   - Verify `timestamp` is valid ISO 8601
   - Verify `reason` is valid enum value

### On Save

#### Path Normalization
- Always store absolute paths (no relative paths)
- Resolve symlinks before storing
- Remove trailing slashes

#### Timestamp Format
- Always use ISO 8601: "YYYY-MM-DDTHH:MM:SSZ"
- Always use UTC timezone (Z suffix)

#### Array Limits
- `favorites`: No hard limit (but UX degrades after ~50)
- `recents`: Enforce `max_recents` limit

---

## Configuration Directory Detection

### XDG Base Directory Specification

**Linux/macOS**:
1. Check `$XDG_CONFIG_HOME` environment variable
2. If set: use `$XDG_CONFIG_HOME/fac/`
3. If not set: use `~/.config/fac/`

**Fallback**:
- If `~/.config/` doesn't exist and can't be created: use `~/.fac/`

**Windows (Future)**:
- Use `%APPDATA%\fac\` (e.g., `C:\Users\User\AppData\Roaming\fac\`)

**Fortran Example**:
```fortran
function get_config_directory() result(path)
    character(len=:), allocatable :: path
    character(len=1024) :: xdg_config_home, home
    integer :: status

    ! Try XDG_CONFIG_HOME
    call get_environment_variable('XDG_CONFIG_HOME', xdg_config_home, status=status)
    if (status == 0 .and. len_trim(xdg_config_home) > 0) then
        path = trim(xdg_config_home) // '/fac/'
        return
    end if

    ! Try ~/.config/fac/
    call get_environment_variable('HOME', home, status=status)
    if (status == 0) then
        path = trim(home) // '/.config/fac/'
        return
    end if

    ! Fallback to ~/.fac/
    path = trim(home) // '/.fac/'
end function
```

---

## Migration Strategy

### Version 1.0 → 2.0 (Hypothetical)

If schema changes in future:

```fortran
subroutine load_favorites(favorites)
    type(favorites_t), intent(out) :: favorites
    character(len=64) :: version
    integer :: iostat

    ! Parse JSON and get version
    call parse_json_version(path, version, iostat)

    if (version == '1.0') then
        call load_favorites_v1(favorites)
    else if (version == '2.0') then
        call load_favorites_v2(favorites)
    else
        ! Unknown version: try to load as latest, or fail gracefully
        call load_favorites_v2(favorites)
    end if
end subroutine

subroutine migrate_favorites_v1_to_v2(favorites_v1, favorites_v2)
    ! Convert old format to new format
end subroutine
```

**Backward Compatibility**:
- Keep support for old versions for at least 2 major releases
- Automatically migrate on load, save in new format

---

## Security Considerations

### Path Validation
- **Verify paths are absolute**: Reject relative paths
- **Prevent path traversal**: No `..` components in stored paths
- **Resolve symlinks**: Store canonical paths
- **Don't follow untrusted symlinks**: Check ownership

### File Permissions
- **Config directory**: 0755 (rwxr-xr-x)
- **Config files**: 0644 (rw-r--r--)
- **Backup files**: 0644 (rw-r--r--)
- **Respect umask**: Use system default permissions

### Arbitrary Code Execution
- **No eval**: Don't execute any code from config files
- **JSON only**: Only parse JSON, no shell commands
- **Sanitize paths**: Validate before using in file operations

---

## Performance Considerations

### Lazy Loading
- Don't load favorites/recents until needed (Fortress welcome menu)
- Cache in memory during session
- Only write on changes

### Update Frequency
- **recents.json**: Update once per workspace open (not every save)
- **favorites.json**: Update only when user adds/removes favorite
- **Batch updates**: If multiple changes, write once

### File Size Limits
- **favorites.json**: Reasonable limit ~1000 entries (~100KB)
- **recents.json**: Hard limit `max_recents` (default 20)
- **backup-metadata.json**: Limit ~100 backups per workspace (~10KB)

---

## Testing Strategy

### Test Cases

#### favorites.json
1. Create empty favorites file on first run
2. Add favorite, verify saved correctly
3. Remove favorite, verify saved correctly
4. Load favorites on startup
5. Handle corrupted JSON (backup and recreate)
6. Handle nonexistent paths (skip with warning)
7. Handle duplicate paths (don't add duplicate)

#### recents.json
1. Create empty recents on first run
2. Open workspace, verify added to recents
3. Open same workspace again, verify `last_opened` updated
4. Open `max_recents + 1` workspaces, verify oldest removed
5. Load recents on startup
6. Handle corrupted JSON (backup and recreate)
7. Verify sorting (most recent first)

#### backup-metadata.json
1. Quit with dirty buffer, verify backup created
2. Open workspace, detect backup, prompt user
3. Restore backup, verify file restored
4. Ignore backup, verify metadata cleaned up
5. Show diff, verify diff displayed
6. Handle missing backup file (skip entry)
7. Cleanup empty metadata file

### Edge Cases
- Config directory doesn't exist (create it)
- Config directory not writable (read-only mode)
- Disk full during save (handle gracefully)
- Home directory not set (fallback to /tmp?)
- JSON with BOM (byte order mark) - handle or reject
- JSON with comments (non-standard) - reject

---

## Implementation Notes

### JSON Parsing

**Option 1**: Use external library (e.g., json-fortran)
- **Pros**: Robust, well-tested, full JSON support
- **Cons**: External dependency

**Option 2**: Write simple parser for our specific format
- **Pros**: No dependencies, full control
- **Cons**: More work, potential bugs

**Recommendation**: Use json-fortran (fpm package)

**Alternative**: If no dependencies allowed, write minimal parser for our specific schema (we only need basic key-value parsing, not full JSON)

### Timestamp Generation

**Fortran 2003+**:
```fortran
use iso_fortran_env, only: int64

function get_iso8601_timestamp() result(timestamp)
    character(len=24) :: timestamp
    integer :: values(8)

    call date_and_time(values=values)

    write(timestamp, '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2,"Z")') &
        values(1), values(2), values(3), values(5), values(6), values(7)
end function
```

### Path Operations

Use existing fac modules:
- `src/fortress/filesystem/path_utils_module.f90` - Path manipulation
- `src/fortress/filesystem/directory_module.f90` - Directory operations

Or POSIX C bindings:
```fortran
interface
    function c_realpath(path, resolved) bind(C, name="realpath")
        use iso_c_binding
        type(c_ptr), value :: path
        type(c_ptr), value :: resolved
        type(c_ptr) :: c_realpath
    end function
end interface
```

---

## Related Documents

- `WORKSPACE_VISION.md` - Overall design vision
- `workspace_spec.md` - workspace.json format
- `fortress_integration.md` - Fortress integration details
- `WORKSPACE_ROADMAP.md` - Implementation phases

---

## Appendix: JSON Examples

### Empty Configuration (First Run)

**favorites.json**:
```json
{
  "version": "1.0",
  "favorites": []
}
```

**recents.json**:
```json
{
  "version": "1.0",
  "max_recents": 20,
  "recents": []
}
```

### Populated Configuration

**favorites.json**:
```json
{
  "version": "1.0",
  "favorites": [
    {
      "path": "/home/user/projects/facsimile",
      "label": "fac Editor",
      "added": "2025-01-01T08:00:00Z",
      "pinned": true
    },
    {
      "path": "/home/user/projects/fortress",
      "label": "Fortress",
      "added": "2025-01-02T09:15:00Z",
      "pinned": false
    }
  ]
}
```

**recents.json**:
```json
{
  "version": "1.0",
  "max_recents": 20,
  "recents": [
    {
      "path": "/home/user/projects/facsimile",
      "label": "facsimile",
      "last_opened": "2025-01-05T15:30:00Z",
      "open_count": 47
    }
  ]
}
```

---

**End of Specification**
