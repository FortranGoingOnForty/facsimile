# Workspace Configuration Specification

**Version**: 1.0
**Last Updated**: 2025-01-05

---

## Overview

`workspace.json` stores the complete editor state for a workspace. It is located in `.fac/workspace.json` within the workspace root directory.

**Purpose**: Persist all editor state so users can resume exactly where they left off.

**Location**: `<workspace_root>/.fac/workspace.json`

---

## JSON Schema

### Root Object

```json
{
  "version": "1.0",
  "workspace_path": "/absolute/path/to/workspace",
  "last_opened": "2025-01-05T10:30:00Z",
  "tabs": [ /* array of tab objects */ ],
  "active_tab": 0,
  "fuss_mode": { /* fuss mode state */ }
}
```

### Field Descriptions

#### `version` (string, required)
- Schema version for forward/backward compatibility
- Format: "major.minor"
- Current: "1.0"
- Used to handle migration if schema changes

#### `workspace_path` (string, required)
- Absolute path to workspace root directory
- Used to verify workspace hasn't moved
- Example: "/home/user/projects/myapp"

#### `last_opened` (string, required)
- ISO 8601 timestamp of last workspace open
- Format: "YYYY-MM-DDTHH:MM:SSZ"
- Used for recents ordering
- Example: "2025-01-05T10:30:00Z"

#### `tabs` (array of objects, required)
- Array of tab objects
- Order matters (tab bar order)
- Empty array = no tabs (will create empty tab on load)
- See [Tab Object Schema](#tab-object-schema)

#### `active_tab` (integer, required)
- Index of currently active tab (0-based)
- Must be < length of tabs array
- Default: 0 if tabs exist, otherwise ignored

#### `fuss_mode` (object, required)
- File tree sidebar state
- See [Fuss Mode Object Schema](#fuss-mode-object-schema)

---

## Tab Object Schema

```json
{
  "label": "main.f90",
  "panes": [ /* array of pane objects */ ],
  "split_type": "none",
  "active_pane": 0
}
```

### Field Descriptions

#### `label` (string, required)
- Display label in tab bar
- Typically filename or basename
- Example: "main.f90"

#### `panes` (array of objects, required)
- Array of pane objects
- Single pane = no splits
- Multiple panes = split layout
- See [Pane Object Schema](#pane-object-schema)

#### `split_type` (string, required)
- Type of split in this tab
- Valid values:
  - `"none"` - single pane (default)
  - `"vertical"` - split left/right
  - `"horizontal"` - split top/bottom
- If split_type != "none", panes array must have 2+ items

#### `active_pane` (integer, required)
- Index of currently active pane (0-based)
- Must be < length of panes array
- Default: 0

---

## Pane Object Schema

```json
{
  "file": "src/main.f90",
  "cursor": {
    "line": 42,
    "column": 10
  },
  "viewport": {
    "line": 30,
    "column": 1
  },
  "modified": false
}
```

### Field Descriptions

#### `file` (string, required)
- Path to file in this pane
- **Workspace files**: Relative to workspace root (e.g., "src/main.f90")
- **Orphan files**: Absolute path (e.g., "/etc/hosts")
- Empty string = empty buffer

#### `cursor` (object, required)
- Cursor position in this file
- See [Cursor Object Schema](#cursor-object-schema)

#### `viewport` (object, required)
- Viewport scroll position
- See [Viewport Object Schema](#viewport-object-schema)

#### `modified` (boolean, required)
- Whether buffer has unsaved changes
- `true` = dirty buffer (check for backup file)
- `false` = clean buffer
- Used to detect if backup needed on restore

---

## Cursor Object Schema

```json
{
  "line": 42,
  "column": 10
}
```

### Field Descriptions

#### `line` (integer, required)
- Line number (1-based)
- Must be >= 1
- If > file line count, clamp to last line

#### `column` (integer, required)
- Column number (1-based)
- Must be >= 1
- If > line length, clamp to end of line

---

## Viewport Object Schema

```json
{
  "line": 30,
  "column": 1
}
```

### Field Descriptions

#### `line` (integer, required)
- Top line of viewport (1-based)
- Controls vertical scroll position
- Must be >= 1

#### `column` (integer, required)
- Left column of viewport (1-based)
- Controls horizontal scroll position
- Must be >= 1 (typically 1 unless horizontal scrolling)

---

## Fuss Mode Object Schema

```json
{
  "active": true,
  "width": 30
}
```

### Field Descriptions

#### `active` (boolean, required)
- Whether file tree is currently visible
- `true` = Ctrl-B was pressed, tree showing
- `false` = tree hidden

#### `width` (integer, required)
- Width of file tree pane (in columns)
- Typically 20-40
- Default: 30

---

## Example Workspace Configurations

### Example 1: Simple Single File

```json
{
  "version": "1.0",
  "workspace_path": "/home/user/projects/myapp",
  "last_opened": "2025-01-05T10:30:00Z",
  "tabs": [
    {
      "label": "main.f90",
      "panes": [
        {
          "file": "src/main.f90",
          "cursor": {"line": 1, "column": 1},
          "viewport": {"line": 1, "column": 1},
          "modified": false
        }
      ],
      "split_type": "none",
      "active_pane": 0
    }
  ],
  "active_tab": 0,
  "fuss_mode": {
    "active": false,
    "width": 30
  }
}
```

### Example 2: Multiple Tabs, Split Panes

```json
{
  "version": "1.0",
  "workspace_path": "/home/user/projects/kernel",
  "last_opened": "2025-01-05T14:45:00Z",
  "tabs": [
    {
      "label": "main.f90",
      "panes": [
        {
          "file": "src/main.f90",
          "cursor": {"line": 150, "column": 25},
          "viewport": {"line": 140, "column": 1},
          "modified": true
        },
        {
          "file": "src/module.f90",
          "cursor": {"line": 50, "column": 8},
          "viewport": {"line": 40, "column": 1},
          "modified": false
        }
      ],
      "split_type": "vertical",
      "active_pane": 0
    },
    {
      "label": "test.f90",
      "panes": [
        {
          "file": "tests/test.f90",
          "cursor": {"line": 10, "column": 1},
          "viewport": {"line": 1, "column": 1},
          "modified": false
        }
      ],
      "split_type": "none",
      "active_pane": 0
    }
  ],
  "active_tab": 0,
  "fuss_mode": {
    "active": true,
    "width": 35
  }
}
```

### Example 3: Empty Workspace

```json
{
  "version": "1.0",
  "workspace_path": "/home/user/new-project",
  "last_opened": "2025-01-05T09:00:00Z",
  "tabs": [
    {
      "label": "untitled",
      "panes": [
        {
          "file": "",
          "cursor": {"line": 1, "column": 1},
          "viewport": {"line": 1, "column": 1},
          "modified": false
        }
      ],
      "split_type": "none",
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

## Validation Rules

### On Load (Deserialization)

1. **Version Check**
   - If version != "1.0", attempt migration or warn user
   - Future-proof for schema changes

2. **Path Validation**
   - Verify workspace_path exists on disk
   - If moved, update workspace_path or prompt user

3. **File Validation**
   - For each pane's file path:
     - If relative, resolve against workspace_path
     - Check if file exists
     - If missing: log warning, skip pane (or show placeholder)
     - If orphan (absolute path outside workspace): load as orphan tab

4. **Cursor Bounds**
   - Clamp cursor line to file line count
   - Clamp cursor column to line length
   - Never crash on out-of-bounds cursor

5. **Viewport Bounds**
   - Clamp viewport to valid range
   - Ensure viewport shows cursor if possible

6. **Tab/Pane Indices**
   - Clamp active_tab to [0, tabs.length-1]
   - Clamp active_pane to [0, panes.length-1]

7. **Split Type Consistency**
   - If split_type = "none", panes array must have exactly 1 item
   - If split_type != "none", panes array must have 2+ items

### On Save (Serialization)

1. **Path Normalization**
   - Workspace files: convert to relative paths
   - Orphan files: use absolute paths
   - Remove trailing slashes

2. **Timestamp**
   - Update last_opened to current time (ISO 8601)

3. **Modified Flag**
   - Set modified = true for dirty buffers
   - Set modified = false for clean buffers

4. **Empty Workspace**
   - If no tabs, create one empty tab
   - Never save workspace with 0 tabs

---

## File Operations

### Creating Workspace

1. Check if `.fac/` directory exists
2. If not, create `.fac/` directory
3. Create default workspace.json with empty tab
4. Set workspace_path to absolute path of workspace root

### Loading Workspace

1. Read `.fac/workspace.json`
2. Parse JSON
3. Validate (see Validation Rules)
4. Restore editor state:
   - Create tabs from tabs array
   - Create panes with splits
   - Load files into buffers
   - Set cursor/viewport positions
   - Set active tab/pane
   - Restore fuss mode state

### Saving Workspace

1. Collect current editor state:
   - Tab list with labels
   - Pane layout per tab
   - File paths per pane
   - Cursor/viewport per pane
   - Active tab/pane indices
   - Fuss mode state
2. Serialize to JSON (see Validation Rules)
3. Write to `.fac/workspace.json`
4. Ensure atomic write (write temp file, then rename)

### Error Handling

- **File Not Found**: Log warning, skip that pane
- **Invalid JSON**: Fallback to empty workspace, backup corrupt file
- **Permission Denied**: Warn user, operate in read-only mode
- **Disk Full**: Warn user, attempt to save minimal state

---

## Migration Strategy (Future)

When schema version changes:

### Version 1.0 → 2.0 (Hypothetical)
```fortran
if (parsed_version == "1.0") then
    ! Migrate old format to new format
    call migrate_1_to_2(workspace)
end if
```

Keep backward compatibility for at least 2 major versions.

---

## Security Considerations

1. **Path Traversal**: Validate file paths don't escape workspace
2. **Symlink Handling**: Resolve symlinks before storing paths
3. **Permissions**: Respect file permissions, don't bypass
4. **Size Limits**: Limit workspace.json to reasonable size (< 1MB)

---

## Performance Considerations

1. **Lazy Loading**: Don't load all files immediately
   - Load active tab/pane first
   - Load other tabs on demand (when switching)
2. **Large Workspaces**: Handle 100+ tabs gracefully
3. **Atomic Writes**: Use temp file + rename to avoid corruption

---

## Testing Strategy

### Test Cases

1. **Empty workspace**: Create, save, load
2. **Single file**: Open, edit, save, load
3. **Multiple tabs**: Create 5 tabs, save, load, verify order
4. **Split panes**: Split vertical/horizontal, save, load
5. **Missing file**: Delete file, load workspace, verify warning
6. **Moved workspace**: Move directory, load, verify update
7. **Orphan tabs**: Open /etc/hosts, save, load, verify absolute path
8. **Cursor positions**: Edit various positions, save, load, verify exact positions
9. **Fuss mode**: Toggle Ctrl-B, save, load, verify state
10. **Corrupted JSON**: Malformed file, verify fallback

### Validation Tests

1. Out-of-bounds cursors
2. Invalid tab/pane indices
3. Split type mismatch (none with 2 panes)
4. Empty tabs array
5. Missing required fields

---

## Related Documents

- `WORKSPACE_VISION.md` - Overall design vision
- `fortress_integration.md` - Fortress integration details
- `config_spec.md` - User config files (favorites/recents)

---

## Appendix: JSON Schema (JSON Schema Format)

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "required": ["version", "workspace_path", "last_opened", "tabs", "active_tab", "fuss_mode"],
  "properties": {
    "version": {"type": "string", "pattern": "^[0-9]+\\.[0-9]+$"},
    "workspace_path": {"type": "string", "minLength": 1},
    "last_opened": {"type": "string", "format": "date-time"},
    "tabs": {
      "type": "array",
      "items": {"$ref": "#/definitions/tab"}
    },
    "active_tab": {"type": "integer", "minimum": 0},
    "fuss_mode": {"$ref": "#/definitions/fuss_mode"}
  },
  "definitions": {
    "tab": {
      "type": "object",
      "required": ["label", "panes", "split_type", "active_pane"],
      "properties": {
        "label": {"type": "string"},
        "panes": {
          "type": "array",
          "items": {"$ref": "#/definitions/pane"},
          "minItems": 1
        },
        "split_type": {"type": "string", "enum": ["none", "vertical", "horizontal"]},
        "active_pane": {"type": "integer", "minimum": 0}
      }
    },
    "pane": {
      "type": "object",
      "required": ["file", "cursor", "viewport", "modified"],
      "properties": {
        "file": {"type": "string"},
        "cursor": {"$ref": "#/definitions/position"},
        "viewport": {"$ref": "#/definitions/position"},
        "modified": {"type": "boolean"}
      }
    },
    "position": {
      "type": "object",
      "required": ["line", "column"],
      "properties": {
        "line": {"type": "integer", "minimum": 1},
        "column": {"type": "integer", "minimum": 1}
      }
    },
    "fuss_mode": {
      "type": "object",
      "required": ["active", "width"],
      "properties": {
        "active": {"type": "boolean"},
        "width": {"type": "integer", "minimum": 10, "maximum": 100}
      }
    }
  }
}
```

---

**End of Specification**
