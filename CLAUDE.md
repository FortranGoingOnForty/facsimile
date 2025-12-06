# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Facsimile (`fac`) is a terminal text editor written in modern Fortran with VSCode-style keybindings. It uses a gap buffer for text storage and pure ANSI escape sequences for terminal rendering.

## Build Commands

```bash
# Standard build (recommended)
make

# Clean and rebuild
make clean && make

# Development build with comprehensive warnings
make dev

# Debug build with runtime checks
make debug

# Show compiler info
make info
```

The Makefile auto-detects the platform:
- **macOS arm64**: Uses gfortran-15 or flang-new from Homebrew
- **macOS Intel/Linux**: Uses standard gfortran

## Version Management

```bash
# Check current version
make version

# Bump versions
make bump-patch    # 0.9.1 -> 0.9.2
make bump-minor    # 0.9.1 -> 0.10.0
make bump-major    # 0.9.1 -> 1.0.0

# Full release build with checklist
make release
```

The `VERSION` file is the single source of truth. The Makefile auto-generates `src/version_module.f90`.

## Architecture

### Core Data Flow

```
Input → input_handler_module → command_handler_module → buffer operations → renderer_module → Terminal
```

### Key Modules

- **`src/buffer/text_buffer_module.f90`**: Gap buffer implementation for text storage. All operations maintain gap position for efficient insertions.

- **`src/editor_state_module.f90`**: Central state management. Contains `editor_state_t` with tabs, panes, cursors, LSP state, and UI panels. This is the "god object" that gets passed around.

- **`src/terminal/input_handler_module.f90`**: Raw keyboard input processing. Handles escape sequences, mouse events, and key combinations.

- **`src/terminal/renderer_module.f90`**: Screen rendering with ANSI escape sequences. Handles syntax highlighting, status bar, and split panes.

- **`src/commands/command_handler_module.f90`**: Main command dispatch. Maps key inputs to editor actions (~7000 lines).

- **`src/terminal/termios_wrapper.c`**: C wrapper for terminal raw mode via termios.

### Module Dependency Order

The Makefile's `SOURCES` list defines the required compilation order. Fortran modules must be compiled before modules that `use` them. The `.NOTPARALLEL` directive enforces sequential builds.

### UTF-8 Handling

All cursor positions use CHARACTER indices, not byte indices. The `utf8_module` provides conversion functions:
- `utf8_char_count()` - count characters in string
- `buffer_byte_to_char_col()` - convert byte position to character column
- `buffer_char_to_byte_col()` - convert character column to byte position

### Pane/Tab Architecture

```
editor_state_t
  └── tabs[]           (multiple open files)
       └── panes[]     (split views of same file)
            ├── buffer (text content)
            ├── cursors[] (multiple cursor support)
            └── viewport (scroll position)
```

### LSP Integration

Located in `src/lsp/`. Communicates with language servers via JSON-RPC over stdio:
- `lsp_server_manager_module.f90` - Server lifecycle management
- `json_module.f90` - JSON parsing/generation
- `lsp_client_module.f90` - Request/response handling

## Fortran-Specific Constraints

### Line Length Limit
Fortran has a 132-character line limit. Unicode characters (like box-drawing `═`) count as multiple bytes. Long lines must be split using `&` continuation:

```fortran
! Bad - will fail compilation
line = '═══════════════════════════════════════════════════════════════════'

! Good - split across lines
line = '═══════════════════════' // &
       '═══════════════════════' // &
       '═══════════════════════'
```

### Module Files
Compilation generates `.mod` files in the root directory. These are binary module interfaces, not source files.

## Distribution

The project is distributed via three channels:
- **Homebrew**: `homebrew-facsimile` repo with `facsimile.rb` formula
- **AUR**: Arch User Repository PKGBUILD
- **RPM**: Spec file at `~/rpmbuild/SPECS/facsimile.spec`

When releasing, update all three with the new version and SHA256 hash from the GitHub release tarball.

## Testing

```bash
# LSP module tests
make test-lsp

# Test LSP with editor
make test-lsp-editor

# Manual key testing
./keytest      # Basic key codes
./keytest_fac  # With fac's termios settings
```

## Running

```bash
./fac [filename]        # Open file
./fac --version         # Show version
./fac --help            # Show help
```

Key bindings follow VSCode conventions: Ctrl-S save, Ctrl-Q quit, Ctrl-B file tree, Ctrl-F search, Ctrl-Z undo.
