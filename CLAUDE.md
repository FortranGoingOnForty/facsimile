# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Facsimile (`fac`) is a terminal text editor written in modern Fortran with VSCode-style keybindings. It uses a gap buffer for text storage and pure ANSI escape sequences for terminal rendering. Low-level terminal/PTY/regex/LSP-process work is done in small C wrappers bound via `iso_c_binding`.

## Build Commands

```bash
# Standard build (recommended) — produces ./fac
make

# Clean and rebuild
make clean && make

# Development build with comprehensive warnings
make dev

# Debug build with runtime checks
make debug

# Show detected compiler and flags
make info
```

The Makefile auto-detects the platform:
- **macOS arm64**: Uses gfortran-15 or flang-new from Homebrew
- **macOS Intel/Linux**: Uses standard gfortran

An fpm build also exists (`fpm build`, `fpm run -- [filename]`), driven by `fpm.toml`. The Makefile is the primary/release path; fpm is used by `run_tests.sh`. The version in `fpm.toml` is not kept in sync with `VERSION`.

**When adding a new module**: add it to the `SOURCES` list in the Makefile in dependency order (modules must be compiled before modules that `use` them — the `.NOTPARALLEL` directive enforces sequential builds). New C files go in `C_SOURCES`.

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

## Testing

```bash
# Full test suite: fpm build + fpm test, then Python/pexpect integration tests
./run_tests.sh

# Fortran unit tests only (test/*.f90, auto-discovered by fpm)
fpm test

# Integration tests only (requires: pip3 install pexpect)
python3 test/integration_test.py

# LSP module tests (compiles and runs tests/lsp/test_json.f90, test_lsp_init.f90)
make test-lsp

# Smoke-test LSP inside the editor (opens tests/lsp/sample.c, needs clangd)
make test-lsp-editor

# Manual escape-sequence inspection (what your terminal actually sends)
./test_keys.sh
./test_raw_keys.sh
```

Unit tests live in `test/` (fpm-discovered); LSP test programs, expect scripts, and sample files for various languages live in `tests/lsp/`.

## Running

```bash
./fac [filename]         # Open file
./fac <directory>        # Open directory in fortress navigator
./fac -w <dir>           # Workspace mode
./fac --version          # Show version
./fac --help             # Show help
```

Run with no arguments to get the welcome menu (fortress). Key bindings follow VSCode conventions: Ctrl-S save, Ctrl-Q quit, Ctrl-B file tree, Ctrl-F search, Ctrl-Z undo. Full list in README.md and `docs/KEYBINDINGS.md`.

## Architecture

### Core Data Flow

```
Input → input_handler_module → command_handler_module → buffer operations → renderer_module → Terminal
```

The main loop in `app/main.f90` polls keys, dispatches to `handle_key_command`, pumps LSP server messages, and re-renders.

### Key Modules

- **`src/buffer/text_buffer_module.f90`**: Gap buffer implementation for text storage. All operations maintain gap position for efficient insertions.

- **`src/editor_state_module.f90`**: Central state management. Contains `editor_state_t` with tabs, panes, cursors, LSP state, and UI panels. This is the "god object" that gets passed around.

- **`src/commands/command_handler_module.f90`**: Main command dispatch (~7,500 lines). Maps key inputs to editor actions.

- **`src/terminal/input_handler_module.f90`**: Raw keyboard input processing. Handles escape sequences, mouse events, and key combinations.

- **`src/terminal/renderer_module.f90`**: Screen rendering with ANSI escape sequences. Handles syntax highlighting, status bar, and split panes.

- **C wrappers**: `src/terminal/termios_wrapper.c` (raw mode), `src/terminal/pty_wrapper.c` + `vt100_grid.c` (integrated terminal panel), `src/utils/regex_wrapper.c` (POSIX regex), `src/utils/platform_wrapper.c`, `src/lsp/lsp_process_wrapper.c` (LSP server subprocess I/O).

### Subsystems

- **`src/fortress/`**: The welcome menu and full-screen directory navigator shown when `fac` is launched with no file or with a directory argument.
- **`src/workspace/`**: File tree ("fuss mode", Ctrl-B) with git status integration, plus config, favorites, recents, session/app state, and backup handling.
- **`src/ui/`**: Modal panels and prompts (command palette, search/replace, completion popup, diagnostics panel, hover/signature tooltips, integrated terminal panel, etc.). Each is its own module with show/handle/render entry points.
- **`src/undo/`**, **`src/clipboard/`**, **`src/navigation/`**: Undo stack, system clipboard + Emacs-style yank stack, and jump stack.
- **`src/syntax/syntax_highlighter_module.f90`**: Syntax highlighting.

### Pane/Tab Architecture

```
editor_state_t
  └── tabs[]           (multiple open files)
       └── panes[]     (split views of same file)
            ├── buffer (text content)
            ├── cursors[] (multiple cursor support)
            └── viewport (scroll position)
```

### UTF-8 Handling

All cursor positions use CHARACTER indices, not byte indices. The `utf8_module` provides conversion functions:
- `utf8_char_count()` - count characters in string
- `buffer_byte_to_char_col()` - convert byte position to character column
- `buffer_char_to_byte_col()` - convert character column to byte position

### LSP Integration

Located in `src/lsp/`. Communicates with language servers via JSON-RPC over stdio:
- `lsp_server_manager_module.f90` - Server lifecycle management
- `json_module.f90` - JSON parsing/generation
- `lsp_client_module.f90` - Request/response handling
- `server_detection_module.f90` / `server_installer_module.f90` - Detecting and installing servers per language

See `docs/LSP_GUIDE.md` for details.

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
