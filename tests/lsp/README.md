# LSP Tests

This directory contains tests for the Language Server Protocol (LSP) integration in the fac editor.

## Test Files

### Language-Specific Samples
- `sample.c` - C/C++ test file with completion and hover test points
- `sample_errors.c` - C file with intentional syntax errors for testing diagnostics
- `sample_python.py` - Python test file with class/module completion tests
- `sample_rust.rs` - Rust test file with struct and trait tests
- `sample_typescript.ts` - TypeScript test file with interface and generic tests

### Unit Tests
- `test_json.f90` - Tests the JSON parser implementation
- `test_lsp_init.f90` - Tests LSP server initialization and communication

### Integration Tests (Expect Scripts)
- `test_initial_diagnostics.exp` - Tests initial diagnostics when opening a file
- `test_realtime_diagnostics.exp` - Tests real-time diagnostic updates
- `test_didsave_diagnostics.exp` - Tests diagnostics after save (Ctrl+S)
- `test_diagnostics_debug.exp` - Debug version with verbose output

## Running Tests

### Test JSON Parser
```bash
make test-lsp
```

### Test LSP in Editor
```bash
make test-lsp-editor
```

### Manual Testing
Open a supported file type in the editor:
```bash
./fac tests/lsp/sample.c   # Tests C/C++ with clangd
./fac tests/lsp/sample.py  # Tests Python with pylsp
./fac tests/lsp/sample.rs  # Tests Rust with rust-analyzer
```

## Supported Languages

The editor currently supports LSP for:
- C/C++ (clangd)
- Python (pylsp)
- Rust (rust-analyzer)
- Go (gopls)
- TypeScript/JavaScript (typescript-language-server)
- Fortran (fortls)

## Requirements

For LSP to work, you need the corresponding language servers installed:
- C/C++: `brew install llvm` (provides clangd)
- Python: `pip install python-lsp-server`
- Rust: `rustup component add rust-analyzer`
- Go: `go install golang.org/x/tools/gopls@latest`
- TypeScript: `npm install -g typescript typescript-language-server`
- Fortran: `pip install fortls`

## Current Features
- ✅ Automatic server startup when opening supported files
- ✅ Server initialization handshake with capability detection
- ✅ textDocument/didOpen notifications
- ✅ textDocument/didChange notifications with debouncing (500ms)
- ✅ textDocument/didSave notifications on file save
- ✅ Code completion (`Ctrl+Space`) with popup UI
- ✅ Hover information (`Ctrl+H`) with tooltip display
- ✅ Diagnostics display with error/warning/info/hint markers
- ✅ Diagnostics panel (`Ctrl+Shift+D`) showing all issues
- ✅ Real-time document synchronization
- ✅ Multi-language support (C/C++, Python, Rust, Go, TypeScript, Fortran)
- ✅ JSON-RPC message handling
- ✅ Response callback system

## Keyboard Shortcuts
| Action | Key | Description |
|--------|-----|-------------|
| Code Completion | `Ctrl+Space` | Show completion popup at cursor |
| Hover Info | `Ctrl+H` | Show type/doc tooltip at cursor |
| Diagnostics Panel | `Ctrl+Shift+D` | Toggle diagnostics panel |
| Save File | `Ctrl+S` | Save file (triggers LSP didSave) |
| Navigate Popup | `↑`/`↓` | Move through completion items |
| Navigate Panel | `j`/`k` | Move through diagnostics in panel |
| Jump to Diagnostic | `Enter` | Go to selected diagnostic location |
| Select Item | `Enter` | Insert selected completion |
| Dismiss Popup | `Escape` | Close any popup/tooltip/panel |

## Planned Features
- [ ] Go to definition (`Ctrl+]`)
- [ ] Find references (`Shift+F12`)
- [ ] Code actions and quick fixes (`Ctrl+.`)
- [ ] Rename symbol (`F2`)
- [ ] Document symbols outline (`Ctrl+Shift+O`)
- [ ] Signature help
- [ ] Document formatting