# LSP Tests

This directory contains tests for the Language Server Protocol (LSP) integration in the fac editor.

## Test Files

### Language-Specific Samples
- `sample.c` - C/C++ test file with completion and hover test points
- `sample_python.py` - Python test file with class/module completion tests
- `sample_rust.rs` - Rust test file with struct and trait tests
- `sample_typescript.ts` - TypeScript test file with interface and generic tests

### Unit Tests
- `test_json.f90` - Tests the JSON parser implementation
- `test_lsp_init.f90` - Tests LSP server initialization and communication

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
- ✅ Code completion (`Ctrl+Space`) with popup UI
- ✅ Hover information (`Ctrl+H`) with tooltip display
- ✅ Multi-language support (C/C++, Python, Rust, Go, TypeScript, Fortran)
- ✅ JSON-RPC message handling
- ✅ Response callback system

## Keyboard Shortcuts
| Action | Key | Description |
|--------|-----|-------------|
| Code Completion | `Ctrl+Space` | Show completion popup at cursor |
| Hover Info | `Ctrl+H` | Show type/doc tooltip at cursor |
| Navigate Popup | `↑`/`↓` | Move through completion items |
| Select Item | `Enter` | Insert selected completion |
| Dismiss Popup | `Escape` | Close any popup/tooltip |

## Planned Features
- [ ] textDocument/didChange notifications for real-time updates
- [ ] Diagnostics display with error/warning markers
- [ ] Go to definition (`Ctrl+]`)
- [ ] Find references
- [ ] Code actions and quick fixes
- [ ] Rename symbol
- [ ] Document symbols outline