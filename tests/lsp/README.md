# LSP Tests

This directory contains tests for the Language Server Protocol (LSP) integration in the fac editor.

## Test Files

- `test_json.f90` - Tests the JSON parser implementation
- `test_lsp_init.f90` - Tests LSP server initialization and communication
- `sample.c` - Sample C file for testing LSP with clangd
- `sample.py` - Sample Python file for testing LSP with pylsp
- `sample.rs` - Sample Rust file for testing LSP with rust-analyzer

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
- ✓ Automatic server startup when opening supported files
- ✓ textDocument/didOpen notifications
- ✓ Server initialization handshake

## Planned Features
- [ ] Code completion (Ctrl+Space)
- [ ] Hover information (Ctrl+H)
- [ ] Go to definition (Ctrl+D)
- [ ] Find references (Ctrl+R)
- [ ] Diagnostics display
- [ ] Code actions and fixes