# fac Editor Enhancement Targets

## 🎯 LSP Enhancements

### 1. Diagnostics Display
- [ ] Parse textDocument/publishDiagnostics notifications
- [ ] Store diagnostics per file in editor state
- [ ] Display error/warning markers in the gutter
- [ ] Show diagnostic messages in status line when cursor on error line
- [ ] Add diagnostic severity colors (error=red, warning=yellow, info=blue)
- [ ] Create diagnostics panel (Ctrl+E) to list all issues

### 2. Real-time Updates (didChange)
- [ ] Send textDocument/didChange notifications on buffer edits
- [ ] Implement incremental sync (send only changed portions)
- [ ] Debounce changes to avoid overwhelming the server
- [ ] Update diagnostics in real-time as user types
- [ ] Handle server capability negotiation for sync type

### 3. Go to Definition (Ctrl+])
- [ ] Implement textDocument/definition request
- [ ] Parse LocationLink/Location responses
- [ ] Jump to definition location (same file or different file)
- [ ] Add jump stack to return to previous location (Ctrl+O)
- [ ] Show preview of definition in tooltip if same file

### 4. Find References (Shift+F12)
- [ ] Implement textDocument/references request
- [ ] Create references panel showing all occurrences
- [ ] Navigate through references with n/N keys
- [ ] Group references by file
- [ ] Show preview context for each reference

### 5. Code Actions & Quick Fixes
- [ ] Request code actions at cursor position
- [ ] Display available actions in popup menu
- [ ] Apply workspace edits from code actions
- [ ] Support quick fixes for diagnostics
- [ ] Add keyboard shortcut (Ctrl+.)

### 6. Rename Symbol (F2)
- [ ] Implement textDocument/rename request
- [ ] Show rename prompt with current symbol name
- [ ] Apply workspace-wide rename edits
- [ ] Preview changes before applying
- [ ] Handle rename validation

### 7. Document Symbols Outline (Ctrl+Shift+O)
- [ ] Implement textDocument/documentSymbol request
- [ ] Create outline panel showing document structure
- [ ] Support hierarchical symbol tree
- [ ] Navigate to symbol on selection
- [ ] Show symbol kinds with icons/labels

### 8. Signature Help
- [ ] Trigger on '(' and ',' characters
- [ ] Show function signature tooltip
- [ ] Highlight current parameter
- [ ] Handle overloaded functions
- [ ] Auto-dismiss when cursor moves away

### 9. Document Formatting
- [ ] Implement textDocument/formatting request
- [ ] Add format document command (Shift+Alt+F)
- [ ] Support range formatting for selections
- [ ] Handle format-on-save option
- [ ] Respect .editorconfig settings

### 10. Workspace Symbols (Ctrl+T)
- [ ] Implement workspace/symbol request
- [ ] Create fuzzy search interface
- [ ] Show symbol kind and location
- [ ] Navigate to selected symbol
- [ ] Support incremental search

## 🔧 Editor Core Improvements

### 11. Multiple Cursors Enhancement
- [ ] Add cursor at next occurrence (Ctrl+D)
- [ ] Add cursors above/below (Ctrl+Alt+Up/Down)
- [ ] Select all occurrences (Ctrl+Shift+L)
- [ ] Column selection mode (Alt+Shift+drag)
- [ ] Multi-cursor paste handling

### 12. Search & Replace Improvements
- [ ] Regex search highlighting
- [ ] Search history (up/down in search prompt)
- [ ] Replace preview before applying
- [ ] Search in selection
- [ ] Case-sensitive toggle (Alt+C in search)

### 13. File Explorer Enhancement
- [ ] File icons based on type
- [ ] Create/delete/rename files from tree
- [ ] Drag and drop support (if terminal supports)
- [ ] Git status indicators in tree
- [ ] Filter/search in tree view

### 14. Split Pane Features
- [ ] Synchronized scrolling option
- [ ] Diff view between panes
- [ ] Quick pane switching (Ctrl+1/2/3)
- [ ] Save pane layouts
- [ ] Terminal pane support

### 15. Snippet System
- [ ] Parse VSCode snippet format
- [ ] Tab stops and placeholders
- [ ] Choice elements
- [ ] Variable substitution
- [ ] Custom snippet definitions

## 🎨 UI/UX Enhancements

### 16. Theme System
- [ ] Load VSCode themes (JSON format)
- [ ] Theme hot-reload
- [ ] Separate UI and syntax themes
- [ ] High contrast mode
- [ ] Theme picker interface

### 17. Status Bar Enhancements
- [ ] Clickable status bar items (if terminal supports)
- [ ] LSP server status indicator
- [ ] Git branch and status
- [ ] Encoding and line ending display
- [ ] Language mode selector

### 18. Command Palette (Ctrl+Shift+P)
- [ ] Fuzzy command search
- [ ] Recent commands
- [ ] Command shortcuts display
- [ ] Extension commands
- [ ] Settings commands

### 19. Settings UI
- [ ] JSON settings file (~/.fac/settings.json)
- [ ] Settings editor interface
- [ ] Search settings
- [ ] Workspace-specific settings
- [ ] Settings sync

### 20. Welcome Screen
- [ ] Recent files/projects
- [ ] Quick actions (New, Open, Clone)
- [ ] Tips and tutorials
- [ ] Extension recommendations
- [ ] News/updates section

## 🚀 Performance & Architecture

### 21. Performance Optimizations
- [ ] Lazy loading for large files
- [ ] Virtual scrolling for long documents
- [ ] Syntax highlighting caching
- [ ] Incremental rendering
- [ ] Background file indexing

### 22. Extension System
- [ ] Plugin API definition
- [ ] Lua/Python plugin support
- [ ] Extension marketplace integration
- [ ] Extension settings
- [ ] Extension commands

### 23. Testing Infrastructure
- [ ] Automated UI testing with expect
- [ ] Performance benchmarks
- [ ] LSP mock server for testing
- [ ] Regression test suite
- [ ] Code coverage reporting

### 24. Project Management
- [ ] Project-wide search
- [ ] Project settings (.fac/project.json)
- [ ] Build task integration
- [ ] Debug adapter protocol
- [ ] Source control integration

### 25. Documentation
- [ ] In-editor help system (F1)
- [ ] Interactive tutorial mode
- [ ] Keyboard shortcut cheatsheet
- [ ] API documentation for extensions
- [ ] Video tutorials

## 📊 Priority Order

### Phase 1: Core LSP (Current Sprint)
1. Diagnostics Display
2. Real-time Updates (didChange)
3. Go to Definition
4. Find References

### Phase 2: Essential IDE Features
5. Code Actions & Quick Fixes
6. Document Symbols Outline
7. Signature Help
8. Rename Symbol

### Phase 3: Editor Polish
9. Command Palette
10. Multiple Cursors Enhancement
11. Search & Replace Improvements
12. Document Formatting

### Phase 4: Advanced Features
13. Snippet System
14. Theme System
15. Extension System
16. Project Management

### Phase 5: Professional Features
17. Workspace Symbols
18. Split Pane Features
19. Settings UI
20. Debug Adapter Protocol

## 🎯 Success Metrics

- [ ] All LSP features working with at least 3 language servers
- [ ] Performance: <100ms response time for all operations
- [ ] Memory usage: <50MB for typical usage
- [ ] Test coverage: >80% for core modules
- [ ] Documentation: Complete for all user-facing features

## 📝 Notes

- Each target should include tests and documentation
- Maintain backward compatibility where possible
- Keep terminal compatibility (no GUI dependencies)
- Prioritize user experience and responsiveness
- Consider accessibility (screen reader support)