# fac Editor Enhancement Targets

## 🎯 LSP Enhancements

### 1. Diagnostics Display ✅ (100% Complete!)
- [x] Parse textDocument/publishDiagnostics notifications
- [x] Store diagnostics per file in editor state
- [x] Display error/warning markers in the gutter
- [x] Show diagnostic messages in status line when cursor on error line
- [x] Add diagnostic severity colors (error=red, warning=yellow, info=blue)
- [x] Create diagnostics panel (Ctrl+Shift+D) to list all issues

### 2. Real-time Updates (didChange) ✅ (90% Complete!)
- [x] Send textDocument/didChange notifications on buffer edits
- [x] Document sync module with version tracking
- [x] Debounce changes to avoid overwhelming the server (500ms delay)
- [x] Send textDocument/didSave notifications on file save (Ctrl+S)
- [x] Integration with buffer change tracking
- [ ] Update diagnostics in real-time as user types (server-dependent)
- [ ] Implement incremental sync (send only changed portions)

### 3. Go to Definition ✅ (80% Complete!)
- [x] Implement textDocument/definition request
- [x] Parse LocationLink/Location responses
- [x] Jump to definition location (same file)
- [x] Add jump stack to return to previous location (Alt+,)
- [x] F12 keybinding for go to definition
- [ ] Jump to definition in different file (needs tab opening)
- [ ] Show preview of definition in tooltip if same file

### 4. Find References (Shift+F12) ✅ (100% Complete!)
- [x] Implement textDocument/references request
- [x] Create references panel showing all occurrences
- [x] Navigate through references with arrow keys
- [x] Show references with line/column information
- [x] Parse and populate references from LSP response with callback integration
- [x] Jump to selected reference with Enter key
- [ ] Load preview context for each reference (enhancement)
- [ ] Group references by file (enhancement)

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

### Phase 1: Core LSP 🚀 (98% Complete!)
1. ✅ Diagnostics Display (100%)
2. ✅ Real-time Updates (didChange/didSave) (90%)
3. ✅ Go to Definition (F12) (80%)
4. ✅ Find References (Shift+F12) (100%)

### Phase 2: Essential IDE Features ✅ (100% Complete!)
5. ✅ Code Actions & Quick Fixes (Ctrl+.)
6. ✅ Document Symbols Outline (Ctrl+Shift+O)
7. ✅ Signature Help (auto-trigger)
8. ✅ Rename Symbol (F2)

### Phase 3: Editor Polish (In Progress)
9. ✅ Command Palette (Ctrl+Shift+P)
10. ✅ Multiple Cursors Enhancement (Ctrl+D) - Already complete!
11. ✅ Search & Replace Improvements (100% Complete!)
    - ✅ Regex, case-sensitive, whole word toggles
    - ✅ Replace one/all, match counter
    - ✅ Search history (up/down arrows)
    - ✅ Highlight all matches in viewport
    - ✅ Search in selection
12. ✅ Document Formatting (Shift+Alt+F)

### Phase 3: Completed! ✅
13. ✅ Workspace Symbols (Ctrl+Shift+T) - Fuzzy search all symbols across entire project
14. 🔜 Split Pane Enhancements - Synchronized scrolling, diff view (Moved to trunk)
15. 🔜 Snippet System - Code templates with tab stops (Consider separate branch)

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

## 📚 Documentation

**Complete LSP documentation now available!**

- **[LSP_GUIDE.md](docs/LSP_GUIDE.md)** - Comprehensive guide to all LSP features
  - What is LSP and why use it?
  - How to install and configure language servers
  - Detailed explanation of every LSP feature
  - Language-specific setup (Python, JavaScript, Rust, Fortran, Go, C/C++, etc.)
  - Troubleshooting common issues
  - Tips and tricks for power users

- **[KEYBINDINGS.md](docs/KEYBINDINGS.md)** - Complete keyboard shortcuts reference
  - All LSP keybindings
  - File operations, editing, navigation
  - Search/replace, tabs/windows
  - Panel navigation and special modes
  - Quick reference for most useful combos

## 📝 Notes

- Each target should include tests and documentation
- Maintain backward compatibility where possible
- Keep terminal compatibility (no GUI dependencies)
- Prioritize user experience and responsiveness
- Consider accessibility (screen reader support)