# Makefile for facsimile
# Detect operating system
UNAME_S := $(shell uname -s 2>/dev/null || echo Windows)
UNAME_M := $(shell uname -m 2>/dev/null || echo x86_64)

# Default compilers
FC = gfortran
CC = gcc

# Platform-specific settings
ifeq ($(UNAME_S),Darwin)
    # macOS
    ifeq ($(UNAME_M),arm64)
        # Apple Silicon - use gfortran-15 for syntax highlighting support
        BREW_PREFIX = /opt/homebrew
        ifneq ($(wildcard $(BREW_PREFIX)/bin/gfortran-15),)
            FC = $(BREW_PREFIX)/bin/gfortran-15
            FFLAGS = -O2 -Wall -ffree-line-length-none
            FFLAGS_DEV = -O0 -g -Wall -Wextra -pedantic -Wunused-variable -Wuninitialized \
                         -Wimplicit-interface -fcheck=all -fbacktrace -ffree-line-length-none
            FFLAGS_DEBUG = -O0 -g -fcheck=all -fbacktrace -ffree-line-length-none
        else ifneq ($(wildcard $(BREW_PREFIX)/bin/flang-new),)
            # Fallback to flang-new if gfortran-15 not available
            FC = $(BREW_PREFIX)/bin/flang-new
            # flang-new flags
            FFLAGS = -O2
            # Development flags (flang-new has very limited warning support)
            # For comprehensive warnings, use gfortran instead
            FFLAGS_DEV = -O0 -g -pedantic
            # Debug flags with debug symbols
            FFLAGS_DEBUG = -O0 -g
        else
            # Fallback to any available gfortran
            ifneq ($(wildcard $(BREW_PREFIX)/bin/gfortran-*),)
                FC = $(shell ls $(BREW_PREFIX)/bin/gfortran-* | head -n1)
            endif
            FFLAGS = -O2 -Wall -ffree-line-length-none
            FFLAGS_DEV = -O0 -g -Wall -Wextra -pedantic -Wunused-variable -Wuninitialized \
                         -Wimplicit-interface -fcheck=all -fbacktrace -ffree-line-length-none
            FFLAGS_DEBUG = -O0 -g -fcheck=all -fbacktrace -ffree-line-length-none
        endif
        CFLAGS = -O2 -Wall
        CFLAGS_DEV = -O0 -g -Wall -Wextra -pedantic -Wconversion
    else
        # Intel Mac
        BREW_PREFIX = /usr/local
        FFLAGS = -O2 -Wall -ffree-line-length-none
        FFLAGS_DEV = -O0 -g -Wall -Wextra -pedantic -Wunused-variable -Wuninitialized \
                     -fcheck=all -fbacktrace -ffree-line-length-none
        FFLAGS_DEBUG = -O0 -g -fcheck=all -fbacktrace -ffree-line-length-none
        CFLAGS = -O2 -Wall
        CFLAGS_DEV = -O0 -g -Wall -Wextra -pedantic -Wconversion
    endif
    TARGET = fac
else ifneq (,$(findstring MINGW,$(UNAME_S)))
    # Windows (MSYS2/MinGW)
    FFLAGS = -O2 -Wall -ffree-line-length-none
    FFLAGS_DEV = -O0 -g -Wall -Wextra -pedantic -Wunused-variable -Wuninitialized \
                 -fcheck=all -fbacktrace -ffree-line-length-none
    FFLAGS_DEBUG = -O0 -g -fcheck=all -fbacktrace -ffree-line-length-none
    CFLAGS = -O2 -Wall
    CFLAGS_DEV = -O0 -g -Wall -Wextra -pedantic -Wconversion
    TARGET = fac.exe
else ifneq (,$(findstring MSYS,$(UNAME_S)))
    # Windows (MSYS2)
    FFLAGS = -O2 -Wall -ffree-line-length-none
    FFLAGS_DEV = -O0 -g -Wall -Wextra -pedantic -Wunused-variable -Wuninitialized \
                 -fcheck=all -fbacktrace -ffree-line-length-none
    FFLAGS_DEBUG = -O0 -g -fcheck=all -fbacktrace -ffree-line-length-none
    CFLAGS = -O2 -Wall
    CFLAGS_DEV = -O0 -g -Wall -Wextra -pedantic -Wconversion
    TARGET = fac.exe
else ifeq ($(UNAME_S),Windows)
    # Windows (native or cross-compile)
    FFLAGS = -O2 -Wall -ffree-line-length-none
    FFLAGS_DEV = -O0 -g -Wall -Wextra -pedantic -Wunused-variable -Wuninitialized \
                 -fcheck=all -fbacktrace -ffree-line-length-none
    FFLAGS_DEBUG = -O0 -g -fcheck=all -fbacktrace -ffree-line-length-none
    CFLAGS = -O2 -Wall
    CFLAGS_DEV = -O0 -g -Wall -Wextra -pedantic -Wconversion
    TARGET = fac.exe
else
    # Linux
    FFLAGS = -O2 -Wall
    FFLAGS_DEV = -O0 -g -Wall -Wextra -pedantic -Wunused-variable -Wuninitialized \
                 -fcheck=all -fbacktrace
    FFLAGS_DEBUG = -O0 -g -fcheck=all -fbacktrace
    CFLAGS = -O2 -Wall
    CFLAGS_DEV = -O0 -g -Wall -Wextra -pedantic -Wconversion
    TARGET = fac
endif

VERSION := $(shell cat VERSION 2>/dev/null || echo "unknown")

# Source files (order matters for dependencies)
SOURCES = src/version_module.f90 \
          src/utils/platform_module.f90 \
          src/utils/utf8_module.f90 \
          src/utils/regex_module.f90 \
          src/utils/dir_scan_module.f90 \
          src/workspace/session_ipc_module.f90 \
          src/ui/clickable_region_module.f90 \
          src/ui/tab_drag_module.f90 \
          src/syntax/comment_syntax_module.f90 \
          src/ai/ai_http_module.f90 \
          src/ai/completion_cache_module.f90 \
          src/ai/ai_state_module.f90 \
          src/ai/ai_json_module.f90 \
          src/ai/completion_sanitize_module.f90 \
          src/ai/ollama_client_module.f90 \
          src/buffer/text_buffer_module.f90 \
          src/ai/completion_context_module.f90 \
          src/ai/completion_prompt_module.f90 \
          src/clipboard/yank_stack_module.f90 \
          src/clipboard/clipboard_module.f90 \
          src/terminal/raw_mode_module.f90 \
          src/terminal/terminal_io_module.f90 \
          src/terminal/input_handler_module.f90 \
          src/utils/bracket_matching_module.f90 \
          src/navigation/jump_stack_module.f90 \
          src/lsp/json_module.f90 \
          src/lsp/lsp_protocol_module.f90 \
          src/lsp/lsp_server_manager_module.f90 \
          src/lsp/lsp_client_module.f90 \
          src/lsp/document_sync_module.f90 \
          src/lsp/diagnostics_module.f90 \
          src/lsp/server_detection_module.f90 \
          src/lsp/server_installer_module.f90 \
          src/ui/context_menu_module.f90 \
          src/ui/completion_popup_module.f90 \
          src/ui/hover_tooltip_module.f90 \
          src/ui/diagnostics_panel_module.f90 \
          src/ui/references_panel_module.f90 \
          src/ui/code_actions_panel_module.f90 \
          src/ui/symbols_panel_module.f90 \
          src/ui/signature_tooltip_module.f90 \
          src/ui/command_palette_module.f90 \
          src/ui/workspace_symbols_panel_module.f90 \
          src/ui/lsp_server_installer_panel_module.f90 \
          src/ui/group_picker_module.f90 \
          src/ui/terminal_panel_module.f90 \
          src/ui/ghost_text_module.f90 \
          src/editor_state_module.f90 \
          src/undo/undo_stack_module.f90 \
          src/workspace/file_tree_module.f90 \
          src/workspace/git_ops_module.f90 \
          src/workspace/file_tree_renderer_module.f90 \
          src/workspace/config_module.f90 \
          src/workspace/settings_module.f90 \
          src/workspace/app_state_module.f90 \
          src/workspace/favorites_module.f90 \
          src/workspace/recents_module.f90 \
          src/workspace/workspace_module.f90 \
          src/workspace/backup_module.f90 \
          src/syntax/syntax_highlighter_module.f90 \
          src/ui/help_display_module.f90 \
          src/ui/text_prompt_module.f90 \
          src/ui/rename_prompt_module.f90 \
          src/ui/search_prompt_module.f90 \
          src/ui/unified_search_module.f90 \
          src/terminal/renderer_module.f90 \
          src/ui/replace_prompt_module.f90 \
          src/ui/goto_prompt_module.f90 \
          src/ui/save_prompt_module.f90 \
          src/ui/binary_prompt_module.f90 \
          src/fortress/filesystem/fortress_fs_module.f90 \
          src/fortress/ui/fortress_display_module.f90 \
          src/fortress/ui/welcome_menu_module.f90 \
          src/fortress/fortress_navigator_module.f90 \
          src/ai/ai_engine_module.f90 \
          src/commands/comment_command_module.f90 \
          src/commands/indent_policy_module.f90 \
          src/commands/command_handler_module.f90 \
          app/main.f90

OBJECTS = $(SOURCES:.f90=.o)
C_SOURCES = src/terminal/termios_wrapper.c \
            src/terminal/pty_wrapper.c \
            src/terminal/vt100_grid.c \
            src/utils/regex_wrapper.c \
            src/utils/platform_wrapper.c \
            src/utils/dir_scan_wrapper.c \
            src/lsp/lsp_process_wrapper.c \
            src/ai/ai_http.c
C_OBJECTS = $(C_SOURCES:.c=.o)

all: $(TARGET)

# Generate version module before building
src/version_module.f90: VERSION
	@echo "Generating version module..."
	@echo "module version_module" > $@
	@echo "    implicit none" >> $@
	@echo "    character(len=*), parameter :: VERSION = '$(VERSION)'" >> $@
	@echo "end module version_module" >> $@

$(TARGET): src/version_module.f90 $(OBJECTS) $(C_OBJECTS)
	$(FC) $(FFLAGS) -o $(TARGET) $(OBJECTS) $(C_OBJECTS)

# version_module has no tracked .mod dependency, so a version bump would
# otherwise only recompile version_module.o and leave stale VERSION strings
# inlined in modules that `use` it (e.g. app/main.o). Force a full rebuild.
$(OBJECTS): src/version_module.f90

# Disable parallel builds to ensure correct module compilation order
.NOTPARALLEL:

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJECTS) $(C_OBJECTS) $(TARGET) fac fac.exe *.mod src/*/*.mod src/workspace/*.o src/utils/*.o

# Development build with comprehensive warnings
dev: clean
	@echo "Building with development flags (warnings + checks)..."
	@echo "Fortran flags: $(FFLAGS_DEV)"
	@echo "C flags: $(CFLAGS_DEV)"
	@echo ""
	@$(MAKE) all FFLAGS="$(FFLAGS_DEV)" CFLAGS="$(CFLAGS_DEV)"

# Debug build with runtime checks and symbols
debug: clean
	@echo "Building with debug flags (runtime checks + symbols)..."
	@echo "Fortran flags: $(FFLAGS_DEBUG)"
	@echo ""
	@$(MAKE) all FFLAGS="$(FFLAGS_DEBUG)"

# Show current compiler and flags
info:
	@echo "Platform: $(UNAME_S)"
	@echo "Architecture: $(UNAME_M)"
	@echo "Target: $(TARGET)"
	@echo ""
	@echo "Fortran Compiler: $(FC)"
	@echo "Default FFLAGS: $(FFLAGS)"
	@echo "Dev FFLAGS: $(FFLAGS_DEV)"
	@echo "Debug FFLAGS: $(FFLAGS_DEBUG)"
	@echo ""
	@echo "C Compiler: $(CC)"
	@echo "Default CFLAGS: $(CFLAGS)"
	@echo "Dev CFLAGS: $(CFLAGS_DEV)"

# Generate compile_commands.json for clangd, covering the C wrappers.
# Without it clangd falls back to fpm's build/compile_commands.json and
# interpolates gfortran flags (-ffree-form, -J) onto the C files. Uses
# dev CFLAGS so the editor surfaces the same warnings as `make dev`.
compile-commands:
	@echo '[' > compile_commands.json
	@first=1; for f in $(C_SOURCES); do \
	  [ $$first -eq 1 ] || echo '  ,' >> compile_commands.json; \
	  first=0; \
	  printf '  {"directory": "%s", "file": "%s", "arguments": ["$(CC)"' \
	    "$$(pwd)" "$$f" >> compile_commands.json; \
	  for a in $(CFLAGS_DEV); do \
	    printf ', "%s"' "$$a" >> compile_commands.json; \
	  done; \
	  printf ', "-c", "%s", "-o", "%s"]}\n' \
	    "$$f" "$${f%.c}.o" >> compile_commands.json; \
	done
	@echo ']' >> compile_commands.json
	@echo "Wrote compile_commands.json ($(words $(C_SOURCES)) C entries)"

# Installation (supports DESTDIR and PREFIX for packaging)
PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
DOCDIR ?= $(PREFIX)/share/doc/facsimile

install: $(TARGET)
	@echo "Installing to $(DESTDIR)$(BINDIR)..."
	install -d "$(DESTDIR)$(BINDIR)"
	install -m755 "$(TARGET)" "$(DESTDIR)$(BINDIR)/fac"
	ln -sf fac "$(DESTDIR)$(BINDIR)/facsimile"
	install -d "$(DESTDIR)$(DOCDIR)"
	install -m644 README.md "$(DESTDIR)$(DOCDIR)/README.md"

uninstall:
	rm -f "$(DESTDIR)$(BINDIR)/fac" "$(DESTDIR)$(BINDIR)/facsimile"
	rm -rf "$(DESTDIR)$(DOCDIR)"

# Version management targets
bump-patch:
	@echo "Current version: $(VERSION)"
	@NEW_VERSION=$$(echo $(VERSION) | awk -F. '{print $$1"."$$2"."$$3+1}'); \
	echo "Bumping to: $$NEW_VERSION"; \
	echo $$NEW_VERSION > VERSION; \
	echo "Updated VERSION file to $$NEW_VERSION"; \
	$(MAKE) src/version_module.f90; \
	echo "Updated src/version_module.f90"
	@echo "Now run: make clean && make"

bump-minor:
	@echo "Current version: $(VERSION)"
	@NEW_VERSION=$$(echo $(VERSION) | awk -F. '{print $$1"."$$2+1".0"}'); \
	echo "Bumping to: $$NEW_VERSION"; \
	echo $$NEW_VERSION > VERSION; \
	echo "Updated VERSION file to $$NEW_VERSION"; \
	$(MAKE) src/version_module.f90; \
	echo "Updated src/version_module.f90"
	@echo "Now run: make clean && make"

bump-major:
	@echo "Current version: $(VERSION)"
	@NEW_VERSION=$$(echo $(VERSION) | awk -F. '{print $$1+1".0.0"}'); \
	echo "Bumping to: $$NEW_VERSION"; \
	echo $$NEW_VERSION > VERSION; \
	echo "Updated VERSION file to $$NEW_VERSION"; \
	$(MAKE) src/version_module.f90; \
	echo "Updated src/version_module.f90"
	@echo "Now run: make clean && make"

# Show current version
version:
	@echo "$(VERSION)"

# Release checklist
release: clean all
	@echo ""
	@echo "========================================"
	@echo "Release Build Complete: v$(VERSION)"
	@echo "========================================"
	@echo ""
	@./$(TARGET) --version
	@echo ""
	@echo "Next steps:"
	@echo "1. Test the binary: ./$(TARGET)"
	@echo "2. Commit changes: git add VERSION src/version_module.f90"
	@echo "3. Commit: git commit -m 'Release v$(VERSION)'"
	@echo "4. Tag: git tag v$(VERSION)"
	@echo "5. Push: git push && git push --tags"
	@echo ""

# LSP development targets
lsp-modules: src/lsp/json_module.o src/lsp/lsp_protocol_module.o src/lsp/lsp_server_manager_module.o src/lsp/lsp_client_module.o src/lsp/lsp_process_wrapper.o
	@echo "LSP modules built successfully"

test-lsp: lsp-modules
	@echo "Testing LSP JSON parser..."
	@$(FC) $(FFLAGS_DEBUG) tests/lsp/test_json.f90 src/lsp/json_module.o -o tests/lsp/test_json 2>/dev/null && tests/lsp/test_json || true
	@echo ""
	@echo "Testing LSP initialization..."
	@$(FC) $(FFLAGS_DEBUG) tests/lsp/test_lsp_init.f90 src/lsp/json_module.o src/lsp/lsp_protocol_module.o src/lsp/lsp_process_wrapper.o src/lsp/lsp_client_module.o src/lsp/lsp_server_manager_module.o -o tests/lsp/test_lsp_init 2>/dev/null && tests/lsp/test_lsp_init || true

	@echo ""
	@echo "Testing that a server which never reads cannot hang us..."
	@$(CC) -O1 -o tests/lsp/test_write_deadlock tests/lsp/test_write_deadlock.c \
		src/lsp/lsp_process_wrapper.o && timeout 60 tests/lsp/test_write_deadlock

# Syntax-check the _WIN32 branches without a Windows toolchain. See
# tests/winstub/README.md -- these branches are invisible to the normal build,
# and a mistake in one is otherwise found only after a tag has been cut.
WIN_CHECK_SRC = src/utils/platform_wrapper.c src/terminal/pty_wrapper.c src/ai/ai_http.c

check-windows:
	@echo "Syntax-checking the Windows branches..."
	@for f in $(WIN_CHECK_SRC); do \
		printf '  %-40s ' "$$f"; \
		$(CC) -fsyntax-only -D_WIN32 -Itests/winstub "$$f" || exit 1; \
		echo ok; \
	done
	@echo "Windows branches OK (see tests/winstub/README.md for what is not covered)"

test-lsp-editor: all
	@echo "Testing LSP in editor with sample C file..."
	@echo "Opening tests/lsp/sample.c - check for LSP server initialization"
	@timeout 2 ./fac tests/lsp/sample.c < /dev/null 2>&1 | grep -q "LSP server initialized" && \
		echo "✓ LSP server initialized for C file" || \
		echo "✗ LSP server did not initialize (check if clangd is installed)"

clean-lsp:
	rm -f src/lsp/*.o src/lsp/*.mod /tmp/test_json /tmp/test_lsp_init /tmp/test_lsp_raw

# Build LSP modules with debug flags for development
lsp-dev: clean-lsp
	@echo "Building LSP modules with debug flags..."
	@$(MAKE) lsp-modules FFLAGS="$(FFLAGS_DEBUG)" CFLAGS="$(CFLAGS_DEV)"

.PHONY: all clean dev debug info install uninstall bump-patch bump-minor bump-major version release lsp-modules test-lsp test-lsp-editor clean-lsp lsp-dev compile-commands
