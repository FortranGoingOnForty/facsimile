# Makefile for facsimile
# Detect operating system
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

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
else
    # Linux
    FFLAGS = -O2 -Wall
    FFLAGS_DEV = -O0 -g -Wall -Wextra -pedantic -Wunused-variable -Wuninitialized \
                 -fcheck=all -fbacktrace
    FFLAGS_DEBUG = -O0 -g -fcheck=all -fbacktrace
    CFLAGS = -O2 -Wall
    CFLAGS_DEV = -O0 -g -Wall -Wextra -pedantic -Wconversion
endif

TARGET = fac
VERSION := $(shell cat VERSION 2>/dev/null || echo "unknown")

# Source files (order matters for dependencies)
SOURCES = src/version_module.f90 \
          src/utils/utf8_module.f90 \
          src/utils/regex_module.f90 \
          src/buffer/text_buffer_module.f90 \
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
          src/editor_state_module.f90 \
          src/undo/undo_stack_module.f90 \
          src/workspace/file_tree_module.f90 \
          src/workspace/git_ops_module.f90 \
          src/workspace/file_tree_renderer_module.f90 \
          src/workspace/config_module.f90 \
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
          src/commands/command_handler_module.f90 \
          app/main.f90

OBJECTS = $(SOURCES:.f90=.o)
C_SOURCES = src/terminal/termios_wrapper.c \
            src/utils/regex_wrapper.c \
            src/lsp/lsp_process_wrapper.c
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

# Disable parallel builds to ensure correct module compilation order
.NOTPARALLEL:

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJECTS) $(C_OBJECTS) $(TARGET) *.mod src/*/*.mod src/workspace/*.o src/utils/*.o

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
	@echo "Compiler: $(FC)"
	@echo "Default FFLAGS: $(FFLAGS)"
	@echo "Dev FFLAGS: $(FFLAGS_DEV)"
	@echo "Debug FFLAGS: $(FFLAGS_DEBUG)"
	@echo ""
	@echo "C Compiler: $(CC)"
	@echo "Default CFLAGS: $(CFLAGS)"
	@echo "Dev CFLAGS: $(CFLAGS_DEV)"

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

.PHONY: all clean dev debug info bump-patch bump-minor bump-major version release lsp-modules test-lsp test-lsp-editor clean-lsp lsp-dev
