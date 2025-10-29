# Makefile for facsimile
FC = gfortran
CC = gcc
FFLAGS = -O2 -Wall
CFLAGS = -O2 -Wall
TARGET = fac

# Source files (order matters for dependencies)
SOURCES = src/buffer/text_buffer_module.f90 \
          src/clipboard/yank_stack_module.f90 \
          src/clipboard/clipboard_module.f90 \
          src/terminal/raw_mode_module.f90 \
          src/terminal/terminal_io_module.f90 \
          src/terminal/input_handler_module.f90 \
          src/utils/bracket_matching_module.f90 \
          src/editor_state_module.f90 \
          src/undo/undo_stack_module.f90 \
          src/terminal/renderer_module.f90 \
          src/ui/help_display_module.f90 \
          src/ui/search_prompt_module.f90 \
          src/ui/replace_prompt_module.f90 \
          src/ui/goto_prompt_module.f90 \
          src/commands/command_handler_module.f90 \
          app/main.f90

OBJECTS = $(SOURCES:.f90=.o)
C_SOURCES = src/terminal/termios_wrapper.c
C_OBJECTS = $(C_SOURCES:.c=.o)

all: $(TARGET)

$(TARGET): $(OBJECTS) $(C_OBJECTS)
	$(FC) $(FFLAGS) -o $(TARGET) $(OBJECTS) $(C_OBJECTS)

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJECTS) $(C_OBJECTS) $(TARGET) *.mod src/*/*.mod

.PHONY: all clean
