// C wrapper for terminal functions to enable raw mode in Fortran
// Platform-independent implementation for Unix and Windows

#ifdef _WIN32
// Windows implementation
#include <windows.h>
#include <conio.h>
#include <stdio.h>
#include <string.h>

static HANDLE hStdin = INVALID_HANDLE_VALUE;
static HANDLE hStdout = INVALID_HANDLE_VALUE;
static DWORD orig_stdin_mode = 0;
static DWORD orig_stdout_mode = 0;
static int raw_mode_enabled = 0;

// Input buffer for batching reads
#define INPUT_BUFFER_SIZE 256
static unsigned char input_buffer[INPUT_BUFFER_SIZE];
static int buffer_start = 0;
static int buffer_end = 0;

// Flush any pending input from stdin
static void flush_input(void) {
    FlushConsoleInputBuffer(hStdin);
    buffer_start = buffer_end = 0;
}

// Enable raw mode - returns 0 on success, -1 on failure
int enable_raw_mode(void) {
    if (raw_mode_enabled) return 0;

    hStdin = GetStdHandle(STD_INPUT_HANDLE);
    hStdout = GetStdHandle(STD_OUTPUT_HANDLE);

    if (hStdin == INVALID_HANDLE_VALUE || hStdout == INVALID_HANDLE_VALUE) {
        return -1;
    }

    // Save original console modes
    if (!GetConsoleMode(hStdin, &orig_stdin_mode)) {
        return -1;
    }
    if (!GetConsoleMode(hStdout, &orig_stdout_mode)) {
        return -1;
    }

    // Set input mode: disable line input, echo, and processed input
    DWORD new_stdin_mode = orig_stdin_mode;
    new_stdin_mode &= ~(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT);
    new_stdin_mode |= ENABLE_VIRTUAL_TERMINAL_INPUT;  // Enable VT input sequences

    if (!SetConsoleMode(hStdin, new_stdin_mode)) {
        // Try without VT input (older Windows)
        new_stdin_mode &= ~ENABLE_VIRTUAL_TERMINAL_INPUT;
        if (!SetConsoleMode(hStdin, new_stdin_mode)) {
            return -1;
        }
    }

    // Enable VT processing for output (ANSI escape codes)
    DWORD new_stdout_mode = orig_stdout_mode;
    new_stdout_mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING | DISABLE_NEWLINE_AUTO_RETURN;

    if (!SetConsoleMode(hStdout, new_stdout_mode)) {
        // Try with just VT processing
        new_stdout_mode = orig_stdout_mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING;
        if (!SetConsoleMode(hStdout, new_stdout_mode)) {
            // Restore stdin and fail
            SetConsoleMode(hStdin, orig_stdin_mode);
            return -1;
        }
    }

    raw_mode_enabled = 1;
    buffer_start = buffer_end = 0;
    flush_input();

    return 0;
}

// Disable raw mode - returns 0 on success, -1 on failure
int disable_raw_mode(void) {
    if (!raw_mode_enabled) return 0;

    int result = 0;
    if (!SetConsoleMode(hStdin, orig_stdin_mode)) {
        result = -1;
    }
    if (!SetConsoleMode(hStdout, orig_stdout_mode)) {
        result = -1;
    }

    raw_mode_enabled = 0;
    buffer_start = buffer_end = 0;
    return result;
}

// Check if input is available (non-blocking)
int input_available(void) {
    if (buffer_start < buffer_end) {
        return 1;
    }

    DWORD num_events = 0;
    if (!GetNumberOfConsoleInputEvents(hStdin, &num_events)) {
        return 0;
    }

    if (num_events == 0) return 0;

    // Peek to see if there's actually a key event
    INPUT_RECORD ir[16];
    DWORD events_read = 0;
    if (!PeekConsoleInput(hStdin, ir, 16, &events_read)) {
        return 0;
    }

    for (DWORD i = 0; i < events_read; i++) {
        if (ir[i].EventType == KEY_EVENT && ir[i].Event.KeyEvent.bKeyDown) {
            return 1;
        }
    }

    return 0;
}

// Get count of available input bytes
int input_available_count(void) {
    int buffered = buffer_end - buffer_start;
    DWORD num_events = 0;
    GetNumberOfConsoleInputEvents(hStdin, &num_events);
    return buffered + (int)num_events;
}

// Read a single character from console
static int read_console_char(int timeout_ms) {
    DWORD wait_result;

    if (timeout_ms < 0) {
        wait_result = WaitForSingleObject(hStdin, INFINITE);
    } else {
        wait_result = WaitForSingleObject(hStdin, (DWORD)timeout_ms);
    }

    if (wait_result != WAIT_OBJECT_0) {
        return -1;  // Timeout or error
    }

    INPUT_RECORD ir;
    DWORD events_read;

    while (ReadConsoleInput(hStdin, &ir, 1, &events_read) && events_read > 0) {
        if (ir.EventType == KEY_EVENT && ir.Event.KeyEvent.bKeyDown) {
            KEY_EVENT_RECORD *key = &ir.Event.KeyEvent;

            // Handle special keys by generating escape sequences
            if (key->wVirtualKeyCode == VK_UP) {
                input_buffer[buffer_end++] = 27;  // ESC
                input_buffer[buffer_end++] = '[';
                input_buffer[buffer_end++] = 'A';
                return input_buffer[buffer_start++];
            } else if (key->wVirtualKeyCode == VK_DOWN) {
                input_buffer[buffer_end++] = 27;
                input_buffer[buffer_end++] = '[';
                input_buffer[buffer_end++] = 'B';
                return input_buffer[buffer_start++];
            } else if (key->wVirtualKeyCode == VK_RIGHT) {
                input_buffer[buffer_end++] = 27;
                input_buffer[buffer_end++] = '[';
                input_buffer[buffer_end++] = 'C';
                return input_buffer[buffer_start++];
            } else if (key->wVirtualKeyCode == VK_LEFT) {
                input_buffer[buffer_end++] = 27;
                input_buffer[buffer_end++] = '[';
                input_buffer[buffer_end++] = 'D';
                return input_buffer[buffer_start++];
            } else if (key->wVirtualKeyCode == VK_HOME) {
                input_buffer[buffer_end++] = 27;
                input_buffer[buffer_end++] = '[';
                input_buffer[buffer_end++] = 'H';
                return input_buffer[buffer_start++];
            } else if (key->wVirtualKeyCode == VK_END) {
                input_buffer[buffer_end++] = 27;
                input_buffer[buffer_end++] = '[';
                input_buffer[buffer_end++] = 'F';
                return input_buffer[buffer_start++];
            } else if (key->wVirtualKeyCode == VK_DELETE) {
                input_buffer[buffer_end++] = 27;
                input_buffer[buffer_end++] = '[';
                input_buffer[buffer_end++] = '3';
                input_buffer[buffer_end++] = '~';
                return input_buffer[buffer_start++];
            } else if (key->wVirtualKeyCode == VK_PRIOR) {  // Page Up
                input_buffer[buffer_end++] = 27;
                input_buffer[buffer_end++] = '[';
                input_buffer[buffer_end++] = '5';
                input_buffer[buffer_end++] = '~';
                return input_buffer[buffer_start++];
            } else if (key->wVirtualKeyCode == VK_NEXT) {  // Page Down
                input_buffer[buffer_end++] = 27;
                input_buffer[buffer_end++] = '[';
                input_buffer[buffer_end++] = '6';
                input_buffer[buffer_end++] = '~';
                return input_buffer[buffer_start++];
            } else if (key->wVirtualKeyCode == VK_INSERT) {
                input_buffer[buffer_end++] = 27;
                input_buffer[buffer_end++] = '[';
                input_buffer[buffer_end++] = '2';
                input_buffer[buffer_end++] = '~';
                return input_buffer[buffer_start++];
            } else if (key->wVirtualKeyCode >= VK_F1 && key->wVirtualKeyCode <= VK_F12) {
                // F1-F12 keys
                int fnum = key->wVirtualKeyCode - VK_F1 + 1;
                input_buffer[buffer_end++] = 27;
                input_buffer[buffer_end++] = 'O';
                input_buffer[buffer_end++] = 'P' + (fnum - 1);  // Simplified
                return input_buffer[buffer_start++];
            } else if (key->uChar.AsciiChar != 0) {
                // Regular ASCII character
                return (unsigned char)key->uChar.AsciiChar;
            }
        }
    }

    return -1;
}

// Fill the input buffer
static void fill_input_buffer(void) {
    if (buffer_start > 0 && buffer_start < buffer_end) {
        memmove(input_buffer, input_buffer + buffer_start,
                (size_t)(buffer_end - buffer_start));
        buffer_end -= buffer_start;
        buffer_start = 0;
    } else if (buffer_start >= buffer_end) {
        buffer_start = buffer_end = 0;
    }
}

// Read a single character with smart timeout
int read_char_timeout(void) {
    if (buffer_start < buffer_end) {
        return input_buffer[buffer_start++];
    }

    fill_input_buffer();
    if (buffer_start < buffer_end) {
        return input_buffer[buffer_start++];
    }

    return read_console_char(50);  // 50ms timeout
}

// Read a character with very short timeout (for escape sequences)
int read_char_escape(void) {
    if (buffer_start < buffer_end) {
        return input_buffer[buffer_start++];
    }

    fill_input_buffer();
    if (buffer_start < buffer_end) {
        return input_buffer[buffer_start++];
    }

    return read_console_char(5);  // 5ms timeout
}

// Public function to flush input buffer
void flush_input_buffer(void) {
    flush_input();
}

// Windows resize events are observed through GetConsoleScreenBufferInfo.
int take_terminal_resize_event(void) {
    return 0;
}

// Get terminal size
void get_terminal_size(int *rows, int *cols) {
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    if (GetConsoleScreenBufferInfo(hStdout, &csbi)) {
        *cols = csbi.srWindow.Right - csbi.srWindow.Left + 1;
        *rows = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
    } else {
        *rows = 24;
        *cols = 80;
    }
}

// Output buffering (Windows)
#define OUTPUT_BUFFER_SIZE (256 * 1024)
static char output_buf[OUTPUT_BUFFER_SIZE];
static int output_pos = 0;

void term_buf_write(const char *data, int len) {
    if (len <= 0) return;
    if (output_pos + len > OUTPUT_BUFFER_SIZE) {
        if (output_pos > 0) {
            DWORD written;
            WriteFile(hStdout, output_buf, output_pos, &written, NULL);
            output_pos = 0;
        }
    }
    if (len > OUTPUT_BUFFER_SIZE) {
        DWORD written;
        WriteFile(hStdout, data, len, &written, NULL);
        return;
    }
    memcpy(output_buf + output_pos, data, len);
    output_pos += len;
}

void term_buf_flush(void) {
    if (output_pos > 0) {
        DWORD written;
        WriteFile(hStdout, output_buf, output_pos, &written, NULL);
        output_pos = 0;
    }
}

#else
// Unix implementation (original code)
#include <termios.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#include <string.h>


// write() that finishes the job.
//
// A short write is legal and does happen: the kernel takes what fits in the
// tty buffer and returns that count. These calls discarded the count, which
// is exactly what the compiler was warning about, and the consequence is a
// silently truncated frame -- the screen stays wrong until something else
// repaints it. Retrying is the fix; ignoring the warning was not.
static void write_all(int fd, const void *data, size_t len) {
    const char *buf = data;
    while (len > 0) {
        ssize_t n = write(fd, buf, len);
        if (n > 0) {
            buf += n;
            len -= (size_t)n;
            continue;
        }
        if (n < 0 && errno == EINTR) continue;
        return;                 // a real error: nothing useful to do here
    }
}

static struct termios orig_termios;
static int raw_mode_enabled = 0;
static volatile sig_atomic_t terminal_resize_pending = 0;
static struct sigaction orig_sigwinch;
static int sigwinch_handler_installed = 0;

static void note_terminal_resize(int signo) {
    (void)signo;
    terminal_resize_pending = 1;
}

// Input buffer for batching reads
#define INPUT_BUFFER_SIZE 256
static unsigned char input_buffer[INPUT_BUFFER_SIZE];
static int buffer_start = 0;
static int buffer_end = 0;

// Flush any pending input from stdin
static void flush_input(void) {
    // Discard any pending input data
    tcflush(STDIN_FILENO, TCIFLUSH);

    // Also drain our internal buffer
    buffer_start = buffer_end = 0;

    // Small delay to let any in-flight data arrive and be discarded
    struct timeval tv = {0, 10000};  // 10ms
    fd_set readfds;
    FD_ZERO(&readfds);
    FD_SET(STDIN_FILENO, &readfds);
    while (select(STDIN_FILENO + 1, &readfds, NULL, NULL, &tv) > 0) {
        char discard[256];
        // Draining leftover input, so the count really is of no interest --
        // named rather than dropped, to say that is deliberate.
        ssize_t discarded = read(STDIN_FILENO, discard, sizeof(discard));
        (void)discarded;
        tv.tv_sec = 0;
        tv.tv_usec = 5000;  // Keep draining with shorter timeout
        FD_ZERO(&readfds);
        FD_SET(STDIN_FILENO, &readfds);
    }
}

// Enable raw mode - returns 0 on success, -1 on failure
int enable_raw_mode(void) {
    if (raw_mode_enabled) return 0;

    if (tcgetattr(STDIN_FILENO, &orig_termios) == -1) {
        return -1;
    }

    struct termios raw = orig_termios;

    // Input flags: disable break, CR to NL, parity check, strip char, start/stop output control
    raw.c_iflag &= ~(tcflag_t)(BRKINT | ICRNL | INPCK | ISTRIP | IXON);

    // Output flags: disable post processing
    raw.c_oflag &= ~(tcflag_t)(OPOST);

    // Control flags: set 8 bit chars
    raw.c_cflag |= (CS8);

    // Local flags: disable canonical mode, echo, signals, extended input processing
    raw.c_lflag &= ~(tcflag_t)(ECHO | ICANON | ISIG | IEXTEN);

    // Control characters: non-blocking reads
    // We'll use select() for timeout management instead of VTIME
    raw.c_cc[VMIN] = 0;   // Don't block
    raw.c_cc[VTIME] = 0;  // No timeout - we use select() instead

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) {
        return -1;
    }

    raw_mode_enabled = 1;
    buffer_start = buffer_end = 0;

    // A host may ask an embedded terminal to redraw without changing its
    // final dimensions. Size polling cannot see that event, so retain it in
    // a signal-safe latch for the main loop. No terminal work happens here.
    struct sigaction action;
    memset(&action, 0, sizeof(action));
    action.sa_handler = note_terminal_resize;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGWINCH, &action, &orig_sigwinch) == 0) {
        sigwinch_handler_installed = 1;
    }
    terminal_resize_pending = 0;

    // Flush any stale input that might be waiting
    flush_input();

    return 0;
}

// Disable raw mode - returns 0 on success, -1 on failure
int disable_raw_mode(void) {
    if (!raw_mode_enabled) return 0;

    if (sigwinch_handler_installed) {
        sigaction(SIGWINCH, &orig_sigwinch, NULL);
        sigwinch_handler_installed = 0;
    }

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios) == -1) {
        return -1;
    }

    raw_mode_enabled = 0;
    buffer_start = buffer_end = 0;
    return 0;
}

// Check if input is available (non-blocking)
int input_available(void) {
    // First check our buffer
    if (buffer_start < buffer_end) {
        return 1;
    }

    // Then check stdin
    int nread;
    if (ioctl(STDIN_FILENO, FIONREAD, &nread) == -1) {
        return 0;
    }
    return nread > 0;
}

// Get count of available input bytes
int input_available_count(void) {
    int buffered = buffer_end - buffer_start;
    int pending = 0;
    if (ioctl(STDIN_FILENO, FIONREAD, &pending) == -1) {
        pending = 0;
    }
    return buffered + pending;
}

// Fill the input buffer with all available data
static void fill_input_buffer(void) {
    // Shift remaining data to start of buffer
    if (buffer_start > 0 && buffer_start < buffer_end) {
        memmove(input_buffer, input_buffer + buffer_start,
                (size_t)(buffer_end - buffer_start));
        buffer_end -= buffer_start;
        buffer_start = 0;
    } else if (buffer_start >= buffer_end) {
        buffer_start = buffer_end = 0;
    }

    // Read all available data into buffer
    int space = INPUT_BUFFER_SIZE - buffer_end;
    if (space > 0) {
        ssize_t nread = read(STDIN_FILENO, input_buffer + buffer_end,
                             (size_t)space);
        if (nread > 0) {
            buffer_end += (int)nread;
        }
    }
}

// Read a single character with smart timeout
int read_char_timeout(void) {
    // Return from buffer if available
    if (buffer_start < buffer_end) {
        return input_buffer[buffer_start++];
    }

    // Try to fill buffer
    fill_input_buffer();
    if (buffer_start < buffer_end) {
        return input_buffer[buffer_start++];
    }

    // No data available - wait with select()
    fd_set readfds;
    struct timeval tv;

    FD_ZERO(&readfds);
    FD_SET(STDIN_FILENO, &readfds);
    tv.tv_sec = 0;
    tv.tv_usec = 50000;  // 50ms

    int ret = select(STDIN_FILENO + 1, &readfds, NULL, NULL, &tv);
    if (ret > 0) {
        fill_input_buffer();
        if (buffer_start < buffer_end) {
            return input_buffer[buffer_start++];
        }
    }

    return -1;  // No input
}

// Read a character with very short timeout (for escape sequences)
int read_char_escape(void) {
    // Return from buffer if available
    if (buffer_start < buffer_end) {
        return input_buffer[buffer_start++];
    }

    // Try immediate read first
    fill_input_buffer();
    if (buffer_start < buffer_end) {
        return input_buffer[buffer_start++];
    }

    // Short wait for escape sequence continuation (5ms)
    fd_set readfds;
    struct timeval tv;

    FD_ZERO(&readfds);
    FD_SET(STDIN_FILENO, &readfds);
    tv.tv_sec = 0;
    tv.tv_usec = 5000;  // 5ms

    int ret = select(STDIN_FILENO + 1, &readfds, NULL, NULL, &tv);
    if (ret > 0) {
        fill_input_buffer();
        if (buffer_start < buffer_end) {
            return input_buffer[buffer_start++];
        }
    }

    return -1;
}

// Public function to flush input buffer (callable from Fortran)
void flush_input_buffer(void) {
    flush_input();
}

// Return and clear the SIGWINCH latch. sig_atomic_t is the only shared state;
// all querying and rendering remains in the ordinary main-loop context.
int take_terminal_resize_event(void) {
    sigset_t resize_set;
    sigset_t old_set;
    int blocked;
    int pending;

    sigemptyset(&resize_set);
    sigaddset(&resize_set, SIGWINCH);
    blocked = sigprocmask(SIG_BLOCK, &resize_set, &old_set) == 0;

    pending = terminal_resize_pending != 0;
    terminal_resize_pending = 0;

    // If SIGWINCH arrived while blocked, restoring the mask delivers it and
    // leaves the latch set for the next main-loop pass instead of losing it
    // between the read and clear above.
    if (blocked) sigprocmask(SIG_SETMASK, &old_set, NULL);
    return pending;
}

// ============================================================
// Output buffering for flicker-free rendering
// ============================================================

#define OUTPUT_BUFFER_SIZE (256 * 1024)  // 256KB - enough for any frame
static char output_buf[OUTPUT_BUFFER_SIZE];
static int output_pos = 0;

// Append data to the output buffer
void term_buf_write(const char *data, int len) {
    if (len <= 0) return;
    // If this write would overflow, flush first
    if (output_pos + len > OUTPUT_BUFFER_SIZE) {
        if (output_pos > 0) {
            write_all(STDOUT_FILENO, output_buf, (size_t)output_pos);
            output_pos = 0;
        }
    }
    // If single write is larger than buffer, write directly
    if (len > OUTPUT_BUFFER_SIZE) {
        write_all(STDOUT_FILENO, data, (size_t)len);
        return;
    }
    memcpy(output_buf + output_pos, data, (size_t)len);
    output_pos += len;
}

// Flush the output buffer to stdout in one write() syscall
void term_buf_flush(void) {
    if (output_pos > 0) {
        write_all(STDOUT_FILENO, output_buf, (size_t)output_pos);
        output_pos = 0;
    }
}

// Get terminal size using ioctl (no escape sequences needed)
void get_terminal_size(int *rows, int *cols) {
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0) {
        *rows = ws.ws_row;
        *cols = ws.ws_col;
    } else {
        // Fallback to defaults
        *rows = 24;
        *cols = 80;
    }
}

#endif
