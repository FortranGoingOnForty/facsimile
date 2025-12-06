// C wrapper for termios functions to enable raw mode in Fortran
#include <termios.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#include <string.h>

static struct termios orig_termios;
static int raw_mode_enabled = 0;

// Input buffer for batching reads
#define INPUT_BUFFER_SIZE 256
static unsigned char input_buffer[INPUT_BUFFER_SIZE];
static int buffer_start = 0;
static int buffer_end = 0;

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
    return 0;
}

// Disable raw mode - returns 0 on success, -1 on failure
int disable_raw_mode(void) {
    if (!raw_mode_enabled) return 0;

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
        memmove(input_buffer, input_buffer + buffer_start, buffer_end - buffer_start);
        buffer_end -= buffer_start;
        buffer_start = 0;
    } else if (buffer_start >= buffer_end) {
        buffer_start = buffer_end = 0;
    }

    // Read all available data into buffer
    int space = INPUT_BUFFER_SIZE - buffer_end;
    if (space > 0) {
        ssize_t nread = read(STDIN_FILENO, input_buffer + buffer_end, space);
        if (nread > 0) {
            buffer_end += nread;
        }
    }
}

// Read a single character with smart timeout
// - If data is buffered or available, return immediately
// - Otherwise wait up to timeout_ms for input
// - Use short timeout (5ms) for escape sequence continuation
// - Use longer timeout (50ms) for initial wait when idle
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
    // Use 50ms timeout for responsive feel without busy-waiting
    fd_set readfds;
    struct timeval tv;

    FD_ZERO(&readfds);
    FD_SET(STDIN_FILENO, &readfds);
    tv.tv_sec = 0;
    tv.tv_usec = 50000;  // 50ms - good balance of responsiveness and CPU usage

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
// This is used when we've already seen ESC and are looking for the rest
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
    tv.tv_usec = 5000;  // 5ms - fast escape sequence detection

    int ret = select(STDIN_FILENO + 1, &readfds, NULL, NULL, &tv);
    if (ret > 0) {
        fill_input_buffer();
        if (buffer_start < buffer_end) {
            return input_buffer[buffer_start++];
        }
    }

    return -1;
}
