// C wrapper for termios functions to enable raw mode in Fortran
#include <termios.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <sys/ioctl.h>

static struct termios orig_termios;
static int raw_mode_enabled = 0;

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

    // Control characters: minimum bytes and timeout for read()
    raw.c_cc[VMIN] = 0;  // Return each byte, or zero for timeout
    raw.c_cc[VTIME] = 1; // 100ms timeout (unit is 1/10 second)

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) {
        return -1;
    }

    raw_mode_enabled = 1;
    return 0;
}

// Disable raw mode - returns 0 on success, -1 on failure
int disable_raw_mode(void) {
    if (!raw_mode_enabled) return 0;

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios) == -1) {
        return -1;
    }

    raw_mode_enabled = 0;
    return 0;
}

// Check if input is available (non-blocking)
int input_available(void) {
    int nread;
    if (ioctl(STDIN_FILENO, FIONREAD, &nread) == -1) {
        return 0;
    }
    return nread > 0;
}

// Read a single character (with timeout)
int read_char_timeout(void) {
    char c;
    ssize_t nread = read(STDIN_FILENO, &c, 1);
    if (nread == 1) {
        return (unsigned char)c;
    } else {
        return -1; // No input or error
    }
}
