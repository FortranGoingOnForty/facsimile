// PTY wrapper for integrated terminal
// Provides pseudo-terminal spawning, I/O, and lifecycle management

#ifdef _WIN32
// Windows stubs — integrated terminal not supported on Windows yet
#include <stdio.h>
#include <string.h>

void pty_spawn_f(const char *shell, int *shell_len,
                 int *rows, int *cols,
                 void **handle, int *error) {
    (void)shell; (void)shell_len; (void)rows; (void)cols;
    *handle = NULL;
    *error = -1;
}

int pty_read_f(void **handle, char *buffer, int *bufsize) {
    (void)handle; (void)buffer; (void)bufsize;
    return -1;
}

int pty_write_f(void **handle, const char *data, int *len) {
    (void)handle; (void)data; (void)len;
    return -1;
}

int pty_resize_f(void **handle, int *rows, int *cols) {
    (void)handle; (void)rows; (void)cols;
    return -1;
}

int pty_is_running_f(void **handle) {
    (void)handle;
    return 0;
}

void pty_close_f(void **handle) {
    (void)handle;
}

#else
// Unix implementation

#define _XOPEN_SOURCE 600
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <sys/select.h>
#include <sys/time.h>

typedef struct {
    int master_fd;
    pid_t child_pid;
    int running;
} pty_state_t;

// DA response strings
static const char DA_PRIMARY[] = "\x1b[?62;22c";
static const char DA_SECONDARY[] = "\x1b[>41;1;0c";

// Handle terminal capability queries during shell startup.
// Called synchronously on the blocking fd before it's set non-blocking.
// Blocks for at most ~1.5 seconds, but returns early once DA is answered.
static void handle_startup_queries(int fd) {
    char buf[4096];
    struct timeval start, now;
    gettimeofday(&start, NULL);

    for (;;) {
        gettimeofday(&now, NULL);
        double elapsed = (now.tv_sec - start.tv_sec) +
                          (now.tv_usec - start.tv_usec) / 1e6;
        if (elapsed > 1.5) break;

        fd_set rfds;
        struct timeval tv;
        FD_ZERO(&rfds);
        FD_SET(fd, &rfds);
        tv.tv_sec = 0;
        tv.tv_usec = 20000; // 20ms

        int ret = select(fd + 1, &rfds, NULL, NULL, &tv);
        if (ret <= 0) continue;

        ssize_t n = read(fd, buf, sizeof(buf));
        if (n <= 0) continue;

        int responded = 0;
        for (int i = 0; i < n - 1; i++) {
            if (buf[i] != 0x1b || buf[i+1] != '[') continue;

            // Scan forward from ESC[ for DA-like sequences
            // ending in 'c': ESC[c, ESC[0c, ESC[>0c, ESC[?1c
            for (int j = i + 2; j < n && j < i + 12; j++) {
                if (buf[j] == 'c') {
                    // Check if this is a DA query (not a response)
                    // Queries: ESC[c ESC[0c ESC[>0c ESC[>c
                    if (j == i + 2 || buf[i+2] == '0' ||
                        buf[i+2] == '?') {
                        write(fd, DA_PRIMARY,
                              sizeof(DA_PRIMARY) - 1);
                        responded = 1;
                    } else if (buf[i+2] == '>') {
                        write(fd, DA_SECONDARY,
                              sizeof(DA_SECONDARY) - 1);
                        responded = 1;
                    }
                    break;
                }
                // Stop scanning at non-parameter bytes
                if (buf[j] < '0' || buf[j] > '?') break;
            }
        }
        if (responded) {
            // Give shell a moment to process, then return
            usleep(50000);
            break;
        }
    }
}

// Spawn a shell in a new PTY
void pty_spawn_f(const char *shell, int *shell_len,
                 int *rows, int *cols,
                 void **handle, int *error) {
    pty_state_t *state;
    int master_fd, slave_fd;
    pid_t pid;
    char *slave_name;
    struct winsize ws;
    char shell_path[512];
    int exec_err_pipe[2];

    *handle = NULL;
    *error = 0;

    // Copy shell path (Fortran strings are not null-terminated)
    if (*shell_len >= (int)sizeof(shell_path)) {
        *error = -1;
        return;
    }
    memcpy(shell_path, shell, *shell_len);
    shell_path[*shell_len] = '\0';

    // Trim trailing spaces (Fortran pads strings)
    int slen = *shell_len;
    while (slen > 0 && shell_path[slen - 1] == ' ') {
        shell_path[--slen] = '\0';
    }

    // Set up window size
    ws.ws_row = *rows;
    ws.ws_col = *cols;
    ws.ws_xpixel = 0;
    ws.ws_ypixel = 0;

    // Create exec-error pipe (close-on-exec)
    if (pipe(exec_err_pipe) < 0) {
        *error = -2;
        return;
    }
    fcntl(exec_err_pipe[1], F_SETFD, FD_CLOEXEC);

    // Open PTY master
    master_fd = posix_openpt(O_RDWR | O_NOCTTY);
    if (master_fd < 0) {
        close(exec_err_pipe[0]);
        close(exec_err_pipe[1]);
        *error = -3;
        return;
    }

    if (grantpt(master_fd) < 0 || unlockpt(master_fd) < 0) {
        close(master_fd);
        close(exec_err_pipe[0]);
        close(exec_err_pipe[1]);
        *error = -4;
        return;
    }

    slave_name = ptsname(master_fd);
    if (!slave_name) {
        close(master_fd);
        close(exec_err_pipe[0]);
        close(exec_err_pipe[1]);
        *error = -5;
        return;
    }

    // Fork
    pid = fork();
    if (pid < 0) {
        close(master_fd);
        close(exec_err_pipe[0]);
        close(exec_err_pipe[1]);
        *error = -6;
        return;
    }

    if (pid == 0) {
        // === Child process ===
        close(exec_err_pipe[0]); // Close read end
        close(master_fd);

        // Create new session
        setsid();

        // Open slave PTY
        slave_fd = open(slave_name, O_RDWR);
        if (slave_fd < 0) {
            int err = errno;
            write(exec_err_pipe[1], &err, sizeof(err));
            _exit(127);
        }

        // Set as controlling terminal
#ifdef TIOCSCTTY
        ioctl(slave_fd, TIOCSCTTY, 0);
#endif

        // Set window size
        ioctl(slave_fd, TIOCSWINSZ, &ws);

        // Redirect stdio
        dup2(slave_fd, STDIN_FILENO);
        dup2(slave_fd, STDOUT_FILENO);
        dup2(slave_fd, STDERR_FILENO);
        if (slave_fd > STDERR_FILENO) {
            close(slave_fd);
        }

        // Suppress fish greeting
        setenv("fish_greeting", "", 1);

        // Set TERM
        setenv("TERM", "xterm-256color", 1);

        // Exec shell as login shell
        // Build argv[0] with leading dash for login shell
        const char *base = strrchr(shell_path, '/');
        if (!base) base = shell_path; else base++;

        char argv0[64];
        snprintf(argv0, sizeof(argv0), "-%.*s", (int)(sizeof(argv0) - 2), base);

        execl(shell_path, argv0, (char *)NULL);

        // exec failed — signal parent via pipe
        int err = errno;
        write(exec_err_pipe[1], &err, sizeof(err));
        _exit(127);
    }

    // === Parent process ===
    close(exec_err_pipe[1]); // Close write end

    // Check if exec succeeded (pipe closes on successful exec)
    int child_err = 0;
    ssize_t n = read(exec_err_pipe[0], &child_err, sizeof(child_err));
    close(exec_err_pipe[0]);

    if (n > 0) {
        // Child failed to exec
        close(master_fd);
        waitpid(pid, NULL, 0);
        *error = -7;
        return;
    }

    // Handle DA queries synchronously while fd is still blocking.
    // This catches fish's PDA query and responds before the 2s timeout.
    handle_startup_queries(master_fd);

    // NOW set master to non-blocking for normal operation
    int flags = fcntl(master_fd, F_GETFL, 0);
    fcntl(master_fd, F_SETFL, flags | O_NONBLOCK);

    // Allocate state
    state = (pty_state_t *)malloc(sizeof(pty_state_t));
    state->master_fd = master_fd;
    state->child_pid = pid;
    state->running = 1;

    *handle = state;
}

// Non-blocking read from PTY
// Returns bytes read, 0 if no data, -1 on error/EOF
int pty_read_f(void **handle, char *buffer, int *bufsize) {
    pty_state_t *state = (pty_state_t *)*handle;
    if (!state || state->master_fd < 0) return -1;

    ssize_t n = read(state->master_fd, buffer, *bufsize);
    if (n < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return 0; // No data available
        }
        return -1; // Error
    }
    if (n == 0) {
        return -1; // EOF — child closed
    }
    return (int)n;
}

// Write data to PTY
int pty_write_f(void **handle, const char *data, int *len) {
    pty_state_t *state = (pty_state_t *)*handle;
    if (!state || state->master_fd < 0) return -1;

    ssize_t n = write(state->master_fd, data, *len);
    if (n < 0) return -1;
    return (int)n;
}

// Resize PTY
int pty_resize_f(void **handle, int *rows, int *cols) {
    pty_state_t *state = (pty_state_t *)*handle;
    if (!state || state->master_fd < 0) return -1;

    struct winsize ws;
    ws.ws_row = *rows;
    ws.ws_col = *cols;
    ws.ws_xpixel = 0;
    ws.ws_ypixel = 0;

    if (ioctl(state->master_fd, TIOCSWINSZ, &ws) < 0) {
        return -1;
    }
    return 0;
}

// Check if child is still running (1=yes, 0=no)
int pty_is_running_f(void **handle) {
    pty_state_t *state = (pty_state_t *)*handle;
    if (!state) return 0;
    if (!state->running) return 0;

    int status;
    pid_t result = waitpid(state->child_pid, &status, WNOHANG);
    if (result == state->child_pid) {
        state->running = 0;
        return 0;
    }
    if (result < 0) {
        state->running = 0;
        return 0;
    }
    return 1; // Still running
}

// Close PTY and kill child
void pty_close_f(void **handle) {
    pty_state_t *state = (pty_state_t *)*handle;
    if (!state) return;

    if (state->master_fd >= 0) {
        close(state->master_fd);
        state->master_fd = -1;
    }

    if (state->running && state->child_pid > 0) {
        kill(state->child_pid, SIGHUP);
        usleep(50000); // 50ms grace
        kill(state->child_pid, SIGKILL);
        waitpid(state->child_pid, NULL, 0);
        state->running = 0;
    }

    free(state);
    *handle = NULL;
}

#endif
