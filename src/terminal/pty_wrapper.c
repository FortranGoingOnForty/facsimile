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

// Nothing to advertise to: there is no terminal panel here to spawn a shell
// in, so no shell can ever be a client. The stub exists because the Fortran
// side binds it unconditionally, and a bind with no symbol is a LINK error --
// which is how this was missed until a Windows build ran.
void pty_set_session_f(const char *dir, int *dir_len) {
    (void)dir; (void)dir_len;
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

int pty_get_fd_f(void **handle) {
    (void)handle;
    return -1;
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

#define STARTUP_BUF_SIZE 16384

typedef struct {
    int master_fd;
    pid_t child_pid;
    int running;
    // Buffered startup data (consumed during DA query handling)
    char startup_buf[STARTUP_BUF_SIZE];
    int startup_len;
    int startup_pos;
} pty_state_t;

// Terminal query response strings
static const char DA_PRIMARY[] = "\x1b[?62;22c";
static const char DA_SECONDARY[] = "\x1b[>41;1;0c";
static const char XTVERSION[] = "\x1bP>|facsimile(0.10)\x1b\\";
// Keyboard protocol: not supported (mode 0)
static const char KEYBOARD_PROTO[] = "\x1b[?0u";
// Background color response (dark theme)
static const char BG_COLOR[] = "\x1b]11;rgb:1e1e/1e1e/1e1e\x1b\\";

// Scan a buffer for terminal queries and respond to them.
// Called on EVERY read from the PTY, not just startup.
static void respond_to_queries(int fd, const char *buf, int n) {
    for (int i = 0; i < n - 1; i++) {
        if (buf[i] != 0x1b) continue;

        // DCS: ESC P+q<hex>ESC\ — XTGETTCAP
        if (buf[i+1] == 'P' && i + 4 < n &&
            buf[i+2] == '+' && buf[i+3] == 'q') {
            int key_start = i + 4;
            int key_end = key_start;
            while (key_end < n - 1) {
                if (buf[key_end] == 0x1b || buf[key_end] == 0x07)
                    break;
                key_end++;
            }
            if (key_end > key_start) {
                char resp[256];
                int rlen = 0;
                resp[rlen++] = 0x1b; resp[rlen++] = 'P';
                resp[rlen++] = '0'; resp[rlen++] = '+';
                resp[rlen++] = 'r';
                int klen = key_end - key_start;
                if (klen > 200) klen = 200;
                memcpy(resp + rlen, buf + key_start, (size_t)klen);
                rlen += klen;
                resp[rlen++] = 0x1b; resp[rlen++] = '\\';
                write_all(fd, resp, (size_t)rlen);
            }
        }

        // CSI sequences
        if (buf[i+1] == '[') {
            // DA queries ending in 'c'
            for (int j = i+2; j < n && j < i+12; j++) {
                if (buf[j] == 'c') {
                    if (j == i+2 || buf[i+2] == '0' ||
                        buf[i+2] == '?') {
                        write_all(fd, DA_PRIMARY,
                              sizeof(DA_PRIMARY) - 1);
                    } else if (buf[i+2] == '>') {
                        write_all(fd, DA_SECONDARY,
                              sizeof(DA_SECONDARY) - 1);
                    }
                    break;
                }
                if (buf[j] < '0' || buf[j] > '?') break;
            }
            // ESC[?u — keyboard protocol query
            if (i+2 < n && buf[i+2] == '?') {
                for (int j = i+3; j < n && j < i+8; j++) {
                    if (buf[j] == 'u') {
                        write_all(fd, KEYBOARD_PROTO,
                              sizeof(KEYBOARD_PROTO) - 1);
                        break;
                    }
                }
            }
            // ESC[>...q — XTVERSION
            if (i+2 < n && buf[i+2] == '>') {
                for (int j = i+3; j < n && j < i+8; j++) {
                    if (buf[j] == 'q') {
                        write_all(fd, XTVERSION,
                              sizeof(XTVERSION) - 1);
                        break;
                    }
                }
            }
        }

        // OSC: ESC]11;? — background color query
        if (buf[i+1] == ']' && i+4 < n &&
            buf[i+2] == '1' && buf[i+3] == '1' &&
            buf[i+4] == ';') {
            write_all(fd, BG_COLOR, sizeof(BG_COLOR) - 1);
        }
    }
}

// Handle terminal capability queries during shell startup.
// Saves all consumed data to state->startup_buf so pty_read_f
// can replay it to the grid. Returns early once DA is answered.
static void handle_startup_queries(pty_state_t *state) {
    int fd = state->master_fd;
    char buf[4096];
    struct timeval start, now;
    gettimeofday(&start, NULL);

    state->startup_len = 0;
    state->startup_pos = 0;

    int idle_count = 0;
    for (;;) {
        gettimeofday(&now, NULL);
        double elapsed = (double)(now.tv_sec - start.tv_sec) +
                          (double)(now.tv_usec - start.tv_usec) / 1e6;
        if (elapsed > 3.0) break;

        fd_set rfds;
        struct timeval tv;
        FD_ZERO(&rfds);
        FD_SET(fd, &rfds);
        tv.tv_sec = 0;
        tv.tv_usec = 10000; // 10ms

        int ret = select(fd + 1, &rfds, NULL, NULL, &tv);
        if (ret <= 0) {
            idle_count++;
            if (idle_count > 30) break; // 300ms of silence = done
            continue;
        }

        idle_count = 0;
        ssize_t n = read(fd, buf, sizeof(buf));
        if (n <= 0) continue;

        // Save to startup buffer for replay
        int copy = (int)n;
        if (state->startup_len + copy > STARTUP_BUF_SIZE)
            copy = STARTUP_BUF_SIZE - state->startup_len;
        if (copy > 0) {
            memcpy(state->startup_buf + state->startup_len,
                   buf, (size_t)copy);
            state->startup_len += copy;
        }

        // Respond to queries using shared helper
        respond_to_queries(fd, buf, (int)n);
    }
}

// The session spool directory advertised to the shell as FAC_SESSION.
//
// Set on the CHILD only, never on our own environment. A fac started from the
// panel must see it so it can hand its argument back to us, but WE must not,
// or a fac launched normally from a directory we happen to have spawned would
// mistake itself for a client. Nothing here is inherited by us.
static char g_session_dir[512] = {0};

void pty_set_session_f(const char *dir, int *dir_len) {
    int n = *dir_len;

    if (n < 0) n = 0;
    if (n >= (int)sizeof(g_session_dir)) n = (int)sizeof(g_session_dir) - 1;
    memcpy(g_session_dir, dir, (size_t)n);
    g_session_dir[n] = '\0';
    while (n > 0 && g_session_dir[n - 1] == ' ') {
        g_session_dir[--n] = '\0';
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
    memcpy(shell_path, shell, (size_t)*shell_len);
    shell_path[*shell_len] = '\0';

    // Trim trailing spaces (Fortran pads strings)
    int slen = *shell_len;
    while (slen > 0 && shell_path[slen - 1] == ' ') {
        shell_path[--slen] = '\0';
    }

    // Set up window size
    ws.ws_row = (unsigned short)*rows;
    ws.ws_col = (unsigned short)*cols;
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
            write_all(exec_err_pipe[1], &err, sizeof(err));
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

        // Tell anything started here how to reach the editor that owns it
        if (g_session_dir[0]) {
            setenv("FAC_SESSION", g_session_dir, 1);
        }

        // Exec shell as login shell
        // Build argv[0] with leading dash for login shell
        const char *base = strrchr(shell_path, '/');
        if (!base) base = shell_path; else base++;

        char argv0[64];
        snprintf(argv0, sizeof(argv0), "-%.*s", (int)(sizeof(argv0) - 2), base);

        execl(shell_path, argv0, (char *)NULL);

        // exec failed — signal parent via pipe
        int err = errno;
        write_all(exec_err_pipe[1], &err, sizeof(err));
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

    // Allocate state first so startup handler can buffer data
    state = (pty_state_t *)malloc(sizeof(pty_state_t));
    state->master_fd = master_fd;
    state->child_pid = pid;
    state->running = 1;
    state->startup_len = 0;
    state->startup_pos = 0;

    // Handle DA queries synchronously while fd is still blocking.
    handle_startup_queries(state);

    // NOW set master to non-blocking for normal operation
    int flags = fcntl(master_fd, F_GETFL, 0);
    fcntl(master_fd, F_SETFL, flags | O_NONBLOCK);

    *handle = state;
}

// Non-blocking read from PTY
// Returns buffered startup data first, then live reads.
int pty_read_f(void **handle, char *buffer, int *bufsize) {
    pty_state_t *state = (pty_state_t *)*handle;
    if (!state || state->master_fd < 0) return -1;

    // Return buffered startup data first
    if (state->startup_pos < state->startup_len) {
        int avail = state->startup_len - state->startup_pos;
        int copy = avail < *bufsize ? avail : *bufsize;
        memcpy(buffer, state->startup_buf + state->startup_pos,
               (size_t)copy);
        state->startup_pos += copy;
        return copy;
    }

    ssize_t n = read(state->master_fd, buffer, (size_t)*bufsize);
    if (n < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return 0;
        }
        return -1;
    }
    if (n == 0) {
        return -1;
    }

    // Respond to any terminal queries in live data too
    respond_to_queries(state->master_fd, buffer, (int)n);

    return (int)n;
}

// Write data to PTY
int pty_write_f(void **handle, const char *data, int *len) {
    pty_state_t *state = (pty_state_t *)*handle;
    if (!state || state->master_fd < 0) return -1;

    ssize_t n = write(state->master_fd, data, (size_t)*len);
    if (n < 0) return -1;
    return (int)n;
}

// Get the PTY master fd (for grid inline responses)
int pty_get_fd_f(void **handle) {
    pty_state_t *state = (pty_state_t *)*handle;
    if (!state) return -1;
    return state->master_fd;
}

// Resize PTY
int pty_resize_f(void **handle, int *rows, int *cols) {
    pty_state_t *state = (pty_state_t *)*handle;
    if (!state || state->master_fd < 0) return -1;

    struct winsize ws;
    ws.ws_row = (unsigned short)*rows;
    ws.ws_col = (unsigned short)*cols;
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
