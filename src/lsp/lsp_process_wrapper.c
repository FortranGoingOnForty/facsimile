// LSP Process wrapper - platform independent implementation
// Handles process creation, pipes, and I/O for LSP servers

#ifdef _WIN32
// Windows implementation
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    HANDLE hProcess;
    DWORD pid;
    HANDLE stdin_write;
    HANDLE stdout_read;
    HANDLE stderr_read;
} lsp_process_t;

// Get temp directory path
static const char* get_temp_dir(void) {
    static char temp_path[MAX_PATH];
    GetTempPathA(MAX_PATH, temp_path);
    return temp_path;
}

// Start an LSP server process
lsp_process_t* lsp_start_server(const char* command) {
    lsp_process_t* proc = (lsp_process_t*)malloc(sizeof(lsp_process_t));
    if (!proc) return NULL;

    memset(proc, 0, sizeof(lsp_process_t));

    // Create pipes for stdin, stdout, stderr
    HANDLE stdin_read, stdin_write;
    HANDLE stdout_read, stdout_write;
    HANDLE stderr_read, stderr_write;

    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = TRUE;
    sa.lpSecurityDescriptor = NULL;

    if (!CreatePipe(&stdin_read, &stdin_write, &sa, 0)) {
        free(proc);
        return NULL;
    }
    // Don't inherit the write end of stdin
    SetHandleInformation(stdin_write, HANDLE_FLAG_INHERIT, 0);

    if (!CreatePipe(&stdout_read, &stdout_write, &sa, 0)) {
        CloseHandle(stdin_read);
        CloseHandle(stdin_write);
        free(proc);
        return NULL;
    }
    // Don't inherit the read end of stdout
    SetHandleInformation(stdout_read, HANDLE_FLAG_INHERIT, 0);

    if (!CreatePipe(&stderr_read, &stderr_write, &sa, 0)) {
        CloseHandle(stdin_read);
        CloseHandle(stdin_write);
        CloseHandle(stdout_read);
        CloseHandle(stdout_write);
        free(proc);
        return NULL;
    }
    // Don't inherit the read end of stderr
    SetHandleInformation(stderr_read, HANDLE_FLAG_INHERIT, 0);

    // Set up process startup info
    STARTUPINFOA si;
    PROCESS_INFORMATION pi;

    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    si.hStdInput = stdin_read;
    si.hStdOutput = stdout_write;
    si.hStdError = stderr_write;
    si.dwFlags |= STARTF_USESTDHANDLES;

    ZeroMemory(&pi, sizeof(pi));

    // Build command line for cmd.exe
    char cmd_line[2048];
    snprintf(cmd_line, sizeof(cmd_line), "cmd.exe /c %s", command);

    // Create the process
    if (!CreateProcessA(
            NULL,           // Application name (use command line)
            cmd_line,       // Command line
            NULL,           // Process security attributes
            NULL,           // Thread security attributes
            TRUE,           // Inherit handles
            CREATE_NO_WINDOW, // Don't create a console window
            NULL,           // Environment (inherit)
            NULL,           // Current directory (inherit)
            &si,            // Startup info
            &pi             // Process info
        )) {
        CloseHandle(stdin_read);
        CloseHandle(stdin_write);
        CloseHandle(stdout_read);
        CloseHandle(stdout_write);
        CloseHandle(stderr_read);
        CloseHandle(stderr_write);
        free(proc);
        return NULL;
    }

    // Close handles we don't need
    CloseHandle(stdin_read);
    CloseHandle(stdout_write);
    CloseHandle(stderr_write);
    CloseHandle(pi.hThread);

    proc->hProcess = pi.hProcess;
    proc->pid = pi.dwProcessId;
    proc->stdin_write = stdin_write;
    proc->stdout_read = stdout_read;
    proc->stderr_read = stderr_read;

    // Reads use PeekNamedPipe rather than overlapped I/O, which is where
    // the non-blocking behaviour comes from on this platform.

    return proc;
}

// Send data to LSP server
int lsp_send_message(lsp_process_t* proc, const char* message, int len) {
    if (!proc || proc->stdin_write == INVALID_HANDLE_VALUE) return -1;

    DWORD written;
    if (!WriteFile(proc->stdin_write, message, (DWORD)len, &written, NULL)) {
        return -1;
    }

    return (int)written;
}

// Read data from LSP server (non-blocking)
int lsp_read_message(lsp_process_t* proc, char* buffer, int max_len) {
    if (!proc || proc->stdout_read == INVALID_HANDLE_VALUE) return -1;

    // Check if data is available
    DWORD available = 0;
    if (!PeekNamedPipe(proc->stdout_read, NULL, 0, NULL, &available, NULL)) {
        return -1;
    }

    if (available == 0) {
        return 0;  // No data available
    }

    // Read available data
    DWORD bytes_read;
    DWORD to_read = (available < (DWORD)(max_len - 1)) ? available : (DWORD)(max_len - 1);

    if (!ReadFile(proc->stdout_read, buffer, to_read, &bytes_read, NULL)) {
        return -1;
    }

    if (bytes_read > 0) {
        buffer[bytes_read] = '\0';
    }

    return (int)bytes_read;
}

// Check if process is still running
int lsp_is_running(lsp_process_t* proc) {
    if (!proc || proc->hProcess == INVALID_HANDLE_VALUE) return 0;

    DWORD exit_code;
    if (!GetExitCodeProcess(proc->hProcess, &exit_code)) {
        return 0;
    }

    return (exit_code == STILL_ACTIVE) ? 1 : 0;
}

// Stop LSP server
void lsp_stop_server(lsp_process_t* proc) {
    if (!proc) return;

    if (proc->stdin_write != INVALID_HANDLE_VALUE) {
        CloseHandle(proc->stdin_write);
        proc->stdin_write = INVALID_HANDLE_VALUE;
    }

    if (proc->stdout_read != INVALID_HANDLE_VALUE) {
        CloseHandle(proc->stdout_read);
        proc->stdout_read = INVALID_HANDLE_VALUE;
    }

    if (proc->stderr_read != INVALID_HANDLE_VALUE) {
        CloseHandle(proc->stderr_read);
        proc->stderr_read = INVALID_HANDLE_VALUE;
    }

    if (proc->hProcess != INVALID_HANDLE_VALUE) {
        // Try graceful termination first
        TerminateProcess(proc->hProcess, 0);
        WaitForSingleObject(proc->hProcess, 1000);  // Wait up to 1 second
        CloseHandle(proc->hProcess);
        proc->hProcess = INVALID_HANDLE_VALUE;
    }

    free(proc);
}

// Get process ID
DWORD lsp_get_pid(lsp_process_t* proc) {
    return proc ? proc->pid : 0;
}

#else
// Unix implementation
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>
#include <poll.h>

/* Cap on what we will hold for a server that is not reading. A server that
   lets this much pile up is not coming back. */
#define LSP_PENDING_MAX (1 << 20)

typedef struct {
    pid_t pid;
    int stdin_fd;
    int stdout_fd;
    int stderr_fd;
    /* Anything the server was not ready to take. Flushed from the
       editor's pump, never from the keystroke path. */
    char*  pending;
    size_t pending_len;
    size_t pending_cap;
    int    wedged;          /* pending overflowed: stop trying */
} lsp_process_t;

// Start an LSP server process
lsp_process_t* lsp_start_server(const char* command) {
    lsp_process_t* proc = malloc(sizeof(lsp_process_t));
    if (!proc) return NULL;

    // Create pipes for stdin, stdout, stderr
    int stdin_pipe[2], stdout_pipe[2], stderr_pipe[2];

    if (pipe(stdin_pipe) < 0 || pipe(stdout_pipe) < 0 || pipe(stderr_pipe) < 0) {
        free(proc);
        return NULL;
    }

    pid_t pid = fork();
    if (pid < 0) {
        // Fork failed
        close(stdin_pipe[0]); close(stdin_pipe[1]);
        close(stdout_pipe[0]); close(stdout_pipe[1]);
        close(stderr_pipe[0]); close(stderr_pipe[1]);
        free(proc);
        return NULL;
    }

    if (pid == 0) {
        // Child process
        // Redirect stdin, stdout, stderr
        dup2(stdin_pipe[0], STDIN_FILENO);
        dup2(stdout_pipe[1], STDOUT_FILENO);
        dup2(stderr_pipe[1], STDERR_FILENO);

        // Close unused pipe ends
        close(stdin_pipe[1]);
        close(stdout_pipe[0]);
        close(stderr_pipe[0]);

        // Close original pipe fds
        close(stdin_pipe[0]);
        close(stdout_pipe[1]);
        close(stderr_pipe[1]);

        // Execute the command - use sh for simplicity
        execl("/bin/sh", "sh", "-c", command, NULL);

        // If we get here, exec failed
        fprintf(stderr, "Failed to execute: %s\n", command);
        exit(1);
    }

    // Parent process
    proc->pid = pid;
    proc->stdin_fd = stdin_pipe[1];
    proc->stdout_fd = stdout_pipe[0];
    proc->stderr_fd = stderr_pipe[0];

    // Close unused pipe ends
    close(stdin_pipe[0]);
    close(stdout_pipe[1]);
    close(stderr_pipe[1]);

    // Make stdout and stderr non-blocking
    int flags = fcntl(proc->stdout_fd, F_GETFL, 0);
    fcntl(proc->stdout_fd, F_SETFL, flags | O_NONBLOCK);
    flags = fcntl(proc->stderr_fd, F_GETFL, 0);
    fcntl(proc->stderr_fd, F_SETFL, flags | O_NONBLOCK);
    /* And stdin. Without this the editor DEADLOCKS: a blocking write fills
       the server's input pipe, the server stops reading because its own
       output pipe back to us is full, and we cannot drain that because we
       are inside the write. Confirmed from a hung editor -- two and a half
       hours in write(), the server parked in futex_do_wait. The EAGAIN
       handling below was always written for a non-blocking fd; only this
       line was missing. */
    flags = fcntl(proc->stdin_fd, F_GETFL, 0);
    fcntl(proc->stdin_fd, F_SETFL, flags | O_NONBLOCK);

    return proc;
}

// Send data to LSP server
/* How long we will keep trying to hand a message to a server before
   declaring it wedged. Generous: a busy server briefly stops reading all the
   time, and giving up on one of those would drop real requests. */
#define LSP_WRITE_BUDGET_MS 2000
#define LSP_WRITE_SLICE_MS   50

/* Push as much of the buffered remainder as the server will take.
 *
 * Called from the editor's message pump, the only place that may spend time
 * on a slow server.
 */
int lsp_flush_pending(lsp_process_t* proc) {
    if (!proc || proc->stdin_fd < 0) return -1;
    if (proc->wedged) return -1;
    while (proc->pending_len > 0) {
        ssize_t n = write(proc->stdin_fd, proc->pending, proc->pending_len);
        if (n > 0) {
            proc->pending_len -= (size_t)n;
            if (proc->pending_len > 0)
                memmove(proc->pending, proc->pending + n, proc->pending_len);
            continue;
        }
        if (n < 0 && errno == EINTR) continue;
        if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) return 0;
        return -1;
    }
    return 0;
}

static int lsp_queue(lsp_process_t* proc, const char* data, size_t len) {
    if (proc->pending_len + len > LSP_PENDING_MAX) {
        proc->wedged = 1;
        return -1;
    }
    if (proc->pending_len + len > proc->pending_cap) {
        size_t want = proc->pending_cap ? proc->pending_cap : 8192;
        while (want < proc->pending_len + len) want *= 2;
        char* grown = (char*)realloc(proc->pending, want);
        if (!grown) return -1;
        proc->pending = grown;
        proc->pending_cap = want;
    }
    memcpy(proc->pending + proc->pending_len, data, len);
    proc->pending_len += len;
    return 0;
}

/* Hand a message to the server WITHOUT waiting for it.
 *
 * This runs on the keystroke path. It used to spend up to two seconds here
 * when a server stopped reading, which turned typing into a slideshow --
 * measured at 162 keystrokes in 31 seconds against a stopped server.
 * Whatever the server will not take now is buffered and pushed later by the
 * pump, so the editor never waits on it.
 *
 * Ordering is preserved: once anything is queued everything after it is
 * queued too, or a later message could overtake an earlier one and
 * desynchronise the stream.
 */
int lsp_send_message(lsp_process_t* proc, const char* message, int len) {
    if (!proc || proc->stdin_fd < 0) return -1;
    if (len <= 0) return 0;
    if (proc->wedged) return -1;

    size_t off = 0;
    if (proc->pending_len == 0) {
        while (off < (size_t)len) {
            ssize_t n = write(proc->stdin_fd, message + off, (size_t)len - off);
            if (n > 0) { off += (size_t)n; continue; }
            if (n < 0 && errno == EINTR) continue;
            if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) break;
            return -1;
        }
    }
    if (off < (size_t)len) {
        if (lsp_queue(proc, message + off, (size_t)len - off) != 0) return -1;
    }
    return len;
}

// Read data from LSP server (non-blocking)
int lsp_read_message(lsp_process_t* proc, char* buffer, int max_len) {
    if (!proc || proc->stdout_fd < 0) return -1;

    /* This used to append to a log in /tmp on every read that returned data
       and on every hundredth that did not -- unbounded, and disk I/O in the
       middle of the keystroke path. It also READ FROM STDERR to log it,
       which quietly discarded whatever the server had said there. */
    ssize_t bytes_read = read(proc->stdout_fd, buffer, (size_t)(max_len - 1));

    if (bytes_read < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return 0;  // No data available
        }
        return -1;  // Error
    }

    if (bytes_read > 0) {
        buffer[bytes_read] = '\0';
    }

    return (int)bytes_read;
}

// Check if process is still running
int lsp_is_running(lsp_process_t* proc) {
    if (!proc || proc->pid <= 0) return 0;

    int status;
    pid_t result = waitpid(proc->pid, &status, WNOHANG);

    if (result == 0) {
        // Process is still running
        return 1;
    } else if (result == proc->pid) {
        // Process has exited
        proc->pid = -1;
        return 0;
    } else {
        // Error
        return 0;
    }
}

// Stop LSP server
void lsp_stop_server(lsp_process_t* proc) {
    if (!proc) return;

    if (proc->stdin_fd >= 0) {
        close(proc->stdin_fd);
        proc->stdin_fd = -1;
    }

    if (proc->stdout_fd >= 0) {
        close(proc->stdout_fd);
        proc->stdout_fd = -1;
    }

    if (proc->stderr_fd >= 0) {
        close(proc->stderr_fd);
        proc->stderr_fd = -1;
    }

    if (proc->pid > 0) {
        // Send SIGTERM first
        kill(proc->pid, SIGTERM);

        // Wait a bit for graceful shutdown
        usleep(100000);  // 100ms

        // Check if still running
        if (lsp_is_running(proc)) {
            // Force kill
            kill(proc->pid, SIGKILL);
            waitpid(proc->pid, NULL, 0);
        }

        proc->pid = -1;
    }

    free(proc);
}

// Get process ID
pid_t lsp_get_pid(lsp_process_t* proc) {
    return proc ? proc->pid : -1;
}

#endif

// Fortran-callable wrappers (platform independent)
void lsp_start_server_f(const char* command, int command_len, void** handle) {
    char cmd[1024];
    int len = command_len < 1023 ? command_len : 1023;
    strncpy(cmd, command, (size_t)len);
    cmd[len] = '\0';

    *handle = lsp_start_server(cmd);
}

void lsp_stop_server_f(void** handle) {
    if (*handle) {
        lsp_stop_server((lsp_process_t*)*handle);
        *handle = NULL;
    }
}

int lsp_send_message_f(void** handle, const char* message, int message_len) {
    if (!*handle) return -1;
    return lsp_send_message((lsp_process_t*)*handle, message, message_len);
}

int lsp_flush_pending_f(void** handle) {
    if (!*handle) return -1;
    return lsp_flush_pending((lsp_process_t*)*handle);
}

int lsp_read_message_f(void** handle, char* buffer, int buffer_len) {
    if (!*handle) return -1;
    return lsp_read_message((lsp_process_t*)*handle, buffer, buffer_len);
}

int lsp_is_running_f(void** handle) {
    if (!*handle) return 0;
    return lsp_is_running((lsp_process_t*)*handle);
}

int lsp_get_pid_f(void** handle) {
    if (!*handle) return -1;
#ifdef _WIN32
    return (int)lsp_get_pid((lsp_process_t*)*handle);
#else
    return lsp_get_pid((lsp_process_t*)*handle);
#endif
}
