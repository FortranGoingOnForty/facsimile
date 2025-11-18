#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>

typedef struct {
    pid_t pid;
    int stdin_fd;
    int stdout_fd;
    int stderr_fd;
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

        // Execute the command
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

    // Make stdout non-blocking
    int flags = fcntl(proc->stdout_fd, F_GETFL, 0);
    fcntl(proc->stdout_fd, F_SETFL, flags | O_NONBLOCK);

    return proc;
}

// Send data to LSP server
int lsp_send_message(lsp_process_t* proc, const char* message, int len) {
    if (!proc || proc->stdin_fd < 0) return -1;

    int written = write(proc->stdin_fd, message, len);
    if (written < 0) {
        if (errno != EAGAIN && errno != EWOULDBLOCK) {
            return -1;
        }
        return 0;
    }

    return written;
}

// Read data from LSP server (non-blocking)
int lsp_read_message(lsp_process_t* proc, char* buffer, int max_len) {
    if (!proc || proc->stdout_fd < 0) return -1;

    int bytes_read = read(proc->stdout_fd, buffer, max_len - 1);
    if (bytes_read < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return 0;  // No data available
        }
        return -1;  // Error
    }

    if (bytes_read > 0) {
        buffer[bytes_read] = '\0';
    }

    return bytes_read;
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

// Fortran-callable wrappers
void lsp_start_server_f(const char* command, int command_len, void** handle) {
    char cmd[1024];
    int len = command_len < 1023 ? command_len : 1023;
    strncpy(cmd, command, len);
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
    return lsp_get_pid((lsp_process_t*)*handle);
}