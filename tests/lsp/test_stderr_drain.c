/* A language server's stderr must be read, or the server wedges.
 *
 * Found on a live session that had been open about an hour. clangd logs a
 * line per request -- "I[21:46:52.345] Code complete: ..." -- to stderr. The
 * wrapper created that pipe and never read it. A pipe holds 64 KiB; once it
 * was full, a clangd worker parked in write(2, ...) and stayed there. Having
 * blocked, it stopped reading its stdin too, so the editor's document syncs
 * backed up behind a 656 KB didChange that could never go out. Every
 * language feature died at once, and from the editor's side the server had
 * simply gone quiet.
 *
 * O_NONBLOCK on the read end does not help: it governs OUR reads, not the
 * server's writes.
 *
 * This test starts a "server" that logs far more to stderr than a pipe can
 * hold and only then answers on stdout. Without draining, the answer never
 * arrives -- the server is stuck on its log. With it, the answer comes back.
 *
 * Build/run: see the test-lsp target in the Makefile.
 */

#include <stdio.h>

#include <string.h>
#include <time.h>

typedef struct lsp_process lsp_process_t;

extern lsp_process_t* lsp_start_server(const char* command, const char* workdir);
extern int lsp_read_message(lsp_process_t* proc, char* buffer, int max_len);
extern int lsp_drain_stderr(lsp_process_t* proc);
extern void lsp_stop_server(lsp_process_t* proc);

/* Comfortably more than one pipe buffer. */
#define LOG_BYTES (256 * 1024)

/* Long enough that a merely slow machine is not mistaken for a wedged one. */
#define WAIT_SECONDS 5.0

static double elapsed_since(clock_t start) {
    return (double)(clock() - start) / CLOCKS_PER_SEC;
}

/* Poll for a reply for up to WAIT_SECONDS. `drain` says whether to do the
   thing under test. Returns 1 if the server's message arrived. */
static int wait_for_reply(lsp_process_t* proc, int drain) {
    char buf[8192];
    struct timespec nap = {0, 20 * 1000 * 1000};   /* 20ms */
    time_t deadline = time(NULL) + (time_t)WAIT_SECONDS + 1;

    while (time(NULL) < deadline) {
        if (drain) lsp_drain_stderr(proc);
        int n = lsp_read_message(proc, buf, (int)sizeof(buf));
        if (n > 0 && strstr(buf, "HELLO-FROM-SERVER")) return 1;
        nanosleep(&nap, NULL);
    }
    return 0;
}

int main(void) {
    int failures = 0;
    char cmd[512];

    /* Log a lot, THEN speak. A real server interleaves the two, but the
       ordering here is what makes the failure deterministic rather than
       dependent on how chatty the server happens to be. */
    snprintf(cmd, sizeof(cmd),
             "sh -c 'i=0; while [ $i -lt %d ]; do "
             "printf \"%%s\\n\" \"I[00:00:00.000] log line padding padding padding\" >&2; "
             "i=$((i+1)); done; "
             "printf \"Content-Length: 30\\r\\n\\r\\n{\\\"m\\\":\\\"HELLO-FROM-SERVER\\\"}\"'",
             LOG_BYTES / 48);

    /* Without draining: the server is stuck on its own log and never speaks.
       This is the negative control, and it is the bug. */
    {
        lsp_process_t* proc = lsp_start_server(cmd, ".");
        if (!proc) {
            printf("FAIL could not start the stub server\n");
            return 1;
        }
        if (wait_for_reply(proc, 0)) {
            printf("FAIL the stub server answered without its stderr being read\n");
            printf("     (the test cannot prove anything: raise LOG_BYTES)\n");
            failures++;
        } else {
            printf("ok   a server whose stderr is never read goes silent\n");
        }
        lsp_stop_server(proc);
    }

    /* With draining: the log goes nowhere, the server gets through it, and
       the reply arrives. */
    {
        lsp_process_t* proc = lsp_start_server(cmd, ".");
        if (!proc) {
            printf("FAIL could not start the stub server (second run)\n");
            return 1;
        }
        if (wait_for_reply(proc, 1)) {
            printf("ok   draining its stderr lets it speak again\n");
        } else {
            printf("FAIL the server stayed silent even with its stderr drained\n");
            failures++;
        }
        lsp_stop_server(proc);
    }

    /* Draining a server with nothing to say must not report bytes, and must
       not block: it is called every frame. */
    {
        lsp_process_t* proc = lsp_start_server("sh -c 'sleep 30'", ".");
        if (proc) {
            clock_t t0 = clock();
            int n = lsp_drain_stderr(proc);
            if (n != 0) {
                printf("FAIL a quiet server drained %d bytes\n", n);
                failures++;
            } else if (elapsed_since(t0) > 1.0) {
                printf("FAIL draining a quiet server took %.2fs\n",
                       elapsed_since(t0));
                failures++;
            } else {
                printf("ok   draining a quiet server is free\n");
            }
            lsp_stop_server(proc);
        }
    }

    if (failures) {
        printf("test_stderr_drain: %d FAILED\n", failures);
        return 1;
    }
    printf("test_stderr_drain: all passed\n");
    return 0;
}
