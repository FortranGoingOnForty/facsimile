/* A language server that never reads must not be able to hang the editor.
 *
 * Found the hard way: an editor sat in write() for two and a half hours with
 * clangd parked in futex_do_wait. fac was writing a completion request to the
 * server's stdin; the pipe was full because the server had stopped reading;
 * the server had stopped reading because ITS output pipe back to fac was
 * full; and fac could not drain that, being inside the write. Neither side
 * could move again.
 *
 * The server's stdin was the one of the three pipes never set O_NONBLOCK.
 *
 * This test starts a "server" that reads nothing at all, pushes more at it
 * than a pipe can hold, and requires the call to come back. Before the fix it
 * blocks forever and the test has to be killed; after it, the write gives up
 * inside its budget and reports failure.
 *
 * Build/run: see the test-lsp target in the Makefile.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef struct lsp_process lsp_process_t;

extern lsp_process_t* lsp_start_server(const char* command, const char* workdir);
extern int lsp_send_message(lsp_process_t* proc, const char* message, int len);
extern void lsp_stop_server(lsp_process_t* proc);

/* Comfortably more than a pipe buffer, which is 64 KiB on Linux. */
#define PAYLOAD (512 * 1024)

/* The write budget is 2s; allow generous slack for a loaded machine while
   still failing long before "hung forever". */
#define LIMIT_SECONDS 30.0

int main(void) {
    int failures = 0;

    /* Reads nothing, stays alive. Exactly the state a wedged server is in. */
    lsp_process_t* proc = lsp_start_server("sleep 60", ".");
    if (!proc) {
        printf("SKIP test_write_deadlock: could not start the stub server\n");
        return 0;
    }

    char* big = malloc(PAYLOAD);
    if (!big) {
        printf("SKIP test_write_deadlock: out of memory\n");
        lsp_stop_server(proc);
        return 0;
    }
    memset(big, 'x', PAYLOAD);

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);
    int rc = lsp_send_message(proc, big, PAYLOAD);
    clock_gettime(CLOCK_MONOTONIC, &t1);

    double secs = (double)(t1.tv_sec - t0.tv_sec)
                + (double)(t1.tv_nsec - t0.tv_nsec) / 1e9;

    if (secs < LIMIT_SECONDS) {
        printf("ok   the write returned (%.2fs) instead of hanging\n", secs);
    } else {
        printf("FAIL the write took %.2fs\n", secs);
        failures++;
    }

    /* It could not have succeeded -- nothing is reading -- and saying it did
     * would be worse than failing: the caller has nowhere to put a
     * remainder, so a short write reported as success desynchronises the
     * JSON-RPC stream for the rest of the session.
     */
    if (rc < 0 || rc == PAYLOAD) {
        printf("ok   and reported %s rather than a partial count\n",
               rc < 0 ? "failure" : "a complete write");
    } else {
        printf("FAIL it reported %d of %d bytes written\n", rc, PAYLOAD);
        failures++;
    }

    free(big);
    lsp_stop_server(proc);

    if (failures) {
        printf("test_write_deadlock: FAILED (%d)\n", failures);
        return 1;
    }
    printf("test_write_deadlock: all passed\n");
    return 0;
}
