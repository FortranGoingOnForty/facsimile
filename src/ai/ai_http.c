/* Non-blocking HTTP/1.1 client for the local-model completion backend.
 *
 * One implementation serves both a loopback ollama and a host reached over
 * Tailscale, so there is no TLS here and no new link dependency.
 *
 * Everything is driven from fac's single-threaded main loop, which blocks
 * ~50ms on stdin and must never block anywhere else. ai_http_pump() therefore
 * performs at most one syscall's worth of progress and returns; a request
 * advances one state per pump.
 *
 * A fresh connection is opened per request with "Connection: close". Connect
 * costs ~50us on loopback and ~5ms over Tailscale against 250-1000ms of
 * inference, so pooling would buy nothing while adding idle-timeout races,
 * half-closed reuse and pipelining as failure modes. It also makes EOF a
 * valid body terminator, and makes cancellation exact: closing the socket
 * cancels generation server-side and frees the remote GPU.
 */

#ifdef _WIN32
/* Sockets on Windows need winsock2 and -lws2_32, and the Makefile has no
   LDFLAGS at all. Rather than discover that at release time, the feature is
   compiled out here and reports itself unavailable. */
#include <string.h>

int ai_http_available_f(void) { return 0; }
void ai_http_init_f(void) {}
int ai_http_resolve_f(const char *h, int hl, int p, void *out) {
    (void)h; (void)hl; (void)p; (void)out; return 0;
}
void ai_http_begin_f(void **hp, const void *a, const char *b, int bl,
                     int ct, int tt) {
    (void)a; (void)b; (void)bl; (void)ct; (void)tt; if (hp) *hp = 0;
}
void ai_http_pump_f(void **hp, int *st, int *code, int *n) {
    (void)hp; if (st) *st = 6; if (code) *code = 0; if (n) *n = 0;
}
int ai_http_take_f(void **hp, char *o, int c) { (void)hp; (void)o; (void)c; return 0; }
void ai_http_abort_f(void **hp) { (void)hp; }

#else

#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

/* Keep in step with ai_http_module.f90 */
#define AI_HTTP_IDLE       0
#define AI_HTTP_CONNECTING 1
#define AI_HTTP_SENDING    2
#define AI_HTTP_RECV_HEAD  3
#define AI_HTTP_RECV_BODY  4
#define AI_HTTP_DONE       5
#define AI_HTTP_ERROR      6

#define AI_HTTP_MAX_BODY  (256 * 1024)
#define AI_HTTP_HEAD_CAP  (8 * 1024)

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#endif

typedef struct {
    int fd;
    int state;

    char *req;              /* owned request bytes, headers + body */
    size_t req_len;
    size_t req_off;

    char *buf;              /* owned response bytes, headers then body */
    size_t buf_cap;
    size_t buf_len;

    size_t head_len;        /* bytes of buf consumed by headers, 0 until parsed */
    long   content_length;  /* -1 = absent, read until EOF */
    int    status;

    struct sockaddr_in addr;

    long connect_deadline_ms;
    long total_deadline_ms;
} ai_http_t;

static long now_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long)ts.tv_sec * 1000L + ts.tv_nsec / 1000000L;
}

/* SIGPIPE's default action terminates the process. fac installs no signal
   handlers of its own, so without this a peer that closes mid-send -- ollama
   restarting, a laptop resuming, a model being evicted -- would kill the
   editor and take every unsaved buffer with it. MSG_NOSIGNAL and
   SO_NOSIGPIPE below cover the same hazard belt-and-braces. */
void ai_http_init_f(void) {
    static int done = 0;
    if (done) return;
    signal(SIGPIPE, SIG_IGN);
    done = 1;
}

int ai_http_available_f(void) { return 1; }

/* Blocking. Called only when a backend is enabled, never on the completion
   path -- getaddrinfo has no async form and MagicDNS can take seconds. */
int ai_http_resolve_f(const char *host, int host_len, int port, void *out_addr) {
    char name[256];
    struct addrinfo hints, *res = NULL;
    struct sockaddr_in *out = (struct sockaddr_in *)out_addr;
    int n = host_len;

    if (!host || !out || n <= 0) return 0;
    if (n > (int)sizeof(name) - 1) n = (int)sizeof(name) - 1;
    memcpy(name, host, (size_t)n);
    name[n] = '\0';

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;

    if (getaddrinfo(name, NULL, &hints, &res) != 0 || !res) return 0;

    memcpy(out, res->ai_addr, sizeof(struct sockaddr_in));
    out->sin_port = htons((unsigned short)port);
    freeaddrinfo(res);
    return 1;
}

static void ai_http_free(ai_http_t *h) {
    if (!h) return;
    if (h->fd >= 0) { close(h->fd); h->fd = -1; }
    free(h->req); h->req = NULL;
    free(h->buf); h->buf = NULL;
}

static void fail(ai_http_t *h) {
    if (h->fd >= 0) { close(h->fd); h->fd = -1; }
    h->state = AI_HTTP_ERROR;
}

void ai_http_begin_f(void **handle, const void *addr, const char *body,
                     int body_len, int connect_ms, int total_ms) {
    ai_http_t *h;
    int flags;
    int one = 1;

    if (!handle) return;
    *handle = NULL;
    if (!addr || body_len < 0) return;

    h = (ai_http_t *)calloc(1, sizeof(ai_http_t));
    if (!h) return;

    h->fd = -1;
    h->content_length = -1;
    h->connect_deadline_ms = now_ms() + (connect_ms > 0 ? connect_ms : 1000);
    h->total_deadline_ms = now_ms() + (total_ms > 0 ? total_ms : 5000);
    memcpy(&h->addr, addr, sizeof(struct sockaddr_in));

    h->req = (char *)malloc((size_t)body_len);
    if (!h->req) { free(h); return; }
    if (body_len > 0) memcpy(h->req, body, (size_t)body_len);
    h->req_len = (size_t)body_len;

    h->buf_cap = 16 * 1024;
    h->buf = (char *)malloc(h->buf_cap);
    if (!h->buf) { free(h->req); free(h); return; }

    h->fd = socket(AF_INET, SOCK_STREAM, 0);
    if (h->fd < 0) { h->state = AI_HTTP_ERROR; *handle = h; return; }

    flags = fcntl(h->fd, F_GETFL, 0);
    if (flags < 0 || fcntl(h->fd, F_SETFL, flags | O_NONBLOCK) < 0) {
        fail(h); *handle = h; return;
    }
    setsockopt(h->fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));
#ifdef SO_NOSIGPIPE
    setsockopt(h->fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof(one));
#endif

    if (connect(h->fd, (struct sockaddr *)&h->addr, sizeof(h->addr)) == 0) {
        h->state = AI_HTTP_SENDING;
    } else if (errno == EINPROGRESS) {
        h->state = AI_HTTP_CONNECTING;
    } else {
        fail(h);
    }
    *handle = h;
}

static int writable(int fd) {
    struct pollfd p;
    p.fd = fd; p.events = POLLOUT; p.revents = 0;
    return poll(&p, 1, 0) > 0 && (p.revents & POLLOUT);
}

static int readable(int fd) {
    struct pollfd p;
    p.fd = fd; p.events = POLLIN; p.revents = 0;
    return poll(&p, 1, 0) > 0 && (p.revents & (POLLIN | POLLHUP));
}

static int ensure_cap(ai_http_t *h, size_t extra) {
    if (h->buf_len + extra <= h->buf_cap) return 1;
    while (h->buf_cap < h->buf_len + extra) {
        if (h->buf_cap >= (size_t)AI_HTTP_MAX_BODY) return 0;
        h->buf_cap *= 2;
    }
    {
        char *nb = (char *)realloc(h->buf, h->buf_cap);
        if (!nb) return 0;
        h->buf = nb;
    }
    return 1;
}

/* Parse the status line and Content-Length once CRLFCRLF has arrived. */
static void parse_head(ai_http_t *h, size_t head_end) {
    size_t i;
    h->head_len = head_end;
    h->status = 0;
    if (h->buf_len >= 12 && strncmp(h->buf, "HTTP/1.", 7) == 0)
        h->status = atoi(h->buf + 9);

    for (i = 0; i + 15 < head_end; i++) {
        if ((h->buf[i] == 'C' || h->buf[i] == 'c') &&
            strncasecmp(h->buf + i, "Content-Length:", 15) == 0) {
            h->content_length = atol(h->buf + i + 15);
            break;
        }
    }
}

void ai_http_pump_f(void **handle, int *state, int *status, int *nbytes) {
    ai_http_t *h;
    long now;

    if (state) *state = AI_HTTP_ERROR;
    if (status) *status = 0;
    if (nbytes) *nbytes = 0;
    if (!handle || !*handle) return;
    h = (ai_http_t *)*handle;

    now = now_ms();
    if (h->state != AI_HTTP_DONE && h->state != AI_HTTP_ERROR) {
        if (now > h->total_deadline_ms) fail(h);
        else if (h->state == AI_HTTP_CONNECTING && now > h->connect_deadline_ms) fail(h);
    }

    switch (h->state) {
    case AI_HTTP_CONNECTING: {
        int err = 0;
        socklen_t elen = sizeof(err);
        if (!writable(h->fd)) break;
        if (getsockopt(h->fd, SOL_SOCKET, SO_ERROR, &err, &elen) < 0 || err != 0) {
            fail(h);
            break;
        }
        h->state = AI_HTTP_SENDING;
        /* fall through so a loopback connect completes in one pump */
    }
    /* FALLTHROUGH */
    case AI_HTTP_SENDING: {
        ssize_t n;
        if (h->req_off >= h->req_len) { h->state = AI_HTTP_RECV_HEAD; break; }
        if (!writable(h->fd)) break;
        n = send(h->fd, h->req + h->req_off, h->req_len - h->req_off, MSG_NOSIGNAL);
        if (n > 0) {
            h->req_off += (size_t)n;
            if (h->req_off >= h->req_len) h->state = AI_HTTP_RECV_HEAD;
        } else if (n < 0 && errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) {
            fail(h);
        }
        break;
    }
    case AI_HTTP_RECV_HEAD:
    case AI_HTTP_RECV_BODY: {
        ssize_t n;
        if (!readable(h->fd)) break;
        if (!ensure_cap(h, 8192 + 1)) { fail(h); break; }
        n = recv(h->fd, h->buf + h->buf_len, 8192, 0);
        if (n > 0) {
            h->buf_len += (size_t)n;
            if (h->buf_len > (size_t)AI_HTTP_MAX_BODY) { fail(h); break; }
            if (h->state == AI_HTTP_RECV_HEAD) {
                char *p;
                if (h->buf_len > AI_HTTP_HEAD_CAP && h->head_len == 0) { fail(h); break; }
                h->buf[h->buf_len] = '\0';  /* safe: ensure_cap left slack */
                p = strstr(h->buf, "\r\n\r\n");
                if (p) {
                    parse_head(h, (size_t)(p - h->buf) + 4);
                    h->state = AI_HTTP_RECV_BODY;
                }
            }
            if (h->state == AI_HTTP_RECV_BODY && h->content_length >= 0) {
                if (h->buf_len - h->head_len >= (size_t)h->content_length) {
                    close(h->fd); h->fd = -1;
                    h->state = AI_HTTP_DONE;
                }
            }
        } else if (n == 0) {
            /* Peer closed: a valid terminator when Content-Length was absent */
            close(h->fd); h->fd = -1;
            h->state = (h->head_len > 0) ? AI_HTTP_DONE : AI_HTTP_ERROR;
        } else if (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) {
            fail(h);
        }
        break;
    }
    default:
        break;
    }

    if (state) *state = h->state;
    if (status) *status = h->status;
    if (nbytes) *nbytes = (int)(h->buf_len > h->head_len ? h->buf_len - h->head_len : 0);
}

/* Copy the body out. Returns bytes written, or 0. */
int ai_http_take_f(void **handle, char *out, int out_cap) {
    ai_http_t *h;
    size_t n;

    if (!handle || !*handle || !out || out_cap <= 0) return 0;
    h = (ai_http_t *)*handle;
    if (h->state != AI_HTTP_DONE || h->buf_len <= h->head_len) return 0;

    n = h->buf_len - h->head_len;
    if (n > (size_t)out_cap) n = (size_t)out_cap;
    memcpy(out, h->buf + h->head_len, n);
    return (int)n;
}

void ai_http_abort_f(void **handle) {
    if (!handle || !*handle) return;
    ai_http_free((ai_http_t *)*handle);
    free(*handle);
    *handle = NULL;
}

#endif /* _WIN32 */
