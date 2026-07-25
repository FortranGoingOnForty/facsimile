#!/usr/bin/env python3
"""Fixture servers for test_ai_http.

Three behaviours the transport must survive, each on its own port:

  ok    - a well-formed Content-Length response
  slow  - the same body dripped out a few bytes at a time, so the client has
          to carry partial reads across pumps
  close - headers promising more than is sent, then the socket is closed.
          This is the SIGPIPE case: the client is mid-conversation when the
          peer vanishes, and without SIGPIPE ignored the editor would die.

A fourth port is opened and immediately closed so the caller has an address
that refuses connections.

Prints the four ports on stdout (ok slow close dead) then serves until killed.
"""
import socket
import sys
import threading
import time

BODY = b"hello world"
DRIP = b"0123456789" * 4          # 40 bytes


def listener():
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    s.bind(("127.0.0.1", 0))
    s.listen(8)
    return s, s.getsockname()[1]


def read_request(conn):
    conn.settimeout(2.0)
    data = b""
    try:
        while b"\r\n\r\n" not in data and len(data) < 65536:
            chunk = conn.recv(4096)
            if not chunk:
                break
            data += chunk
    except OSError:
        pass
    return data


def serve_ok(sock):
    while True:
        try:
            conn, _ = sock.accept()
        except OSError:
            return
        threading.Thread(target=_ok_one, args=(conn,), daemon=True).start()


def _ok_one(conn):
    with conn:
        read_request(conn)
        resp = (b"HTTP/1.1 200 OK\r\n"
                b"Content-Type: text/plain\r\n"
                b"Content-Length: %d\r\n"
                b"Connection: close\r\n\r\n" % len(BODY)) + BODY
        try:
            conn.sendall(resp)
        except OSError:
            pass


def serve_slow(sock):
    while True:
        try:
            conn, _ = sock.accept()
        except OSError:
            return
        threading.Thread(target=_slow_one, args=(conn,), daemon=True).start()


def _slow_one(conn):
    with conn:
        read_request(conn)
        head = (b"HTTP/1.1 200 OK\r\n"
                b"Content-Length: %d\r\n"
                b"Connection: close\r\n\r\n" % len(DRIP))
        try:
            conn.sendall(head)
            for i in range(0, len(DRIP), 4):
                conn.sendall(DRIP[i:i + 4])
                time.sleep(0.01)
        except OSError:
            pass


def serve_close(sock):
    while True:
        try:
            conn, _ = sock.accept()
        except OSError:
            return
        threading.Thread(target=_close_one, args=(conn,), daemon=True).start()


def _close_one(conn):
    # Promise 500 bytes, send 10, then vanish. The client is mid-body when
    # the peer disappears.
    with conn:
        read_request(conn)
        try:
            conn.sendall(b"HTTP/1.1 200 OK\r\nContent-Length: 500\r\n"
                         b"Connection: close\r\n\r\n" + b"0123456789")
            conn.shutdown(socket.SHUT_RDWR)
        except OSError:
            pass


def main():
    ok_s, ok_p = listener()
    slow_s, slow_p = listener()
    close_s, close_p = listener()

    dead_s, dead_p = listener()
    dead_s.close()                 # nothing listening here now

    for target, sock in ((serve_ok, ok_s), (serve_slow, slow_s),
                         (serve_close, close_s)):
        threading.Thread(target=target, args=(sock,), daemon=True).start()

    print("%d %d %d %d" % (ok_p, slow_p, close_p, dead_p), flush=True)
    try:
        while True:
            time.sleep(3600)
    except KeyboardInterrupt:
        pass


if __name__ == "__main__":
    sys.exit(main())
