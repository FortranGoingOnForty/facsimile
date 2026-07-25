#!/bin/sh
# Build and run the HTTP transport test against the Python fixture servers.
#
# The Fortran test needs live sockets, so the fixture is started here, its
# ports are handed to the test, and it is torn down afterwards regardless of
# the result.
#
#   sh test/run_ai_http_test.sh [path-to-repo]
set -e

ROOT=${1:-$(cd "$(dirname "$0")/.." && pwd)}
cd "$ROOT"

if [ ! -f src/ai/ai_http.o ]; then
    echo "SKIP: run 'make' first (need the compiled objects)"
    exit 0
fi

BIN=$(mktemp -d)/test_ai_http
trap 'rm -rf "$(dirname "$BIN")"; [ -n "$FIXPID" ] && kill "$FIXPID" 2>/dev/null' EXIT

# Start the fixture and read the four ports it prints
FIFO=$(mktemp -u)
mkfifo "$FIFO"
python3 test/ai_http_fixture.py > "$FIFO" &
FIXPID=$!
PORTS=$(head -1 "$FIFO")
rm -f "$FIFO"

if [ -z "$PORTS" ]; then
    echo "FAIL: fixture did not report its ports"
    exit 1
fi

gfortran -O1 -I. -o "$BIN" test/test_ai_http.f90 \
    src/ai/ai_http_module.o src/ai/ai_http.o

# shellcheck disable=SC2086
"$BIN" $PORTS
