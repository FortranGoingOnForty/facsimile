#!/bin/bash
#
# Test runner for FACSIMILE editor
#

set -e

echo "========================================"
echo "FACSIMILE Test Suite"
echo "========================================"

# Build the project.
#
# Note: `make` writes .mod files into the repo root and gfortran searches the
# working directory before any -I path, so fpm reads make's modules rather than
# its own. That is fine while both use the same compiler and breaks loudly
# ("Reading module ... Expected left parenthesis") when they do not. If fpm
# fails that way after a make build, `make clean` first.
echo "Building FACSIMILE..."
fpm build --profile debug

# Run Fortran unit tests
echo ""
echo "Running Fortran Unit Tests..."
echo "----------------------------------------"

if fpm test; then
    echo "✓ Fortran tests passed"
else
    echo "✗ Fortran tests failed"
    exit 1
fi

# HTTP transport needs live sockets, so it runs through its own harness
# (starts the fixture servers, passes their ports, tears them down).
echo ""
echo "Running HTTP transport test..."
echo "----------------------------------------"
sh test/run_ai_http_test.sh

# Integration tests. Discovered by glob rather than listed, the way fpm
# discovers test/*.f90 -- a suite that is written but never added to a list
# is a test that does not run, and that is exactly what happened to the
# 0.21.0 regression suites.
#
# Both imports are checked. The suites self-skip with exit 0 when either is
# missing, so testing only for pexpect meant a machine without pyte printed
# "Running Integration Tests" and then quietly ran almost none of them.
if ! command -v python3 &> /dev/null; then
    echo ""
    echo "⚠ Skipping integration tests (Python 3 not found)"
elif ! python3 -c "import pexpect, pyte" 2>/dev/null; then
    echo ""
    echo "⚠ Skipping integration tests (pexpect and/or pyte not installed)"
    echo "  Install with: pip3 install pexpect pyte"
    echo "  Without them every suite exits 0 without running."
else
    echo ""
    echo "Running Integration Tests..."
    echo "----------------------------------------"

    # Run every suite even if one fails, then report. Stopping at the first
    # failure hides how much else broke.
    integration_failures=""
    set +e
    for suite in test/integration_*.py; do
        echo ""
        echo "--- $(basename "$suite") ---"
        python3 "$suite"
        if [ $? -ne 0 ]; then
            integration_failures="$integration_failures $(basename "$suite")"
        fi
    done
    set -e

    if [ -n "$integration_failures" ]; then
        echo ""
        echo "✗ Integration suites failed:$integration_failures"
        exit 1
    fi
    echo ""
    echo "✓ Integration tests passed"
fi

echo ""
echo "========================================"
echo "Test run complete"
echo "========================================"