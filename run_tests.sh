#!/bin/bash
#
# Test runner for FACSIMILE editor
#

set -e

echo "========================================"
echo "FACSIMILE Test Suite"
echo "========================================"

# Build the project
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

# Check if Python and pexpect are available for integration tests
if command -v python3 &> /dev/null; then
    if python3 -c "import pexpect" 2>/dev/null; then
        echo ""
        echo "Running Integration Tests..."
        echo "----------------------------------------"
        python3 test/integration_test.py
        # These self-skip when pyte is not installed
        python3 test/integration_lazy_tree.py
        python3 test/integration_caret.py
    else
        echo ""
        echo "⚠ Skipping integration tests (pexpect not installed)"
        echo "  Install with: pip3 install pexpect"
    fi
else
    echo ""
    echo "⚠ Skipping integration tests (Python 3 not found)"
fi

echo ""
echo "========================================"
echo "Test run complete"
echo "========================================"