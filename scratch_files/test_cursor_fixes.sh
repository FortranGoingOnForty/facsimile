#!/bin/bash

echo "Testing Cursor Positioning Fixes"
echo "================================"
echo
echo "This test verifies that cursor positioning works correctly with:"
echo "1. Line numbers - cursor should not overlap with line numbers"
echo "2. Panes - cursor should be visible and positioned correctly"
echo "3. Selections - Alt-Shift-Right should select current word, not next"
echo
echo "Tests to perform:"
echo "-----------------"
echo
echo "TEST 1: Basic Cursor Position"
echo "  - Move cursor to start of lines (press Home or Ctrl-A)"
echo "  - Cursor should appear AFTER the line numbers, not on them"
echo
echo "TEST 2: Selection Accuracy"
echo "  - Position cursor at start of a word"
echo "  - Press Alt-Shift-Right"
echo "  - Should select the CURRENT word under cursor"
echo
echo "TEST 3: Pane Splits"
echo "  - Press Alt-V to split vertically"
echo "  - Cursor should be visible"
echo "  - Arrow keys should work"
echo "  - Cursor should respect line number boundaries"
echo
echo "TEST 4: Pane Navigation"
echo "  - Create multiple panes (Alt-V, Alt-S)"
echo "  - Navigate with Alt-H (left), Alt-L (right), Alt-K (up), Alt-J (down)"
echo "  - Or use Ctrl-Shift-Arrows if not conflicting"
echo
echo "Press Enter to start testing..."
read

./build/gfortran_*/app/fac cursor_test.txt