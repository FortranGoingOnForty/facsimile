#!/bin/bash

echo "Testing Shift Selection in Pane Mode"
echo "====================================="
echo
echo "ISSUE REPORTED: Shift selection doesn't work in pane mode"
echo
echo "FIX APPLIED: Added sync_editor_to_pane() calls after all"
echo "selection operations to sync cursor/selection state with panes"
echo
echo "Tests to perform:"
echo "-----------------"
echo
echo "1. BASIC SELECTION IN PANES:"
echo "   - Press Alt-V to split vertically"
echo "   - Use Shift+Arrow keys to select text"
echo "   - Selection should work normally"
echo
echo "2. WORD SELECTION:"
echo "   - Position cursor at start of a word"
echo "   - Press Alt-Shift-Right"
echo "   - Should select the CURRENT word"
echo
echo "3. LINE SELECTION:"
echo "   - Shift-Home: Select to start of line"
echo "   - Shift-End: Select to end of line"
echo
echo "4. NAVIGATION WITH SELECTION:"
echo "   - Make a selection in one pane"
echo "   - Navigate to another pane (Alt-H/J/K/L)"
echo "   - Original pane should maintain selection"
echo
echo "5. CLEAR SELECTION:"
echo "   - Press ESC to clear selection"
echo "   - Selection should be cleared"
echo
echo "Press Enter to start testing..."
read

./build/gfortran_*/app/fac test_shift_selection.txt