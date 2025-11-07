#!/bin/bash

echo "Testing Facsimile Pane Navigation"
echo "=================================="
echo
echo "This test will open the editor with pane_nav_test.txt"
echo "Test the following navigation keys:"
echo
echo "SPLITTING PANES:"
echo "  Alt-V: Split pane vertically"
echo "  Alt-S: Split pane horizontally"
echo
echo "NAVIGATING BETWEEN PANES:"
echo "  Ctrl-Shift-Left or Alt-H:  Move to pane on the left"
echo "  Ctrl-Shift-Right or Alt-L: Move to pane on the right"
echo "  Ctrl-Shift-Up or Alt-K:    Move to pane above"
echo "  Ctrl-Shift-Down or Alt-J:  Move to pane below"
echo
echo "CLOSING PANES:"
echo "  Alt-Q: Close current pane only"
echo "  Ctrl-W: Close current pane (closes tab when last pane)"
echo
echo "VISUAL INDICATORS:"
echo "  - Active pane has normal background with visible cursor"
echo "  - Inactive panes have dark gray background"
echo "  - Cursor position is constrained to active pane"
echo
echo "Press Enter to start the test..."
read

./build/gfortran_*/app/fac pane_nav_test.txt