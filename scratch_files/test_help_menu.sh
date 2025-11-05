#!/bin/bash

echo "Testing Scrollable Help Menu"
echo "============================"
echo
echo "NEW FEATURES:"
echo "1. Scrollable pager for help menu"
echo "2. All keybindings now included (tab/shift-tab was missing!)"
echo "3. Navigate with arrows, j/k, PgUp/PgDn"
echo "4. Quit with q or ESC"
echo
echo "NAVIGATION IN HELP:"
echo "  ↑/k         - scroll up one line"
echo "  ↓/j         - scroll down one line"
echo "  PageUp      - scroll up one page"
echo "  PageDown    - scroll down one page"
echo "  Home        - jump to top"
echo "  End         - jump to bottom"
echo "  q/ESC       - quit help"
echo
echo "STATUS BAR:"
echo "  Look for 'ctrl-/:help' hint in the center of status bar"
echo
echo "Press Enter to start the editor and test ctrl-/ for help..."
read

./build/gfortran_*/app/fac test_scrollable_help.txt