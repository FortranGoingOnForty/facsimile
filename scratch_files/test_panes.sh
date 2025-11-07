#!/bin/bash

echo "Testing Facsimile Pane Functionality"
echo "====================================="
echo
echo "This test will open the editor with the pane_test.txt file"
echo "You can test the following:"
echo
echo "1. Press Alt-V to split the view vertically"
echo "2. Press Alt-S to split the view horizontally"
echo "3. Press Alt-Q to close the active pane only"
echo "4. Press Ctrl-W to close pane (then tab when last pane)"
echo "5. Visual indicators:"
echo "   - Active pane: Normal background, cursor visible"
echo "   - Inactive panes: Dark gray background (color 234)"
echo "   - Separator: Solid vertical line between panes"
echo "   - Current line in active pane: Highlighted (color 237)"
echo ""
echo "6. Behavior:"
echo "   - Each pane has independent scrolling"
echo "   - Cursor stays within active pane boundaries"
echo "   - Empty lines show '~' indicator"
echo "   - Full pane height is utilized for content"
echo ""
echo "Note: Alt keys avoid conflicts with terminal and browser shortcuts"
echo
echo "Press Enter to start the test..."
read

./fac pane_test.txt