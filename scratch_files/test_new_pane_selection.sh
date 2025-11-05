#!/bin/bash

echo "Testing Shift Selection in NEWLY CREATED Panes"
echo "==============================================="
echo
echo "ISSUE: Shift-arrow selection works in original pane but not in newly created panes"
echo
echo "FIX APPLIED:"
echo "1. Ensured cursor state is synced before splitting"
echo "2. Deep copy all cursor fields when creating new pane"
echo "3. Initialize cursor properly if not allocated"
echo
echo "TEST STEPS:"
echo "-----------"
echo "1. Open the test file"
echo "2. Press Alt-V to create a vertical split"
echo "   - You are now in the NEW RIGHT PANE"
echo "3. Test shift selection in NEW pane:"
echo "   - Shift-Right: Should select text character by character"
echo "   - Shift-Left: Should select backward"
echo "   - Alt-Shift-Right: Should select current word"
echo "4. Navigate back to left pane (Alt-H or Ctrl-Shift-Left)"
echo "5. Verify selection still works in original pane"
echo "6. Create a horizontal split (Alt-S)"
echo "7. Test selection in the new bottom pane"
echo
echo "EXPECTED RESULTS:"
echo "- Shift selection should work in ALL panes"
echo "- Both original and newly created panes"
echo "- Each pane maintains independent selection state"
echo
echo "Press Enter to start testing..."
read

./build/gfortran_*/app/fac test_shift_selection.txt