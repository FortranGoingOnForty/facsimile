#!/bin/bash
# Test what escape sequences WezTerm actually sends for F-keys
# Press F8, F12, Shift-F12, etc. and see what comes through

echo "=== Raw Key Sequence Tester ==="
echo "Press F8, F12, or Shift-F12, then Ctrl+C to exit"
echo "This will show the EXACT escape sequences received"
echo ""

# Use od to show the raw bytes of each key press
while true; do
    echo -n "Press a key: "
    # Read one key sequence with timeout
    IFS= read -rsn1 char

    # If it's ESC, read the rest of the sequence
    if [ "$char" = $'\x1b' ]; then
        # Read the next character(s) quickly
        rest=""
        while IFS= read -rsn1 -t 0.01 c; do
            rest="$rest$c"
        done
        full="$char$rest"

        # Show as hex and as escaped string
        echo -n "$full" | od -An -tx1 | tr -d ' \n'
        echo -n " = ESC"
        echo "$rest" | sed 's/./[&]/g'
    else
        echo "$char"
    fi
done
