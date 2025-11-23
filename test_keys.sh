#!/bin/bash
# Test what escape sequences the terminal sends for function keys

echo "Press keys to see their escape sequences (Ctrl+C to quit):"
echo "Try: F12, Shift-F12, F8, F4, F6, F2"
echo ""

# Read raw input and show escape sequences
while true; do
    read -rsn1 char
    if [[ $char == $'\e' ]]; then
        # Start of escape sequence
        seq="$char"
        # Read the rest quickly
        while read -rsn1 -t 0.001 char; do
            seq="$seq$char"
        done
        # Show it in a readable format
        echo -n "Escape sequence: "
        echo -n "$seq" | od -An -tx1 | tr -d ' \n'
        echo " (raw: $(echo -n "$seq" | cat -v))"
    else
        echo "Character: '$char' (ASCII: $(printf '%d' "'$char"))"
    fi
done
