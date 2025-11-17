#!/bin/bash

# Bash script example with syntax highlighting

# Variables
NAME="Bash Script"
COUNT=10
readonly CONSTANT="Can't change this"

# Function definition
function greet() {
    local name=$1
    echo "Hello, $name!"
}

# Conditional
if [ -f "$0" ]; then
    echo "This script exists: $0"
fi

# Case statement
case "$1" in
    start)
        echo "Starting..."
        ;;
    stop)
        echo "Stopping..."
        ;;
    *)
        echo "Usage: $0 {start|stop}"
        exit 1
        ;;
esac

# Loops
for i in {1..5}; do
    if [ $((i % 2)) -eq 0 ]; then
        echo "$i is even"
    else
        echo "$i is odd"
    fi
done

# Array
declare -a fruits=("apple" "banana" "orange")
for fruit in "${fruits[@]}"; do
    echo "Fruit: $fruit"
done

# Command substitution
current_date=$(date +"%Y-%m-%d")
echo "Today is: $current_date"

# Pipe and redirection
ls -la | grep "^d" > /tmp/directories.txt

# Call function
greet "World"