/*
 * Simple utility functions for testing LSP features
 *
 * Test:
 * - Diagnostics: Intentional errors marked below
 * - Go to Definition: F12 on function calls
 * - Find References: Shift+F12 on function names
 * - Code Actions: Ctrl+. on errors
 * - Formatting: Shift+Alt+F
 */

#include <stdio.h>
#include <stdlib.h>

// Calculate factorial
int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

// Calculate fibonacci number
int fibonacci(int n) {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

// Sum array elements
int sum_array(int *arr, int size) {
    int total = 0;
    for (int i = 0; i < size; i++) {
        total += arr[i];
    }
    return total;
}

// Find maximum in array
int find_max(int *arr, int size) {
    if (size <= 0) {
        return 0;
    }

    int max = arr[0];
    for (int i = 1; i < size; i++) {
        if (arr[i] > max) {
            max = arr[i];
        }
    }
    return max;
}

// Intentional error function for diagnostics
int broken_function() {
    int x = 10;
    int y = 20;

    // ERROR: undefined variable
    int result = x + y + undefined_var;

    // ERROR: incompatible pointer type
    char *str = result;

    printf("Result: %d\n", result);
    return result;
}

// Another error - missing return
int missing_return(int x) {
    if (x > 0) {
        return x * 2;
    }
    // ERROR: missing return statement for x <= 0
}
