/**
 * Test file for LSP diagnostics display
 * This file contains intentional errors to test diagnostic markers
 */

#include <stdio.h>

int main() {
    // Error: Missing semicolon
    int x = 5

    // Error: Undefined variable 'y'
    printf("Value: %d\n", y);

    // Warning: Type mismatch
    int z = "hello";

    // Warning: Unused variable
    int unused_var = 10;

    // Error: Wrong number of arguments
    printf();

    // Warning: Missing return in function returning int
    if (x > 0) {
        return 0;
    }
    // Missing return for else case
}

// Function with missing parameter type
void test_func(param) {
    printf("Test\n");
}