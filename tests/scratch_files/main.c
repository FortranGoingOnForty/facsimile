/*
 * Main program using utility functions
 *
 * Test cross-file navigation:
 * - F12 on 'factorial' should jump to utils.c
 * - Shift+F12 on 'fibonacci' should show all usages
 * - Ctrl+Shift+T and search "sum" to find sum_array
 */

#include <stdio.h>

// Forward declarations from utils.c
int factorial(int n);
int fibonacci(int n);
int sum_array(int *arr, int size);
int find_max(int *arr, int size);

// Structure for testing Document Symbols (Ctrl+Shift+O)
typedef struct {
    int id;
    char name[50];
    double value;
} Record;

// Test: F12 on this function name to jump to definition
void process_records(Record *records, int count) {
    printf("Processing %d records\n", count);
    for (int i = 0; i < count; i++) {
        printf("Record %d: %s = %.2f\n",
               records[i].id,
               records[i].name,
               records[i].value);
    }
}

int main() {
    printf("Testing LSP features in C\n\n");

    // Test factorial
    // F12 on 'factorial' should jump to utils.c
    int fact = factorial(5);
    printf("factorial(5) = %d\n", fact);

    // Test fibonacci
    // Shift+F12 on 'fibonacci' should show all usages
    int fib = fibonacci(7);
    printf("fibonacci(7) = %d\n", fib);

    // Test array functions
    int numbers[] = {5, 2, 8, 1, 9, 3};
    int size = sizeof(numbers) / sizeof(numbers[0]);

    // F12 on 'sum_array' should jump to definition
    int total = sum_array(numbers, size);
    printf("sum_array() = %d\n", total);

    // F12 on 'find_max' should jump to definition
    int max = find_max(numbers, size);
    printf("find_max() = %d\n", max);

    // Test with records
    Record records[2] = {
        {1, "Alpha", 3.14},
        {2, "Beta", 2.71}
    };
    process_records(records, 2);

    // Intentional errors for diagnostics
    // ERROR: undefined function
    int result = undefined_function(10);

    // ERROR: type mismatch
    char *str = fact;

    // ERROR: undeclared variable
    printf("Value: %d\n", undeclared_var);

    return 0;
}
