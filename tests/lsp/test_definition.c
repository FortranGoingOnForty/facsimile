#include <stdio.h>

// Function declaration
int add(int a, int b);
int multiply(int x, int y);

// Function definitions
int add(int a, int b) {
    return a + b;
}

int multiply(int x, int y) {
    return x * y;
}

int main() {
    int result1 = add(5, 3);      // Press F12 on 'add' to jump to definition
    int result2 = multiply(4, 7); // Press F12 on 'multiply' to jump to definition

    printf("Addition: %d\n", result1);
    printf("Multiplication: %d\n", result2);

    return 0;
}