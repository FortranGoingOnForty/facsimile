#include <stdio.h>
#include <stdlib.h>

// Function to calculate factorial
int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

int main(int argc, char *argv[]) {
    /* Multi-line comment
       Testing syntax highlighting */
    int num = 10;
    float pi = 3.14159;
    char message[] = "Hello, C!";

    printf("%s\n", message);
    printf("Factorial of %d is %d\n", num, factorial(num));

    for (int i = 0; i < 5; i++) {
        if (i % 2 == 0) {
            printf("%d is even\n", i);
        }
    }

    return 0;
}