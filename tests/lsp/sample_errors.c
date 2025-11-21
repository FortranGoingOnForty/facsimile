#include <stdio.h>

int main() {
    printf("Hello, world!\n")
    // Missing semicolon above - should trigger diagnostic

    int x = 5;
    int y = 10
    // Another missing semicolon

    return 0;
}