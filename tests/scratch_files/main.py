"""Test file for code actions."""
import os
import sys  # unused import


def calculate(x, y):
    """Calculate sum."""
    unused = 42  # unused variable
    return x + y


def main():
    """Main entry."""
    result = calculate(1, 2)
    print(result)


if __name__ == "__main__":
    main()
