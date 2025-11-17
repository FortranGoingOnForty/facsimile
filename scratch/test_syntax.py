#!/usr/bin/env python
"""Test file for syntax highlighting in fac"""

def fibonacci(n: int) -> int:
    """Calculate the nth Fibonacci number"""
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

class Calculator:
    def __init__(self):
        self.result = 0

    def add(self, x: float, y: float) -> float:
        # Add two numbers
        self.result = x + y
        return self.result

# Test the functions
if __name__ == "__main__":
    print("Fibonacci of 10:", fibonacci(10))
    calc = Calculator()
    print("5 + 3 =", calc.add(5, 3))