"""
Simple calculator module for testing LSP features.

Test these features:
- Diagnostics: See the intentional errors below
- Go to Definition: F12 on function calls
- Find References: Shift+F12 on function names
- Rename: F2 on any function/variable
- Code Actions: Ctrl+. on errors
- Document Symbols: Ctrl+Shift+O to see outline
- Formatting: Shift+Alt+F to format
"""

def add(x, y):
    """Add two numbers together."""
    return x + y


def subtract(x, y):
    """Subtract y from x."""
    return x - y


def multiply(x, y):
    """Multiply two numbers."""
    return x * y


def divide(x, y):
    """Divide x by y."""
    if y == 0:
        raise ValueError("Cannot divide by zero")
    return x / y


def calculate_total(items):
    """Calculate total from a list of numbers."""
    total = 0
    for item in items:
        total = total + item
    return total


# Intentional error for diagnostics testing
def broken_function():
    """This function has an error - undefined variable."""
    result = add(5, 3)
    print(f"Result: {reslt}")  # ERROR: typo - should be 'result'
    return reslt


# Another intentional error - missing import
def use_math():
    """Uses math module without importing it."""
    return math.sqrt(16)  # ERROR: 'math' is not defined


# Test code
if __name__ == "__main__":
    # These work fine
    print(add(10, 5))
    print(subtract(10, 5))
    print(multiply(10, 5))
    print(divide(10, 5))

    # This will show diagnostic errors
    broken_function()
    use_math()
