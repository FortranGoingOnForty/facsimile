"""
Main module that uses calculator.

Test cross-file navigation:
- Put cursor on 'add' below and press F12 - should jump to calculator.py
- Put cursor on 'calculate_total' and press Shift+F12 - find all usages
- Use Ctrl+Shift+T and search for "multiply" - should find it in calculator.py
"""

from calculator import add, subtract, multiply, calculate_total


def process_data(numbers):
    """Process a list of numbers using calculator functions."""
    # Test: F12 on 'add' should jump to calculator.py
    sum_result = add(numbers[0], numbers[1])

    # Test: F12 on 'multiply' should jump to calculator.py
    product = multiply(sum_result, 2)

    # Test: Shift+F12 on 'calculate_total' should show usages
    total = calculate_total(numbers)

    return {
        'sum': sum_result,
        'product': product,
        'total': total
    }


class DataProcessor:
    """A class for processing numerical data.

    Test Document Symbols (Ctrl+Shift+O) to see this class structure.
    """

    def __init__(self, data):
        self.data = data
        self.processed = False

    def calculate_sum(self):
        """Calculate sum using calculator module."""
        return calculate_total(self.data)

    def calculate_average(self):
        """Calculate average of the data."""
        total = self.calculate_sum()
        return divide(total, len(self.data))  # ERROR: 'divide' not imported

    def process(self):
        """Process the data."""
        self.processed = True
        return self.calculate_average()


# Test with intentional error
def main():
    numbers = [1, 2, 3, 4, 5]

    # This works
    result = process_data(numbers)
    print(f"Results: {result}")

    # This has errors
    processor = DataProcessor(numbers)
    avg = processor.process()  # Will error due to missing 'divide' import
    print(f"Average: {avg}")


if __name__ == "__main__":
    main()
