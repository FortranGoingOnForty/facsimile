#!/usr/bin/env python3
"""
LSP Test File for Python (pylsp)
Tests code completion, hover, and diagnostics

Required: pip install python-lsp-server
"""

import os
import sys
from typing import List, Dict, Optional


class Employee:
    """Sample class to test hover and completion"""

    def __init__(self, name: str, age: int, salary: float):
        self.name = name
        self.age = age
        self.salary = salary

    def get_info(self) -> str:
        """Returns employee information as a string"""
        return f"{self.name}, {self.age} years, ${self.salary:.2f}"

    def give_raise(self, percentage: float) -> None:
        """Increases salary by given percentage"""
        self.salary *= (1 + percentage / 100)


def process_employees(employees: List[Employee]) -> Dict[str, float]:
    """Process employee data and return salary statistics"""
    if not employees:
        return {"average": 0, "total": 0}

    total = sum(emp.salary for emp in employees)

    # Test completion: Type 'emp.' and press Ctrl+Space
    # Should show: name, age, salary, get_info, give_raise
    emp = employees[0]
    emp.

    return {
        "average": total / len(employees),
        "total": total,
        "count": len(employees)
    }


def main():
    # Test hover: Position cursor on 'Employee' and press Ctrl+H
    # Should show class docstring and constructor signature
    employees = [
        Employee("Alice", 30, 75000),
        Employee("Bob", 35, 85000),
        Employee("Charlie", 28, 65000)
    ]

    # Test completion: Type 'os.' and press Ctrl+Space
    # Should show os module members
    current_dir = os.

    # Test completion: Type 'sys.' and press Ctrl+Space
    # Should show sys module members
    version = sys.

    stats = process_employees(employees)
    print(f"Statistics: {stats}")

    # Intentional error for diagnostics test
    # Should show: undefined variable 'undefined_var'
    print(undefined_var)


if __name__ == "__main__":
    main()