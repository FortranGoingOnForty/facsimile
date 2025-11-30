# Test file for Ruff code actions
import os
import sys  # This is unused - Ruff should offer to remove it
import json  # Also unused

x = 1
y = 2  # y is assigned but never used
print(x)

def foo():
    z = 10  # assigned but never used
    return "hello"
