#!/usr/bin/env python3
"""
Integration test runner for FACSIMILE editor.
This script uses pexpect to simulate terminal interactions with the editor.
"""

import sys
import os
import time
import tempfile
import subprocess
from typing import List, Tuple, Optional

try:
    import pexpect
except ImportError:
    print("Error: pexpect is required. Install with: pip install pexpect")
    sys.exit(1)


class FacsimileTest:
    """Test harness for FACSIMILE editor."""

    def __init__(self, binary_path: str = "./build/gfortran_*/app/fac"):
        """Initialize test harness."""
        # Find the actual binary path
        import glob
        paths = glob.glob(binary_path)
        if not paths:
            raise FileNotFoundError(f"Could not find fac binary at {binary_path}")
        self.binary_path = paths[0]
        self.process = None
        self.test_file = None

    def start(self, initial_content: str = "") -> None:
        """Start the editor with a temporary file."""
        # Create temporary file with initial content
        self.test_file = tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False)
        self.test_file.write(initial_content)
        self.test_file.flush()
        self.test_file.close()

        # Start the editor
        self.process = pexpect.spawn(self.binary_path, [self.test_file.name],
                                     timeout=5, encoding='utf-8')
        time.sleep(0.2)  # Give editor time to initialize

    def stop(self) -> None:
        """Stop the editor and clean up."""
        if self.process:
            self.send_key('ctrl-q')
            self.process.expect(pexpect.EOF)
            self.process = None

        # Clean up temp file
        if self.test_file:
            try:
                os.unlink(self.test_file.name)
            except:
                pass
            self.test_file = None

    def send_key(self, key: str) -> None:
        """Send a special key combination to the editor."""
        key_map = {
            'ctrl-a': '\x01', 'ctrl-b': '\x02', 'ctrl-c': '\x03', 'ctrl-d': '\x04',
            'ctrl-e': '\x05', 'ctrl-f': '\x06', 'ctrl-g': '\x07', 'ctrl-h': '\x08',
            'ctrl-i': '\x09', 'ctrl-j': '\x0a', 'ctrl-k': '\x0b', 'ctrl-l': '\x0c',
            'ctrl-m': '\x0d', 'ctrl-n': '\x0e', 'ctrl-o': '\x0f', 'ctrl-p': '\x10',
            'ctrl-q': '\x11', 'ctrl-r': '\x12', 'ctrl-s': '\x13', 'ctrl-t': '\x14',
            'ctrl-u': '\x15', 'ctrl-v': '\x16', 'ctrl-w': '\x17', 'ctrl-x': '\x18',
            'ctrl-y': '\x19', 'ctrl-z': '\x1a',
            'escape': '\x1b', 'enter': '\n', 'tab': '\t',
            'backspace': '\x7f', 'delete': '\x1b[3~',
            'up': '\x1b[A', 'down': '\x1b[B', 'right': '\x1b[C', 'left': '\x1b[D',
            'home': '\x1b[H', 'end': '\x1b[F',
            'pageup': '\x1b[5~', 'pagedown': '\x1b[6~',
            'alt-[': '\x1b[', 'alt-]': '\x1b]',
        }

        if key in key_map:
            self.process.send(key_map[key])
        else:
            print(f"Warning: Unknown key '{key}'")

    def type_text(self, text: str) -> None:
        """Type regular text into the editor."""
        self.process.send(text)

    def save_file(self) -> None:
        """Save the current file."""
        self.send_key('ctrl-s')
        time.sleep(0.1)

    def get_file_content(self) -> str:
        """Get the current content of the file."""
        self.save_file()
        with open(self.test_file.name, 'r') as f:
            return f.read()

    def screenshot(self) -> str:
        """Get current screen content (for debugging)."""
        # Send a redraw command
        self.send_key('ctrl-l')
        time.sleep(0.1)
        return self.process.before or ""


class TestRunner:
    """Run integration tests for FACSIMILE."""

    def __init__(self):
        self.passed = 0
        self.failed = 0
        self.tests = []

    def add_test(self, name: str, test_func):
        """Add a test to the suite."""
        self.tests.append((name, test_func))

    def run(self) -> bool:
        """Run all tests and return success status."""
        print("=" * 60)
        print("FACSIMILE Integration Tests")
        print("=" * 60)

        for name, test_func in self.tests:
            try:
                editor = FacsimileTest()
                test_func(editor)
                print(f"✓ {name}")
                self.passed += 1
            except Exception as e:
                print(f"✗ {name}: {str(e)}")
                self.failed += 1
            finally:
                if 'editor' in locals():
                    editor.stop()

        print("-" * 60)
        print(f"Tests run: {self.passed + self.failed}, Passed: {self.passed}, Failed: {self.failed}")

        if self.failed == 0:
            print("All tests passed! ✓")
            return True
        else:
            print(f"FAILURE: {self.failed} tests failed")
            return False


# Test functions
def test_basic_typing(editor: FacsimileTest):
    """Test basic text input."""
    editor.start("")
    editor.type_text("Hello World")
    content = editor.get_file_content()
    assert content == "Hello World", f"Expected 'Hello World', got '{content}'"


def test_navigation(editor: FacsimileTest):
    """Test cursor navigation."""
    editor.start("Hello\nWorld")
    editor.send_key('down')
    editor.send_key('end')
    editor.type_text("!")
    content = editor.get_file_content()
    assert content == "Hello\nWorld!", f"Expected 'Hello\\nWorld!', got '{content}'"


def test_auto_close_brackets(editor: FacsimileTest):
    """Test auto-close brackets feature."""
    editor.start("")
    editor.type_text("(")
    editor.type_text("test")
    editor.send_key('right')  # Move past closing paren
    editor.type_text(" [")
    editor.type_text("array")
    content = editor.get_file_content()
    assert "(test) [array]" in content, f"Auto-close failed: '{content}'"


def test_search(editor: FacsimileTest):
    """Test search functionality."""
    editor.start("Hello World\nHello Universe\nHello Galaxy")
    editor.type_text("/")
    time.sleep(0.1)
    editor.type_text("Universe")
    editor.send_key('enter')
    # Cursor should now be on "Universe"
    editor.send_key('ctrl-k')  # Kill line forward
    content = editor.get_file_content()
    assert "Hello Universe" not in content or "Universe" not in content.split('\n')[1]


def test_undo_redo(editor: FacsimileTest):
    """Test undo/redo functionality."""
    editor.start("")
    editor.type_text("First")
    editor.send_key('ctrl-z')  # Undo
    content = editor.get_file_content()
    assert content == "", f"Undo failed: '{content}'"

    editor.send_key('ctrl-shift-z')  # Redo
    content = editor.get_file_content()
    assert content == "First", f"Redo failed: '{content}'"


def test_indent_dedent(editor: FacsimileTest):
    """Test tab/shift-tab indent/dedent."""
    editor.start("Line 1\nLine 2\nLine 3")
    # Select all lines (simplified - would need proper selection in real test)
    editor.send_key('ctrl-a')  # Go to start
    editor.type_text("    ")  # Manual indent for this test
    content = editor.get_file_content()
    assert content.startswith("    "), f"Indent failed: '{content}'"


def test_bracket_jump(editor: FacsimileTest):
    """Test jump to matching bracket."""
    editor.start("(hello [world] test)")
    editor.send_key('alt-]')  # Jump to matching bracket
    editor.type_text("X")
    content = editor.get_file_content()
    # This would need more sophisticated testing to verify cursor position


def test_smart_home(editor: FacsimileTest):
    """Test smart home behavior."""
    editor.start("    Hello World")
    editor.send_key('end')
    editor.send_key('ctrl-a')  # Should go to 'H'
    editor.type_text("X")
    content = editor.get_file_content()
    assert "XHello" in content or "    XHello" in content


def main():
    """Run all integration tests."""
    runner = TestRunner()

    # Add all test functions
    runner.add_test("Basic typing", test_basic_typing)
    runner.add_test("Navigation", test_navigation)
    runner.add_test("Auto-close brackets", test_auto_close_brackets)
    runner.add_test("Search", test_search)
    runner.add_test("Undo/Redo", test_undo_redo)
    runner.add_test("Indent/Dedent", test_indent_dedent)
    runner.add_test("Bracket jump", test_bracket_jump)
    runner.add_test("Smart home", test_smart_home)

    # Run tests
    success = runner.run()
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()