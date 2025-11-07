# Implementation Bookmarks

Issues that require careful surgical implementation (not simple warning fixes)

---

## 🔖 BOOKMARK #1: Clipboard 1MB Stack Buffer

**File:** `src/clipboard/clipboard_module.f90`
**Line:** 40
**Priority:** HIGH (potential stack overflow)

### Current Issue
```fortran
character(len=1000000) :: buffer  ! 1MB buffer for clipboard content
```

This allocates 1MB on the stack, which can cause stack overflow crashes.

### gfortran Warning
```
Warning: Array 'buffer' at (1) is larger than limit set by '-fmax-stack-var-size=',
moved from stack to static storage. This makes the procedure unsafe when called
recursively, or concurrently from multiple threads. Consider increasing the
'-fmax-stack-var-size=' limit (or use '-frecursive', which implies unlimited
'-fmax-stack-var-size') - or change the code to use an ALLOCATABLE array.
```

### Recommended Solution
1. Change to `character(len=:), allocatable :: buffer`
2. Allocate dynamically: `allocate(character(len=needed_size) :: buffer)`
3. Deallocate after use
4. Consider reading clipboard in chunks if very large

### Affected Functions
- `clipboard_paste()` in `src/clipboard/clipboard_module.f90`

### Testing Required
- Test pasting small text
- Test pasting large text (>1MB)
- Test multiple paste operations
- Verify no memory leaks

### Status
⏳ Deferred for careful manual implementation after bulk warning cleanup

---

## Future Bookmarks

Add additional complex issues here as they're discovered...
