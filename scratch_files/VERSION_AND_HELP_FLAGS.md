# Version and Help Flags Implementation

## Overview

Added command-line argument support for `--version`, `-v`, `--help`, and `-h` flags with version pulled from a single source of truth.

## Implementation

### Single Source of Truth

**VERSION file**: Contains just the version number (e.g., `0.7.5`)
- Located at project root
- Easy to update in one place
- Read by Makefile during build

### Auto-Generated Version Module

**Makefile changes**:
```makefile
VERSION := $(shell cat VERSION 2>/dev/null || echo "unknown")

# Generate version module before building
src/version_module.f90: VERSION
	@echo "Generating version module..."
	@echo "module version_module" > $@
	@echo "    implicit none" >> $@
	@echo "    character(len=*), parameter :: VERSION = '$(VERSION)'" >> $@
	@echo "end module version_module" >> $@
```

This generates `src/version_module.f90` which is compiled into the binary.

### Main Program Changes

**app/main.f90**:
- Added `use version_module` to import VERSION constant
- Added argument parsing for `--version`, `-v`, `--help`, `-h`
- Added `print_help()` subroutine with comprehensive key bindings reference

## Usage

```bash
# Show version
./fac --version
./fac -v

# Show help
./fac --help
./fac -h

# Open file (works as before)
./fac myfile.txt

# Open empty editor (works as before)
./fac
```

## Output Examples

### Version
```
$ ./fac -v
fac version 0.7.5
```

### Help
```
$ ./fac -h
fac - Fortran text editor
Version: 0.7.5

Usage:
  fac [filename]       Open a file for editing
  fac                  Start with empty buffer
  fac --version, -v    Show version information
  fac --help, -h       Show this help message

Key Bindings:
  Ctrl-Q               Quit
  Ctrl-S               Save
  Ctrl-F               Find/Replace (unified prompt)
  ...
```

## Updating Version

To update the version for a new release:

1. Update the VERSION file:
   ```bash
   echo "0.8.0" > VERSION
   ```

2. Rebuild:
   ```bash
   make clean && make
   ```

3. The new version is automatically included in the binary.

4. Optionally, create a git tag:
   ```bash
   git tag v0.8.0
   git push --tags
   ```

## Benefits

✅ **Single source of truth**: Version defined once in VERSION file
✅ **No manual sync needed**: Makefile auto-generates the module
✅ **Standard flags**: Follows Unix conventions (`-h`, `-v`)
✅ **Comprehensive help**: Shows all key bindings in one place
✅ **Clean build**: Generated file cleaned by `make clean`

## Files Modified

- `VERSION` (new): Version number
- `Makefile`: Added version module generation
- `app/main.f90`: Added argument parsing and help
- `src/version_module.f90` (auto-generated): Not tracked in git

## Files to Track in Git

- ✅ `VERSION` - The source of truth
- ✅ `Makefile` - Build logic
- ✅ `app/main.f90` - Argument handling
- ❌ `src/version_module.f90` - Auto-generated, add to `.gitignore`
