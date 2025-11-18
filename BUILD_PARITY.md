# Build System Parity

This document explains how to achieve identical builds between fpm and Makefile.

## TL;DR

For identical optimized builds:

```bash
# Using fpm
fpm build --flag "-O2 -Wall -ffree-line-length-none"

# Using make
make

# Or use the unified build script
./build.sh release
```

## Build Systems

facsimile supports two build systems:

1. **fpm** (Fortran Package Manager) - Modern, dependency-aware
2. **Makefile** - Traditional, platform-specific optimizations

## Key Differences

| Aspect | fpm | Makefile |
|--------|-----|----------|
| Dependency Management | Automatic | Manual (order matters) |
| Parallel Build | Safe, automatic | Disabled (.NOTPARALLEL) |
| Compiler Selection | Uses PATH gfortran | Detects Homebrew gfortran on macOS |
| Output Location | `./build/gfortran_*/app/fac` | `./fac` |
| Incremental Builds | Efficient | Basic |
| C Code Handling | Automatic | Explicit gcc compilation |

## Compiler Flags

Both systems use the same optimization flags for release builds:

- `-O2` - Optimization level 2 (good balance of speed and size)
- `-Wall` - Enable all warnings
- `-ffree-line-length-none` - Allow long Fortran lines

## Binary Size Comparison

With identical flags, both systems produce nearly identical binaries:

- Makefile build: ~287KB
- fpm optimized build: ~294KB

The slight difference (~7KB) is due to:
- Different linking order
- Metadata differences
- Path information

## Platform-Specific Notes

### macOS (Apple Silicon)
- Makefile automatically finds Homebrew's gfortran in `/opt/homebrew`
- fpm uses whatever gfortran is in PATH
- Ensure Homebrew's gfortran is in PATH for consistency

### Linux
- Both systems work identically
- No special configuration needed

## Debug Builds

```bash
# fpm debug build
fpm build --flag "-g -Wall -ffree-line-length-none -fbacktrace -fcheck=bounds"

# Makefile doesn't have debug configuration
# Edit FFLAGS manually if needed
```

## Recommendations

1. **For Development**: Use fpm (better incremental builds)
2. **For Distribution**:
   - macOS: Use Makefile (better compiler detection)
   - Linux: Either works fine
3. **For CI/CD**: Use fpm with explicit flags

## Build Script

Use the provided `build.sh` script for unified building:

```bash
./build.sh release  # Optimized build
./build.sh debug    # Debug build (fpm only)
./build.sh clean    # Clean build artifacts
```

The script automatically detects available build systems and uses appropriate flags for parity.