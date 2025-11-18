#!/bin/bash

# Build script for facsimile editor
# Provides parity between fpm and Makefile builds

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default build type
BUILD_TYPE=${1:-release}

echo -e "${BLUE}Building facsimile editor (${BUILD_TYPE})...${NC}"

# Check if fpm is available
if command -v fpm &> /dev/null; then
    echo -e "${GREEN}Using fpm build system${NC}"

    case "$BUILD_TYPE" in
        debug)
            fpm build --flag "-g -Wall -ffree-line-length-none -fbacktrace -fcheck=bounds"
            BINARY="./build/gfortran_*/app/fac"
            ;;
        release)
            # Use same flags as Makefile for optimization parity
            fpm build --flag "-O2 -Wall -ffree-line-length-none"
            BINARY="./build/gfortran_*/app/fac"
            ;;
        clean)
            fpm clean
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown build type: ${BUILD_TYPE}${NC}"
            echo "Usage: $0 [debug|release|clean]"
            exit 1
            ;;
    esac

    # Show location of built binary
    echo -e "${GREEN}Build complete!${NC}"
    echo -e "Binary location: ${BINARY}"
    echo -e "To install: cp ${BINARY} /usr/local/bin/fac"

elif [ -f "Makefile" ]; then
    echo -e "${GREEN}Using Makefile build system${NC}"

    case "$BUILD_TYPE" in
        debug)
            echo -e "${RED}Debug build not configured in Makefile${NC}"
            echo "Edit FFLAGS in Makefile to add debug flags"
            exit 1
            ;;
        release)
            make clean && make
            BINARY="./fac"
            ;;
        clean)
            make clean
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown build type: ${BUILD_TYPE}${NC}"
            echo "Usage: $0 [debug|release|clean]"
            exit 1
            ;;
    esac

    # Show location of built binary
    echo -e "${GREEN}Build complete!${NC}"
    echo -e "Binary location: ${BINARY}"
    echo -e "To install: cp ${BINARY} /usr/local/bin/"

else
    echo -e "${RED}No build system found!${NC}"
    echo "Please install fpm or ensure Makefile is present"
    exit 1
fi