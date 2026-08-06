# Windows syntax stubs

Just enough of `windows.h` to **syntax-check** the `_WIN32` branches of the C
wrappers on Linux. Not a Windows SDK, and nothing built with it runs.

It exists because the Windows branches are invisible to the normal build, so a
mistake in one is only found by CI — after a tag, when the release job refuses
to publish and the version has to be burned. That has now happened twice: once
for a binding with no Windows stub at all, and once for a function accidentally
defined *outside* `#ifdef`, which is harmless on Linux (only one definition
compiles) and a redefinition error on Windows.

    make check-windows

Covers the wrappers that this stub is sufficient for. `lsp_process_wrapper.c`,
`regex_wrapper.c` and `termios_wrapper.c` need more of the API stubbed and are
not covered yet — add what they need here and extend the target.
