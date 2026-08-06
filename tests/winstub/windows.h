/* Minimal stub: enough to SYNTAX-CHECK the _WIN32 branch on Linux.
   Not a Windows SDK -- signatures only need to be close enough to compile. */
#ifndef FAC_WINSTUB_H
#define FAC_WINSTUB_H
#include <stddef.h>
typedef unsigned long DWORD;
typedef void* HGLOBAL;
typedef void* HANDLE;
typedef int BOOL;
#define MAX_PATH 260
#define GMEM_MOVEABLE 2
#define CF_TEXT 1
#define ERROR_ALREADY_EXISTS 183L
void Sleep(DWORD);
DWORD GetLastError(void);
BOOL CreateDirectoryA(const char*, void*);
DWORD GetTempPathA(DWORD, char*);
DWORD GetCurrentDirectoryA(DWORD, char*);
DWORD GetEnvironmentVariableA(const char*, char*, DWORD);
BOOL OpenClipboard(HANDLE);
BOOL CloseClipboard(void);
BOOL EmptyClipboard(void);
HANDLE GetClipboardData(unsigned);
HANDLE SetClipboardData(unsigned, HANDLE);
BOOL IsClipboardFormatAvailable(unsigned);
HGLOBAL GlobalAlloc(unsigned, size_t);
void* GlobalLock(HGLOBAL);
BOOL GlobalUnlock(HGLOBAL);
HGLOBAL GlobalFree(HGLOBAL);
#endif
