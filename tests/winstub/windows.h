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

/* Process and pipe plumbing, for the language-server wrapper. Added after a
   Windows-only definition went missing and nothing noticed: the file was not
   in WIN_CHECK_SRC because the stub could not get through it. */
#define TRUE 1
#define FALSE 0
#define INVALID_HANDLE_VALUE ((HANDLE)-1)
#define HANDLE_FLAG_INHERIT 1
#define STARTF_USESTDHANDLES 0x00000100
#define CREATE_NO_WINDOW 0x08000000
#define STILL_ACTIVE 259
#define INFINITE 0xFFFFFFFF
#define ZeroMemory(p, n) memset((p), 0, (n))
typedef struct { DWORD nLength; void* lpSecurityDescriptor; BOOL bInheritHandle; } SECURITY_ATTRIBUTES;
typedef struct { DWORD cb; char* lpReserved; char* lpDesktop; char* lpTitle;
                 DWORD dwX, dwY, dwXSize, dwYSize, dwXCountChars, dwYCountChars,
                       dwFillAttribute, dwFlags;
                 unsigned short wShowWindow, cbReserved2; unsigned char* lpReserved2;
                 HANDLE hStdInput, hStdOutput, hStdError; } STARTUPINFOA;
typedef struct { HANDLE hProcess; HANDLE hThread; DWORD dwProcessId, dwThreadId; } PROCESS_INFORMATION;
BOOL CreatePipe(HANDLE*, HANDLE*, SECURITY_ATTRIBUTES*, DWORD);
BOOL SetHandleInformation(HANDLE, DWORD, DWORD);
BOOL CreateProcessA(const char*, char*, SECURITY_ATTRIBUTES*, SECURITY_ATTRIBUTES*,
                    BOOL, DWORD, void*, const char*, STARTUPINFOA*, PROCESS_INFORMATION*);
BOOL PeekNamedPipe(HANDLE, void*, DWORD, DWORD*, DWORD*, DWORD*);
BOOL ReadFile(HANDLE, void*, DWORD, DWORD*, void*);
BOOL WriteFile(HANDLE, const void*, DWORD, DWORD*, void*);
BOOL GetExitCodeProcess(HANDLE, DWORD*);
BOOL TerminateProcess(HANDLE, unsigned);
DWORD WaitForSingleObject(HANDLE, DWORD);
BOOL CloseHandle(HANDLE);
#endif
