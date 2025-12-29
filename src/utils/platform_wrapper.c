// Platform wrapper - cross-platform utilities for clipboard and paths
// Windows uses native APIs, Unix uses shell commands/POSIX

#ifdef _WIN32
// Windows implementation
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <shlobj.h>

// Get temp directory path
void get_temp_dir_f(char* buffer, int buffer_len, int* result_len) {
    char temp_path[MAX_PATH];
    DWORD len = GetTempPathA(MAX_PATH, temp_path);

    if (len == 0 || len > MAX_PATH) {
        // Fallback
        strncpy(temp_path, "C:\\Temp\\", MAX_PATH);
        len = 8;
    }

    // Ensure it ends with backslash
    if (temp_path[len-1] != '\\') {
        temp_path[len] = '\\';
        temp_path[len+1] = '\0';
        len++;
    }

    int copy_len = (int)len < buffer_len ? (int)len : buffer_len - 1;
    strncpy(buffer, temp_path, copy_len);
    buffer[copy_len] = '\0';
    *result_len = copy_len;
}

// Get home directory path
void get_home_dir_f(char* buffer, int buffer_len, int* result_len) {
    char home_path[MAX_PATH];

    // Try USERPROFILE first
    DWORD len = GetEnvironmentVariableA("USERPROFILE", home_path, MAX_PATH);
    if (len == 0 || len > MAX_PATH) {
        // Try HOMEDRIVE + HOMEPATH
        char drive[MAX_PATH], path[MAX_PATH];
        GetEnvironmentVariableA("HOMEDRIVE", drive, MAX_PATH);
        GetEnvironmentVariableA("HOMEPATH", path, MAX_PATH);
        snprintf(home_path, MAX_PATH, "%s%s", drive, path);
        len = (DWORD)strlen(home_path);
    }

    int copy_len = (int)len < buffer_len ? (int)len : buffer_len - 1;
    strncpy(buffer, home_path, copy_len);
    buffer[copy_len] = '\0';
    *result_len = copy_len;
}

// Get path separator character
char get_path_separator_f(void) {
    return '\\';
}

// Copy text to clipboard
int copy_to_clipboard_f(const char* text, int text_len) {
    if (!text || text_len <= 0) return 0;

    if (!OpenClipboard(NULL)) return 0;

    EmptyClipboard();

    // Allocate global memory for the text
    HGLOBAL hGlobal = GlobalAlloc(GMEM_MOVEABLE, text_len + 1);
    if (!hGlobal) {
        CloseClipboard();
        return 0;
    }

    // Copy text to global memory
    char* pGlobal = (char*)GlobalLock(hGlobal);
    if (!pGlobal) {
        GlobalFree(hGlobal);
        CloseClipboard();
        return 0;
    }

    memcpy(pGlobal, text, text_len);
    pGlobal[text_len] = '\0';
    GlobalUnlock(hGlobal);

    // Set clipboard data
    if (!SetClipboardData(CF_TEXT, hGlobal)) {
        GlobalFree(hGlobal);
        CloseClipboard();
        return 0;
    }

    CloseClipboard();
    return 1;
}

// Paste text from clipboard
int paste_from_clipboard_f(char* buffer, int buffer_len, int* result_len) {
    *result_len = 0;

    if (!IsClipboardFormatAvailable(CF_TEXT)) return 0;
    if (!OpenClipboard(NULL)) return 0;

    HGLOBAL hGlobal = GetClipboardData(CF_TEXT);
    if (!hGlobal) {
        CloseClipboard();
        return 0;
    }

    char* pGlobal = (char*)GlobalLock(hGlobal);
    if (!pGlobal) {
        CloseClipboard();
        return 0;
    }

    int text_len = (int)strlen(pGlobal);
    int copy_len = text_len < buffer_len - 1 ? text_len : buffer_len - 1;

    memcpy(buffer, pGlobal, copy_len);
    buffer[copy_len] = '\0';
    *result_len = copy_len;

    GlobalUnlock(hGlobal);
    CloseClipboard();

    return 1;
}

// Check if running on Windows
int is_windows_f(void) {
    return 1;
}

// Get config directory (Windows: %APPDATA%)
void get_config_dir_f(char* buffer, int buffer_len, int* result_len) {
    char config_path[MAX_PATH];

    // Use APPDATA environment variable
    DWORD len = GetEnvironmentVariableA("APPDATA", config_path, MAX_PATH);
    if (len == 0 || len > MAX_PATH) {
        // Fallback to home directory
        get_home_dir_f(config_path, MAX_PATH, (int*)&len);
    }

    int copy_len = (int)len < buffer_len ? (int)len : buffer_len - 1;
    strncpy(buffer, config_path, copy_len);
    buffer[copy_len] = '\0';
    *result_len = copy_len;
}

// Get current working directory
void get_cwd_f(char* buffer, int buffer_len, int* result_len) {
    DWORD len = GetCurrentDirectoryA(buffer_len, buffer);
    *result_len = (int)len;
}

#else
// Unix implementation
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pwd.h>
#include <sys/types.h>

// Get temp directory path
void get_temp_dir_f(char* buffer, int buffer_len, int* result_len) {
    const char* tmp = getenv("TMPDIR");
    if (!tmp) tmp = getenv("TMP");
    if (!tmp) tmp = getenv("TEMP");
    if (!tmp) tmp = "/tmp";

    int len = strlen(tmp);

    // Ensure it ends with slash
    int needs_slash = (tmp[len-1] != '/') ? 1 : 0;
    int copy_len = len + needs_slash;
    if (copy_len >= buffer_len) copy_len = buffer_len - 1;

    int bytes_to_copy = len < buffer_len - 1 ? len : buffer_len - 1;
    memcpy(buffer, tmp, bytes_to_copy);
    if (needs_slash && len < buffer_len - 1) {
        buffer[len] = '/';
        buffer[len + 1] = '\0';
        *result_len = len + 1;
    } else {
        buffer[copy_len] = '\0';
        *result_len = copy_len;
    }
}

// Get home directory path
void get_home_dir_f(char* buffer, int buffer_len, int* result_len) {
    const char* home = getenv("HOME");
    if (!home) {
        struct passwd* pw = getpwuid(getuid());
        if (pw) home = pw->pw_dir;
    }
    if (!home) home = "/tmp";

    int len = strlen(home);
    int copy_len = len < buffer_len - 1 ? len : buffer_len - 1;
    strncpy(buffer, home, copy_len);
    buffer[copy_len] = '\0';
    *result_len = copy_len;
}

// Get path separator character
char get_path_separator_f(void) {
    return '/';
}

// Copy text to clipboard (uses shell commands)
int copy_to_clipboard_f(const char* text, int text_len) {
    // This is a fallback - the Fortran code handles this via shell commands
    // Return 0 to indicate Fortran should handle it
    (void)text;
    (void)text_len;
    return 0;
}

// Paste text from clipboard (uses shell commands)
int paste_from_clipboard_f(char* buffer, int buffer_len, int* result_len) {
    // This is a fallback - the Fortran code handles this via shell commands
    // Return 0 to indicate Fortran should handle it
    (void)buffer;
    (void)buffer_len;
    *result_len = 0;
    return 0;
}

// Check if running on Windows
int is_windows_f(void) {
    return 0;
}

// Get config directory (Unix: ~/.config or XDG_CONFIG_HOME)
void get_config_dir_f(char* buffer, int buffer_len, int* result_len) {
    const char* config = getenv("XDG_CONFIG_HOME");
    if (config && strlen(config) > 0) {
        int len = strlen(config);
        int copy_len = len < buffer_len - 1 ? len : buffer_len - 1;
        memcpy(buffer, config, copy_len);
        buffer[copy_len] = '\0';
        *result_len = copy_len;
        return;
    }

    // Default to ~/.config
    const char* home = getenv("HOME");
    if (!home) {
        struct passwd* pw = getpwuid(getuid());
        if (pw) home = pw->pw_dir;
    }
    if (!home) home = "/tmp";

    int len = snprintf(buffer, buffer_len, "%s/.config", home);
    *result_len = len < buffer_len ? len : buffer_len - 1;
}

// Get current working directory
void get_cwd_f(char* buffer, int buffer_len, int* result_len) {
    if (getcwd(buffer, buffer_len)) {
        *result_len = strlen(buffer);
    } else {
        buffer[0] = '\0';
        *result_len = 0;
    }
}

#endif
