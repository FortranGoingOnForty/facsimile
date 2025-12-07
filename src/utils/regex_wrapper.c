// Regex wrapper - platform independent implementation
// Uses POSIX regex on Unix, simplified matching on Windows

#ifdef _WIN32
// Windows implementation - simplified pattern matching
// For full regex support on Windows, consider using PCRE2

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_REGEX 10

typedef struct {
    char* pattern;
    int case_insensitive;
    int used;
} regex_entry_t;

static regex_entry_t regex_storage[MAX_REGEX];

// Initialize storage
static void init_storage(void) {
    static int initialized = 0;
    if (!initialized) {
        memset(regex_storage, 0, sizeof(regex_storage));
        initialized = 1;
    }
}

// Simple wildcard/literal pattern matching
// Supports: literal text, case-insensitive matching
// For Windows, we do basic substring matching
static int simple_match(const char* pattern, const char* text, int case_insensitive,
                        int* match_start, int* match_len) {
    if (!pattern || !text) return 0;

    size_t plen = strlen(pattern);
    size_t tlen = strlen(text);

    if (plen == 0) {
        *match_start = 0;
        *match_len = 0;
        return 1;
    }

    // Simple substring search
    for (size_t i = 0; i <= tlen - plen; i++) {
        int matched = 1;
        for (size_t j = 0; j < plen; j++) {
            char pc = pattern[j];
            char tc = text[i + j];

            if (case_insensitive) {
                pc = (char)tolower((unsigned char)pc);
                tc = (char)tolower((unsigned char)tc);
            }

            if (pc != tc) {
                matched = 0;
                break;
            }
        }

        if (matched) {
            *match_start = (int)i;
            *match_len = (int)plen;
            return 1;
        }
    }

    return 0;  // No match
}

// Compile a regex pattern and return an ID
// cflags: 1 = case insensitive (REG_ICASE equivalent)
int compile_regex(const char* pattern, int cflags) {
    init_storage();

    int id = -1;

    // Find free slot
    for (int i = 0; i < MAX_REGEX; i++) {
        if (!regex_storage[i].used) {
            id = i;
            break;
        }
    }

    if (id == -1) {
        return -1;  // No free slots
    }

    // Store the pattern
    regex_storage[id].pattern = _strdup(pattern);
    if (!regex_storage[id].pattern) {
        return -1;
    }

    regex_storage[id].case_insensitive = (cflags & 1);  // REG_ICASE = 1
    regex_storage[id].used = 1;

    return id;
}

// Match a compiled regex against text
int match_regex(int id, const char* text, int* match_start, int* match_len) {
    if (id < 0 || id >= MAX_REGEX || !regex_storage[id].used) {
        return -1;  // Invalid ID
    }

    if (simple_match(regex_storage[id].pattern, text,
                     regex_storage[id].case_insensitive,
                     match_start, match_len)) {
        return 1;  // Match found
    }

    return 0;  // No match
}

// Free a compiled regex
void free_regex(int id) {
    if (id >= 0 && id < MAX_REGEX && regex_storage[id].used) {
        free(regex_storage[id].pattern);
        regex_storage[id].pattern = NULL;
        regex_storage[id].used = 0;
    }
}

// Free all compiled regexes
void free_all_regex(void) {
    for (int i = 0; i < MAX_REGEX; i++) {
        if (regex_storage[i].used) {
            free(regex_storage[i].pattern);
            regex_storage[i].pattern = NULL;
            regex_storage[i].used = 0;
        }
    }
}

#else
// Unix implementation using POSIX regex

#include <regex.h>
#include <stdlib.h>
#include <string.h>

// Maximum number of compiled regex patterns we can store
#define MAX_REGEX 10

// Storage for compiled regex patterns
static regex_t regex_storage[MAX_REGEX];
static int regex_used[MAX_REGEX] = {0};

// Compile a regex pattern and return an ID (index into storage)
// Returns -1 on error, >= 0 on success
int compile_regex(const char *pattern, int cflags) {
    int id = -1;

    // Find free slot
    for (int i = 0; i < MAX_REGEX; i++) {
        if (!regex_used[i]) {
            id = i;
            break;
        }
    }

    if (id == -1) {
        return -1;  // No free slots
    }

    // Compile the pattern
    int result = regcomp(&regex_storage[id], pattern, cflags);
    if (result != 0) {
        return -1;  // Compilation failed
    }

    regex_used[id] = 1;
    return id;
}

// Match a compiled regex against text
// Returns 1 if match found, 0 if no match, -1 on error
int match_regex(int id, const char *text, int *match_start, int *match_len) {
    if (id < 0 || id >= MAX_REGEX || !regex_used[id]) {
        return -1;  // Invalid ID
    }

    regmatch_t pmatch[1];
    int result = regexec(&regex_storage[id], text, 1, pmatch, 0);

    if (result == 0) {
        // Match found - return start position and length
        *match_start = (int)pmatch[0].rm_so;
        *match_len = (int)(pmatch[0].rm_eo - pmatch[0].rm_so);
        return 1;
    } else if (result == REG_NOMATCH) {
        return 0;  // No match
    } else {
        return -1;  // Error
    }
}

// Free a compiled regex
void free_regex(int id) {
    if (id >= 0 && id < MAX_REGEX && regex_used[id]) {
        regfree(&regex_storage[id]);
        regex_used[id] = 0;
    }
}

// Free all compiled regexes
void free_all_regex(void) {
    for (int i = 0; i < MAX_REGEX; i++) {
        if (regex_used[i]) {
            regfree(&regex_storage[i]);
            regex_used[i] = 0;
        }
    }
}

#endif
