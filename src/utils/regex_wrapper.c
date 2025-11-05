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
