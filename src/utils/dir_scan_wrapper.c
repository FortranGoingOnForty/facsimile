// Directory scan wrapper - native readdir for lazy file-tree expansion.
// Called from Fortran (dir_scan_module.f90) with length-passed strings,
// following the same conventions as platform_wrapper.c / regex_wrapper.c.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <dirent.h>

#define DIR_SCAN_PATH_MAX 4096

// Opaque handle: keeps the DIR stream plus a copy of the directory path so
// dir_next_f can build "path/name" for the stat() fallback.
typedef struct {
    DIR* dir;
    char path[DIR_SCAN_PATH_MAX];
} dir_handle_t;

// Open a directory for iteration. Returns NULL on failure.
void* dir_open_f(const char* path, int path_len) {
    dir_handle_t* h;

    if (!path || path_len <= 0 || path_len >= DIR_SCAN_PATH_MAX) return NULL;

    h = (dir_handle_t*)malloc(sizeof(dir_handle_t));
    if (!h) return NULL;

    memcpy(h->path, path, (size_t)path_len);
    h->path[path_len] = '\0';

    h->dir = opendir(h->path);
    if (!h->dir) {
        free(h);
        return NULL;
    }
    return h;
}

// Fetch the next entry, skipping "." and "..".
// Returns 1 = entry written, 0 = end of directory, -1 = bad arguments.
// name_buf receives up to name_cap bytes (NOT null-terminated; Fortran
// uses name_len). is_dir is 1 for directories (symlinks resolved), else 0.
int dir_next_f(void* handle, char* name_buf, int name_cap,
               int* name_len, int* is_dir) {
    dir_handle_t* h = (dir_handle_t*)handle;
    struct dirent* ent;
    size_t len;
    int isdir;

    if (!h || !h->dir || !name_buf || name_cap <= 0 || !name_len || !is_dir)
        return -1;

    for (;;) {
        ent = readdir(h->dir);
        if (!ent) return 0;
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0)
            continue;
        break;
    }

    len = strlen(ent->d_name);
    if (len > (size_t)name_cap) len = (size_t)name_cap;
    memcpy(name_buf, ent->d_name, len);
    *name_len = (int)len;

    // Prefer d_type when it gives a definite answer; DT_UNKNOWN, DT_LNK and
    // platforms without d_type (some MinGW/filesystem combos) fall back to
    // stat(), which follows symlinks - a symlinked dir lists as a dir, safe
    // here because nothing auto-descends.
    isdir = -1;
#ifdef DT_DIR
    if (ent->d_type == DT_DIR) {
        isdir = 1;
    } else if (ent->d_type == DT_REG) {
        isdir = 0;
    }
#endif
    if (isdir < 0) {
        char full[DIR_SCAN_PATH_MAX + 300];
        struct stat st;
        snprintf(full, sizeof(full), "%s/%s", h->path, ent->d_name);
        if (stat(full, &st) == 0 && S_ISDIR(st.st_mode)) {
            isdir = 1;
        } else {
            isdir = 0;
        }
    }
    *is_dir = isdir;
    return 1;
}

// Release the handle. Safe on NULL.
void dir_close_f(void* handle) {
    dir_handle_t* h = (dir_handle_t*)handle;
    if (!h) return;
    if (h->dir) closedir(h->dir);
    free(h);
}
