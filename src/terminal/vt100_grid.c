// VT100 cell grid with ANSI escape sequence parser
// Minimal terminal emulator for integrated terminal panel

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

// Cell attributes
#define ATTR_BOLD      0x01
#define ATTR_DIM       0x02
#define ATTR_ITALIC    0x04
#define ATTR_UNDERLINE 0x08
#define ATTR_INVERSE   0x10

// Parser states
#define STATE_NORMAL 0
#define STATE_ESC    1
#define STATE_CSI    2
#define STATE_OSC    3
#define STATE_DCS    4

#define MAX_PARAMS 16
#define PARAM_BUF_SIZE 128

typedef struct {
    char ch;
    unsigned char fg;   // 0 = default
    unsigned char bg;   // 0 = default
    unsigned char attr;
} vt100_cell_t;

typedef struct {
    vt100_cell_t *cells;
    int rows;
    int cols;
    int cursor_row;     // 0-based
    int cursor_col;     // 0-based
    int saved_row;      // saved cursor
    int saved_col;
    int scroll_top;     // 0-based, inclusive
    int scroll_bottom;  // 0-based, inclusive
    unsigned char cur_fg;
    unsigned char cur_bg;
    unsigned char cur_attr;
    // Parser state
    int parse_state;
    char param_buf[PARAM_BUF_SIZE];
    int param_len;
    int cursor_visible;
    int cpr_pending;
    int pty_fd;         // PTY fd for inline query responses
    int app_cursor_keys; // DECCKM: 1=application mode (ESC O), 0=normal (ESC [)
} vt100_grid_t;

// ---- Internal helpers ----

static vt100_cell_t *cell_at(vt100_grid_t *g, int row, int col) {
    if (row < 0 || row >= g->rows || col < 0 || col >= g->cols)
        return NULL;
    return &g->cells[row * g->cols + col];
}

static void clear_cell(vt100_cell_t *c) {
    c->ch = ' ';
    c->fg = 0;
    c->bg = 0;
    c->attr = 0;
}

static void clear_region(vt100_grid_t *g, int r1, int c1, int r2, int c2) {
    for (int r = r1; r <= r2 && r < g->rows; r++) {
        for (int c = c1; c <= c2 && c < g->cols; c++) {
            if (r >= 0 && c >= 0)
                clear_cell(cell_at(g, r, c));
        }
    }
}

static void scroll_up(vt100_grid_t *g, int top, int bottom, int n) {
    if (n <= 0 || top > bottom) return;
    if (n > bottom - top + 1) n = bottom - top + 1;

    // Move lines up
    for (int r = top; r <= bottom - n; r++) {
        memcpy(&g->cells[r * g->cols],
               &g->cells[(r + n) * g->cols],
               g->cols * sizeof(vt100_cell_t));
    }
    // Clear bottom n lines
    for (int r = bottom - n + 1; r <= bottom; r++) {
        for (int c = 0; c < g->cols; c++) {
            clear_cell(cell_at(g, r, c));
        }
    }
}

static void scroll_down(vt100_grid_t *g, int top, int bottom, int n) {
    if (n <= 0 || top > bottom) return;
    if (n > bottom - top + 1) n = bottom - top + 1;

    for (int r = bottom; r >= top + n; r--) {
        memcpy(&g->cells[r * g->cols],
               &g->cells[(r - n) * g->cols],
               g->cols * sizeof(vt100_cell_t));
    }
    for (int r = top; r < top + n; r++) {
        for (int c = 0; c < g->cols; c++) {
            clear_cell(cell_at(g, r, c));
        }
    }
}

// Parse CSI parameter string into integer array
static int parse_params(const char *buf, int len, int *params, int max) {
    int count = 0;
    int val = 0;
    int has_val = 0;

    for (int i = 0; i < len && count < max; i++) {
        if (buf[i] >= '0' && buf[i] <= '9') {
            val = val * 10 + (buf[i] - '0');
            has_val = 1;
        } else if (buf[i] == ';') {
            params[count++] = has_val ? val : 0;
            val = 0;
            has_val = 0;
        }
    }
    if (has_val || count > 0) {
        if (count < max) params[count++] = val;
    }
    return count;
}

// Handle SGR (Select Graphic Rendition) — CSI ... m
static void handle_sgr(vt100_grid_t *g, int *params, int nparams) {
    if (nparams == 0) {
        // Reset
        g->cur_fg = 0;
        g->cur_bg = 0;
        g->cur_attr = 0;
        return;
    }
    for (int i = 0; i < nparams; i++) {
        int p = params[i];
        if (p == 0) {
            g->cur_fg = 0; g->cur_bg = 0; g->cur_attr = 0;
        } else if (p == 1) {
            g->cur_attr |= ATTR_BOLD;
        } else if (p == 2) {
            g->cur_attr |= ATTR_DIM;
        } else if (p == 3) {
            g->cur_attr |= ATTR_ITALIC;
        } else if (p == 4) {
            g->cur_attr |= ATTR_UNDERLINE;
        } else if (p == 7) {
            g->cur_attr |= ATTR_INVERSE;
        } else if (p == 22) {
            g->cur_attr &= ~(ATTR_BOLD | ATTR_DIM);
        } else if (p == 23) {
            g->cur_attr &= ~ATTR_ITALIC;
        } else if (p == 24) {
            g->cur_attr &= ~ATTR_UNDERLINE;
        } else if (p == 27) {
            g->cur_attr &= ~ATTR_INVERSE;
        } else if (p >= 30 && p <= 37) {
            g->cur_fg = p - 30 + 1; // 1-8 for standard colors
        } else if (p == 38) {
            // Extended foreground: 38;5;N or 38;2;R;G;B
            if (i + 1 < nparams && params[i + 1] == 5 &&
                i + 2 < nparams) {
                g->cur_fg = params[i + 2] + 1; // +1 so 0=default
                i += 2;
            } else if (i + 1 < nparams && params[i + 1] == 2 &&
                       i + 4 < nparams) {
                // True color — approximate to 256-color
                int r = params[i+2], gr = params[i+3], b = params[i+4];
                if (r == gr && gr == b) {
                    // Grayscale
                    g->cur_fg = 232 + (r * 24 / 256) + 1;
                } else {
                    g->cur_fg = 16 + (r*6/256)*36 + (gr*6/256)*6 +
                                (b*6/256) + 1;
                }
                i += 4;
            }
        } else if (p == 39) {
            g->cur_fg = 0; // Default foreground
        } else if (p >= 40 && p <= 47) {
            g->cur_bg = p - 40 + 1;
        } else if (p == 48) {
            // Extended background
            if (i + 1 < nparams && params[i + 1] == 5 &&
                i + 2 < nparams) {
                g->cur_bg = params[i + 2] + 1;
                i += 2;
            } else if (i + 1 < nparams && params[i + 1] == 2 &&
                       i + 4 < nparams) {
                int r = params[i+2], gr = params[i+3], b = params[i+4];
                if (r == gr && gr == b) {
                    g->cur_bg = 232 + (r * 24 / 256) + 1;
                } else {
                    g->cur_bg = 16 + (r*6/256)*36 + (gr*6/256)*6 +
                                (b*6/256) + 1;
                }
                i += 4;
            }
        } else if (p == 49) {
            g->cur_bg = 0; // Default background
        } else if (p >= 90 && p <= 97) {
            g->cur_fg = p - 90 + 9; // Bright colors 9-16
        } else if (p >= 100 && p <= 107) {
            g->cur_bg = p - 100 + 9;
        }
    }
}

// Handle a complete CSI sequence
static void handle_csi(vt100_grid_t *g, char final) {
    int params[MAX_PARAMS];
    int np = parse_params(g->param_buf, g->param_len, params,
                          MAX_PARAMS);
    int p1 = np > 0 ? params[0] : 0;
    int p2 = np > 1 ? params[1] : 0;

    // Check for private mode prefix '?'
    int is_private = (g->param_len > 0 && g->param_buf[0] == '?');

    switch (final) {
    case 'A': // Cursor up
        if (p1 < 1) p1 = 1;
        g->cursor_row -= p1;
        if (g->cursor_row < 0) g->cursor_row = 0;
        break;
    case 'B': // Cursor down
        if (p1 < 1) p1 = 1;
        g->cursor_row += p1;
        if (g->cursor_row >= g->rows) g->cursor_row = g->rows - 1;
        break;
    case 'C': // Cursor forward
        if (p1 < 1) p1 = 1;
        g->cursor_col += p1;
        if (g->cursor_col >= g->cols) g->cursor_col = g->cols - 1;
        break;
    case 'D': // Cursor back
        if (p1 < 1) p1 = 1;
        g->cursor_col -= p1;
        if (g->cursor_col < 0) g->cursor_col = 0;
        break;
    case 'E': // Cursor next line
        if (p1 < 1) p1 = 1;
        g->cursor_row += p1;
        if (g->cursor_row >= g->rows) g->cursor_row = g->rows - 1;
        g->cursor_col = 0;
        break;
    case 'F': // Cursor previous line
        if (p1 < 1) p1 = 1;
        g->cursor_row -= p1;
        if (g->cursor_row < 0) g->cursor_row = 0;
        g->cursor_col = 0;
        break;
    case 'G': // Cursor horizontal absolute
        if (p1 < 1) p1 = 1;
        g->cursor_col = p1 - 1;
        if (g->cursor_col >= g->cols) g->cursor_col = g->cols - 1;
        break;
    case 'H': // Cursor position
    case 'f':
        if (p1 < 1) p1 = 1;
        if (p2 < 1) p2 = 1;
        g->cursor_row = p1 - 1;
        g->cursor_col = p2 - 1;
        if (g->cursor_row >= g->rows) g->cursor_row = g->rows - 1;
        if (g->cursor_col >= g->cols) g->cursor_col = g->cols - 1;
        break;
    case 'J': // Erase display
        if (p1 == 0) {
            // Erase from cursor to end
            clear_region(g, g->cursor_row, g->cursor_col,
                         g->cursor_row, g->cols - 1);
            clear_region(g, g->cursor_row + 1, 0,
                         g->rows - 1, g->cols - 1);
        } else if (p1 == 1) {
            // Erase from start to cursor
            clear_region(g, 0, 0, g->cursor_row - 1, g->cols - 1);
            clear_region(g, g->cursor_row, 0,
                         g->cursor_row, g->cursor_col);
        } else if (p1 == 2 || p1 == 3) {
            // Erase entire display
            clear_region(g, 0, 0, g->rows - 1, g->cols - 1);
        }
        break;
    case 'K': // Erase line
        if (p1 == 0) {
            clear_region(g, g->cursor_row, g->cursor_col,
                         g->cursor_row, g->cols - 1);
        } else if (p1 == 1) {
            clear_region(g, g->cursor_row, 0,
                         g->cursor_row, g->cursor_col);
        } else if (p1 == 2) {
            clear_region(g, g->cursor_row, 0,
                         g->cursor_row, g->cols - 1);
        }
        break;
    case 'L': // Insert lines
        if (p1 < 1) p1 = 1;
        scroll_down(g, g->cursor_row, g->scroll_bottom, p1);
        break;
    case 'M': // Delete lines
        if (p1 < 1) p1 = 1;
        scroll_up(g, g->cursor_row, g->scroll_bottom, p1);
        break;
    case 'P': // Delete characters
        if (p1 < 1) p1 = 1;
        for (int c = g->cursor_col; c < g->cols - p1; c++) {
            g->cells[g->cursor_row * g->cols + c] =
                g->cells[g->cursor_row * g->cols + c + p1];
        }
        for (int c = g->cols - p1; c < g->cols; c++) {
            clear_cell(cell_at(g, g->cursor_row, c));
        }
        break;
    case '@': // Insert characters
        if (p1 < 1) p1 = 1;
        for (int c = g->cols - 1; c >= g->cursor_col + p1; c--) {
            g->cells[g->cursor_row * g->cols + c] =
                g->cells[g->cursor_row * g->cols + c - p1];
        }
        for (int c = g->cursor_col;
             c < g->cursor_col + p1 && c < g->cols; c++) {
            clear_cell(cell_at(g, g->cursor_row, c));
        }
        break;
    case 'S': // Scroll up
        if (p1 < 1) p1 = 1;
        scroll_up(g, g->scroll_top, g->scroll_bottom, p1);
        break;
    case 'T': // Scroll down
        if (p1 < 1) p1 = 1;
        scroll_down(g, g->scroll_top, g->scroll_bottom, p1);
        break;
    case 'X': // Erase characters
        if (p1 < 1) p1 = 1;
        for (int c = g->cursor_col;
             c < g->cursor_col + p1 && c < g->cols; c++) {
            clear_cell(cell_at(g, g->cursor_row, c));
        }
        break;
    case 'd': // Vertical position absolute
        if (p1 < 1) p1 = 1;
        g->cursor_row = p1 - 1;
        if (g->cursor_row >= g->rows) g->cursor_row = g->rows - 1;
        break;
    case 'm': // SGR
        handle_sgr(g, params, np);
        break;
    case 'r': // Set scroll region (DECSTBM)
        if (p1 < 1) p1 = 1;
        if (p2 < 1) p2 = g->rows;
        g->scroll_top = p1 - 1;
        g->scroll_bottom = p2 - 1;
        if (g->scroll_top < 0) g->scroll_top = 0;
        if (g->scroll_bottom >= g->rows)
            g->scroll_bottom = g->rows - 1;
        g->cursor_row = 0;
        g->cursor_col = 0;
        break;
    case 'n': // Device Status Report
        if (p1 == 6 && g->pty_fd >= 0) {
            // CPR — respond IMMEDIATELY inline
            char cpr[32];
            int cpr_len = snprintf(cpr, sizeof(cpr),
                "\x1b[%d;%dR",
                g->cursor_row + 1, g->cursor_col + 1);
            write(g->pty_fd, cpr, cpr_len);
        }
        g->cpr_pending = 0;
        break;
    case 'h': // Set mode
        if (is_private) {
            if (p1 == 1)
                g->app_cursor_keys = 1;  // DECCKM
            else if (p1 == 25)
                g->cursor_visible = 1;
            else if (p1 == 1049 || p1 == 47) {
                // Enter alternate screen — clear grid
                clear_region(g, 0, 0, g->rows-1, g->cols-1);
                g->cursor_row = 0;
                g->cursor_col = 0;
            }
        }
        break;
    case 'l': // Reset mode
        if (is_private) {
            if (p1 == 1)
                g->app_cursor_keys = 0;  // DECCKM off
            else if (p1 == 25)
                g->cursor_visible = 0;
            else if (p1 == 1049 || p1 == 47) {
                // Leave alternate screen — clear and reset
                clear_region(g, 0, 0, g->rows-1, g->cols-1);
                g->cursor_row = 0;
                g->cursor_col = 0;
                g->scroll_top = 0;
                g->scroll_bottom = g->rows - 1;
            }
        }
        break;
    case 's': // Save cursor position
        g->saved_row = g->cursor_row;
        g->saved_col = g->cursor_col;
        break;
    case 'u': // Restore cursor position (only plain ESC[u)
        if (!is_private && g->param_len > 0 &&
            g->param_buf[0] != '=') {
            // ESC[u with no prefix — restore cursor
            g->cursor_row = g->saved_row;
            g->cursor_col = g->saved_col;
        } else if (!is_private && g->param_len == 0) {
            g->cursor_row = g->saved_row;
            g->cursor_col = g->saved_col;
        }
        // ESC[=Nu and ESC[?Nu — kitty keyboard protocol, ignore
        break;
    default:
        break; // Ignore unknown CSI sequences
    }
}

// Put a printable character at cursor and advance
static void put_char(vt100_grid_t *g, char ch) {
    if (g->cursor_col >= g->cols) {
        // Line wrap
        g->cursor_col = 0;
        g->cursor_row++;
        if (g->cursor_row > g->scroll_bottom) {
            g->cursor_row = g->scroll_bottom;
            scroll_up(g, g->scroll_top, g->scroll_bottom, 1);
        }
    }

    vt100_cell_t *c = cell_at(g, g->cursor_row, g->cursor_col);
    if (c) {
        c->ch = ch;
        c->fg = g->cur_fg;
        c->bg = g->cur_bg;
        c->attr = g->cur_attr;
    }
    g->cursor_col++;
}

// Feed data through the parser
static void grid_feed(vt100_grid_t *g, const char *data, int len) {
    for (int i = 0; i < len; i++) {
        unsigned char ch = (unsigned char)data[i];

        switch (g->parse_state) {
        case STATE_NORMAL:
            if (ch == 0x1B) {
                g->parse_state = STATE_ESC;
                g->param_len = 0;
            } else if (ch == '\r') {
                g->cursor_col = 0;
            } else if (ch == '\n') {
                g->cursor_row++;
                if (g->cursor_row > g->scroll_bottom) {
                    g->cursor_row = g->scroll_bottom;
                    scroll_up(g, g->scroll_top, g->scroll_bottom, 1);
                }
            } else if (ch == '\b') {
                if (g->cursor_col > 0) g->cursor_col--;
            } else if (ch == '\t') {
                int next_tab = (g->cursor_col / 8 + 1) * 8;
                if (next_tab >= g->cols) next_tab = g->cols - 1;
                g->cursor_col = next_tab;
            } else if (ch == 0x07) {
                // BEL — ignore
            } else if (ch >= 0x80 && ch <= 0xBF) {
                // UTF-8 continuation byte — skip (don't advance cursor)
            } else if (ch >= 0xC0 && ch <= 0xFD) {
                // UTF-8 lead byte — put a space placeholder
                // (real terminals render multi-byte chars as 1-2 columns)
                put_char(g, ' ');
            } else if (ch >= 32 && ch != 127) {
                put_char(g, (char)ch);
            }
            break;

        case STATE_ESC:
            if (ch == '[') {
                g->parse_state = STATE_CSI;
                g->param_len = 0;
            } else if (ch == ']') {
                g->parse_state = STATE_OSC;
                g->param_len = 0;
            } else if (ch == 'P') {
                // DCS (Device Control String) — consume until ST
                g->parse_state = STATE_DCS;
                g->param_len = 0;
            } else if (ch == '7') {
                // Save cursor
                g->saved_row = g->cursor_row;
                g->saved_col = g->cursor_col;
                g->parse_state = STATE_NORMAL;
            } else if (ch == '8') {
                // Restore cursor
                g->cursor_row = g->saved_row;
                g->cursor_col = g->saved_col;
                g->parse_state = STATE_NORMAL;
            } else if (ch == 'D') {
                // Index (scroll up)
                g->cursor_row++;
                if (g->cursor_row > g->scroll_bottom) {
                    g->cursor_row = g->scroll_bottom;
                    scroll_up(g, g->scroll_top, g->scroll_bottom, 1);
                }
                g->parse_state = STATE_NORMAL;
            } else if (ch == 'M') {
                // Reverse index (scroll down)
                g->cursor_row--;
                if (g->cursor_row < g->scroll_top) {
                    g->cursor_row = g->scroll_top;
                    scroll_down(g, g->scroll_top, g->scroll_bottom, 1);
                }
                g->parse_state = STATE_NORMAL;
            } else if (ch == 'c') {
                // Full reset
                g->cur_fg = 0; g->cur_bg = 0; g->cur_attr = 0;
                g->cursor_row = 0; g->cursor_col = 0;
                g->scroll_top = 0;
                g->scroll_bottom = g->rows - 1;
                clear_region(g, 0, 0, g->rows - 1, g->cols - 1);
                g->parse_state = STATE_NORMAL;
            } else {
                // Unknown ESC sequence, ignore
                g->parse_state = STATE_NORMAL;
            }
            break;

        case STATE_CSI:
            if ((ch >= '0' && ch <= '9') || ch == ';' || ch == '?' ||
                ch == '>' || ch == '!' || ch == '=' || ch == '<') {
                if (g->param_len < PARAM_BUF_SIZE - 1) {
                    g->param_buf[g->param_len++] = (char)ch;
                }
            } else if (ch >= 0x40 && ch <= 0x7E) {
                // Final byte — process CSI
                g->param_buf[g->param_len] = '\0';
                handle_csi(g, (char)ch);
                g->parse_state = STATE_NORMAL;
            } else {
                // Intermediate byte — ignore for now
            }
            break;

        case STATE_OSC:
            // Consume until BEL or ST (ESC \)
            if (ch == 0x07) {
                g->parse_state = STATE_NORMAL;
            } else if (ch == 0x1B) {
                // ESC starts the ST (ESC \). Go to ESC state so
                // the '\' is consumed as part of the sequence.
                g->parse_state = STATE_ESC;
            }
            break;

        case STATE_DCS:
            // Consume DCS (XTGETTCAP etc.) until ST (ESC \) or BEL
            if (ch == 0x07) {
                g->parse_state = STATE_NORMAL;
            } else if (ch == 0x1B) {
                g->parse_state = STATE_ESC;
            }
            // All other bytes silently consumed
            break;
        }
    }
}

// Debug: dump grid state
static void debug_log_grid(vt100_grid_t *g, const char *label) {
    FILE *f = fopen("/tmp/fac_grid.log", "a");
    if (!f) return;
    fprintf(f, "%s: cur=(%d,%d) scrl=(%d-%d) %dx%d\n",
            label, g->cursor_row, g->cursor_col,
            g->scroll_top, g->scroll_bottom, g->rows, g->cols);
    for (int r = 0; r < g->rows; r++) {
        int has = 0;
        for (int c = 0; c < g->cols; c++)
            if (g->cells[r*g->cols+c].ch != ' ') { has=1; break; }
        if (has) {
            fprintf(f, "  r%02d:[", r);
            for (int c = 0; c < g->cols && c < 70; c++) {
                char ch = g->cells[r*g->cols+c].ch;
                fputc((ch>=32 && ch<127) ? ch : '.', f);
            }
            fprintf(f, "]\n");
        }
    }
    fprintf(f, "---\n");
    fclose(f);
}

// ---- Fortran-callable API ----

void vt100_grid_create_f(void **handle, int *rows, int *cols) {
    vt100_grid_t *g = (vt100_grid_t *)calloc(1, sizeof(vt100_grid_t));
    g->rows = *rows;
    g->cols = *cols;
    g->cells = (vt100_cell_t *)calloc(*rows * *cols, sizeof(vt100_cell_t));
    g->scroll_top = 0;
    g->scroll_bottom = *rows - 1;
    g->cursor_visible = 1;
    g->cpr_pending = 0;
    g->pty_fd = -1;
    g->app_cursor_keys = 0;

    // Initialize all cells to spaces
    for (int i = 0; i < *rows * *cols; i++) {
        g->cells[i].ch = ' ';
    }

    *handle = g;
}

void vt100_grid_destroy_f(void **handle) {
    vt100_grid_t *g = (vt100_grid_t *)*handle;
    if (!g) return;
    if (g->cells) free(g->cells);
    free(g);
    *handle = NULL;
}

void vt100_grid_feed_f(void **handle, const char *data, int *len) {
    vt100_grid_t *g = (vt100_grid_t *)*handle;
    if (!g || !data || *len <= 0) return;
    grid_feed(g, data, *len);
}

void vt100_grid_resize_f(void **handle, int *rows, int *cols) {
    vt100_grid_t *g = (vt100_grid_t *)*handle;
    if (!g) return;

    int new_size = *rows * *cols;
    vt100_cell_t *new_cells = (vt100_cell_t *)calloc(
        new_size, sizeof(vt100_cell_t));

    // Initialize to spaces
    for (int i = 0; i < new_size; i++) {
        new_cells[i].ch = ' ';
    }

    // Copy existing content (as much as fits)
    int copy_rows = g->rows < *rows ? g->rows : *rows;
    int copy_cols = g->cols < *cols ? g->cols : *cols;
    for (int r = 0; r < copy_rows; r++) {
        for (int c = 0; c < copy_cols; c++) {
            new_cells[r * *cols + c] = g->cells[r * g->cols + c];
        }
    }

    free(g->cells);
    g->cells = new_cells;
    g->rows = *rows;
    g->cols = *cols;
    g->scroll_top = 0;
    g->scroll_bottom = *rows - 1;
    if (g->cursor_row >= *rows) g->cursor_row = *rows - 1;
    if (g->cursor_col >= *cols) g->cursor_col = *cols - 1;
}

void vt100_grid_get_cell_f(void **handle, int *row, int *col,
                            char *ch, int *fg, int *bg, int *attr) {
    vt100_grid_t *g = (vt100_grid_t *)*handle;
    if (!g) {
        *ch = ' '; *fg = 0; *bg = 0; *attr = 0;
        return;
    }
    vt100_cell_t *c = cell_at(g, *row, *col);
    if (c) {
        *ch = c->ch;
        *fg = c->fg;
        *bg = c->bg;
        *attr = c->attr;
    } else {
        *ch = ' '; *fg = 0; *bg = 0; *attr = 0;
    }
}

void vt100_grid_get_cursor_f(void **handle, int *row, int *col) {
    vt100_grid_t *g = (vt100_grid_t *)*handle;
    if (!g) { *row = 0; *col = 0; return; }
    *row = g->cursor_row;
    *col = g->cursor_col;
}

int vt100_grid_app_cursor_f(void **handle) {
    vt100_grid_t *g = (vt100_grid_t *)*handle;
    return g ? g->app_cursor_keys : 0;
}

void vt100_grid_set_pty_fd_f(void **handle, int *fd) {
    vt100_grid_t *g = (vt100_grid_t *)*handle;
    if (g) g->pty_fd = *fd;
}

int vt100_grid_cpr_pending_f(void **handle) {
    vt100_grid_t *g = (vt100_grid_t *)*handle;
    if (!g) return 0;
    int pending = g->cpr_pending;
    g->cpr_pending = 0;  // Clear on read
    return pending;
}

int vt100_grid_get_rows_f(void **handle) {
    vt100_grid_t *g = (vt100_grid_t *)*handle;
    return g ? g->rows : 0;
}

int vt100_grid_get_cols_f(void **handle) {
    vt100_grid_t *g = (vt100_grid_t *)*handle;
    return g ? g->cols : 0;
}
