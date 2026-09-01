# facsimile
(noun) : something clever, trevor

A terminal text editor written in modern Fortran. It keeps VSCode's keys where
a terminal can express them, and says so plainly where it can't.

```bash
fac                 # welcome menu: recent workspaces and files
fac notes.md        # open a file
fac src/parser      # open a directory as a workspace
```

## Why

Fortran is a language people assume you write numerical kernels in and nothing
else. `fac` is the argument that the assumption is about the ecosystem rather
than the language. Everything above the system call is Fortran: the gap buffer,
the ANSI renderer, the JSON parser that talks to language servers, the syntax
highlighter. C appears only where an `iso_c_binding` shim is the only way
through — `termios`, `forkpty`, POSIX regex, the pipes to an LSP process.

The practical goal is narrower: an editor that starts instantly, holds a whole
project without a config file, and doesn't need a plugin to open a second file.

## Install

```bash
# Homebrew
brew install FortranGoingOnForty/tap/facsimile

# Arch (AUR)
yay -S facsimile
```

Or take a tarball for Linux, macOS or Windows from
[Releases](https://github.com/FortranGoingOnForty/facsimile/releases).

### From source

Needs gfortran and a C compiler. The Makefile picks the toolchain per platform
— Homebrew gfortran on Apple silicon, plain gfortran
elsewhere.

```bash
make            # -> ./fac
make dev        # warnings and runtime checks
make debug      # -O0 with backtraces
make info       # what it detected
```

There is also an fpm build (`fpm run -- file.txt`), used by the test suite.
`make` is the release path.

## What you get

**Workspaces.** Open a directory and `fac` remembers it: which files were open,
where each caret was, how the panes were split, whether the tree was showing.
It comes back that way. State lives in `.fac/workspace.json` at the workspace
root — not in a dotfile in your home directory deciding things for every
project at once.

**Tab groups.** A sub-workspace inside a workspace. Press `enter` on a
directory in the file tree, tick the files you want, and they collapse into one
tab-bar entry — `src/ (4)` — with their own row while you are inside it.
Members are read from disk the first time you look at them, so a forty-file
group costs one file read rather than forty.

**Panes.** `alt-v` and `alt-s` split the view; each pane keeps its own viewport
and caret over the same buffer. Drag a tab off the bar into a pane's left,
right or bottom quarter to split the file into place — a band shows the shape
before you let go.

**An integrated terminal** (`f5`) that knows it is inside an editor. Typing
`fac notes.md` at its prompt opens a tab in the session you are already in,
rather than starting a second editor inside the first.

**Language servers**, detected and installed per language, with go-to-
definition, references, project-wide rename, diagnostics, code actions and
document formatting. See [docs/LSP_GUIDE.md](docs/LSP_GUIDE.md).

**A git-aware file tree** (`ctrl-b`) showing staged, modified and untracked
status inline, with stage, unstage, commit, push and pull behind a `ctrl-g`
prefix — single letters stay free for fuzzy search.

**Inline AI completion** from a local model, off until you turn it on with
`alt-i`. Nothing runs, connects or leaves your machine before that. See
[docs/AI_COMPLETION.md](docs/AI_COMPLETION.md).

## Keys

The full list is `F1` inside the editor, or
[docs/KEYBINDINGS.md](docs/KEYBINDINGS.md). Enough to get moving:

| | |
|---|---|
| `ctrl-s` / `ctrl-q` | save / quit |
| `ctrl-o` / `ctrl-b` | file browser / file tree |
| `ctrl-p` | command palette |
| `ctrl-f` | find |
| `ctrl-g` | go to line:column |
| `ctrl-z` / `ctrl-]` | undo / redo |
| `alt-v` / `alt-s` | split vertically / horizontally |
| `ctrl-t` / `ctrl-w` | new tab / close tab |
| `alt-1` … `alt-9` | jump to a tab |
| `f12` / `shift-f12` / `f2` | go to definition / references / rename |
| `f1` | everything else |

### Find

`ctrl-f` on a word searches for that word without your typing it, lights every
occurrence, and draws the one you are on differently from the rest. The bar
stays up while you walk them: arrows, page keys, `enter`, `tab` and `space`
step forward, add `shift` to step back, `home` and `end` are the first and
last. Typing replaces the seeded word and re-searches as you go. A second
`ctrl-f` puts the bar away and leaves the matches lit for `n`/`N`; `esc` ends
the search.

### Multiple cursors

`ctrl-d` takes the next match, `alt-click` adds one anywhere, and
`ctrl-alt-up`/`down` adds one on the line above or below. That last one has two
aliases — `super-up`/`down` and `ctrl-shift-alt-up`/`down` — because GNOME and
KDE take `ctrl+alt+arrows` for workspace switching and `super+arrows` for
window tiling, and never pass them on. The three-modifier chord is the one
neither desktop wants.

### Mouse

Click to place the caret, drag to select, `alt-click` for another cursor,
right-click for a context menu at the pointer. The wheel scrolls whatever is
under it — the active pane, an inactive one, the terminal's scrollback — and
does nothing over the tab bar, rather than moving a document you are not
pointing at.

Tabs drag along the bar to reorder, into a pane to split, or onto a group to
join it. Right-clicking inside a selection keeps the selection, so Cut and Copy
act on it rather than on the line.

Full detail in [docs/KEYBINDINGS.md](docs/KEYBINDINGS.md#mouse).

## Terminals

Some chords never reach a terminal program at all, and it is the terminal or
the desktop eating them, not `fac`:

| Chord | Who takes it | Use instead |
|---|---|---|
| `ctrl-a` | tmux and screen, as their prefix | `home` |
| `ctrl-shift-z` | WezTerm, in multi-pane mode | `ctrl-]` |
| `f2` | GNOME, KDE and most tiling WMs, for "rename" | `alt-n` |
| `f10` | terminal and desktop menubars | `alt-z` |
| `ctrl-'` | most terminals send a plain apostrophe | `alt-'` |

`python3 tools/keycap.py` shows what your terminal actually delivers for a
given key, which settles the question faster than guessing does.

`ctrl-/` and `ctrl-?` are the same byte (0x1F) under the legacy encoding, so
`fac` asks for the kitty keyboard protocol at startup to tell them apart. Where
that is declined — including inside tmux without `set -g extended-keys on` —
`ctrl-/` toggles a comment and `F1` opens help.

To get `ctrl-shift-z` back in WezTerm:

```lua
config.keys = {
  { key = 'Z', mods = 'CTRL|SHIFT', action = wezterm.action.DisableDefaultAssignment },
}
```

## Under it

A gap buffer for the text, ANSI escape sequences for the screen, and small C
wrappers where POSIX has no Fortran interface: raw mode, the pseudoterminal
behind the integrated terminal, POSIX regex, and the pipes to language server
subprocesses. Cursor positions are UTF-8 character indices throughout,
converted to bytes only where a C interface demands it.

## Docs

- [KEYBINDINGS.md](docs/KEYBINDINGS.md) — every key, and why some are odd
- [LSP_GUIDE.md](docs/LSP_GUIDE.md) — language servers
- [AI_COMPLETION.md](docs/AI_COMPLETION.md) — inline completion setup
- [THEMES.md](docs/THEMES.md) — built-in themes, terminal capabilities, and custom theme schema
- [WORKSPACE_QUICKSTART.md](docs/WORKSPACE_QUICKSTART.md) — workspaces and the fortress
- [config_spec.md](docs/config_spec.md) — settings reference

## License

MIT
