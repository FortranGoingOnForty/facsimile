# Themes

Facsimile styles its interface with semantic roles rather than fixed ANSI
color codes. The same theme controls the editor, tab strips, Fuss, Fortress,
status and search bars, LSP panels, completion, prompts, context menus, and
integrated-terminal chrome. Output is downgraded automatically for truecolor,
256-color, basic-color, and monochrome terminals.

## Selecting a theme

Open the command palette and run `Preferences: Color Theme`. Moving through
the list previews each theme immediately; `Enter` saves it and `Esc` restores
the previous theme. `Preferences: Reload Theme` reloads the current custom
theme from disk.

The built-in themes are:

| ID | Description |
|---|---|
| `steel` | Cool gray editor surfaces with a restrained steel-blue selection |
| `graphite` | Neutral charcoal surfaces with teal accents |
| `paper` | Light editor and panel surfaces |
| `legacy` | Minimal colors and the previous reverse-video treatment |

The same choice can be made in `~/.config/fac/settings.json`:

```json
{
  "ui.theme": "steel",
  "ui.color_mode": "auto",
  "ui.icons": "unicode",
  "ui.shadows": true
}
```

`ui.color_mode` accepts `auto`, `truecolor`, `256`, `basic`, or `mono`.
Automatic detection checks `TERM`, `COLORTERM`, and `NO_COLOR`. `ui.icons`
accepts `ascii`, `unicode`, or `nerd`. Nerd mode expects a Nerd Font; Unicode
mode works with ordinary modern terminal fonts. Shadows are suppressed in
monochrome mode even when enabled.

## Custom themes

Place custom themes in `$XDG_CONFIG_HOME/fac/themes`, normally
`~/.config/fac/themes`. The filename without `.toml` is the theme ID shown in
the picker. For example, `night-shift.toml`:

```toml
[metadata]
name = "Night Shift"
extends = "steel"

[palette]
ink = "#171a20"
surface = "#222832"
raised = "#303a48"
text = "#d6dbe3"
muted = "#7f8997"
blue = "#91abc5"
green = "#85c79a"
amber = "#e6b86a"

[styles.editor]
fg = "text"
bg = "ink"

[styles.tab.active]
fg = "ink"
bg = "blue"
attrs = ["inverse"]

[styles.tab.inactive]
fg = "muted"
bg = "ink"

[styles.panel]
fg = "text"
bg = "surface"

[styles.panel.selection]
fg = "text"
bg = "raised"
attrs = ["inverse"]

[styles.syntax.keyword]
fg = "blue"
attrs = ["bold"]

[styles.git.added]
fg = "green"
bg = "surface"

[styles.git.modified]
fg = "amber"
bg = "surface"
```

`extends` must name `steel`, `graphite`, `paper`, or `legacy`. A style may set
`fg`, `bg`, or `attrs`; omitted properties remain inherited. Colors are
`#rrggbb`, `default`, or names from `[palette]`. `fg` and `bg` always describe
the colors visible on screen, including when `inverse` is present. Attributes
are `bold`, `dim`, `italic`, `underline`, `inverse`, and `strikethrough`.

Available roles:

```text
editor                  editor.background       muted
accent                  selection               selection.inactive
tab.bar                 tab.active              tab.inactive
tab.modified            tab.orphan              tab.hover
tab.drag                status                  status.accent
panel                   panel.header            panel.footer
panel.selection         border                  border.focus
shadow                  diagnostic.error        diagnostic.warning
diagnostic.info         diagnostic.hint         success
git.added               git.modified            git.deleted
directory               executable              ghost
line_number             line_number.active      syntax.keyword
syntax.string           syntax.comment          syntax.number
syntax.type             syntax.function         syntax.preprocessor
search.match            current_line            disabled
```

An invalid custom theme is rejected without replacing the active theme. Parse
errors include the file, line, and column in the status message.

## Glyphs and ligatures

Facsimile measures UTF-8 text in display cells and preserves combining marks,
variation selectors, and joined glyph sequences in the screen model. Icons
therefore do not shift click regions or neighboring text. Programming
ligatures are still a terminal-font feature: enable them in the terminal and
choose a compatible font. Facsimile sends the original source characters and
does not substitute ligature glyphs itself.
