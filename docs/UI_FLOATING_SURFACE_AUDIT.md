# Floating Surface Audit

This inventory separates true overlays from docked panes and full-screen
workflows. Floating surfaces should use an opaque semantic body, bounded text,
the shared border language, and a clipped two-column/right plus one-row/bottom
shadow when `ui.shadows` is enabled.

## Active floating surfaces

| Surface | Renderer | Status |
| --- | --- | --- |
| Help | `help_display_module` | Shared frame, opaque body, complete shadow, synchronized redraw |
| Fuss command hints | `help_display_module` | Floating shared frame and synchronized redraw |
| Fortress navigator | `fortress_navigator_module` | Shared frame with screen-clipped shadow |
| Tab-group editor | `group_picker_module` | Opaque custom rows with shared clipped shadow |
| Command palette | `command_palette_module` | Opaque rows, Unicode-safe clipping, shared shadow, synchronized redraw |
| Theme picker | `theme_picker_module` | Restores the editor before opening; opaque rows, shared shadow, synchronized preview |
| Context menu | `context_menu_module` | Opaque rows with screen-clipped shared shadow |
| Completion popup | `completion_popup_module` | Opaque rows; reserves and clips shared shadow at screen edges |
| Hover tooltip renderer | `hover_tooltip_module` | Opaque rows; reserves and clips shared shadow at screen edges |
| LSP server manager and confirmation | `lsp_server_installer_panel_module` | Opaque rows with shared shadow |

## Docked surfaces

References, diagnostics, document symbols, workspace symbols, code actions,
unified search, Fuss, and the integrated terminal are layout panes. They should
use full-height or full-width separators and distinct surfaces, not drop
shadows. The References pane has already received its header/body spacing and
selection cleanup.

## Functional debt found during the audit

- Hover requests currently do not install `handle_hover_response` as their LSP
  callback. The renderer is standardized, but the response path must be wired
  and integration-tested before hover can reliably appear.
- Signature help marks its tooltip visible, but its only renderer is commented
  out. This needs a real renderer and screen-aware geometry before it should be
  treated as an active floating surface.
- `show_tags_modal` is an unused full-screen tag pager. The active tag creation
  workflow uses `display_tags_header` plus a prompt and is intentionally not a
  floating modal. These should be redesigned together rather than restyling the
  unused pager in isolation.
- The Fortress welcome menu is a full-screen first-run workflow, not an overlay.

## Surface rules

New floating UI should use `modal_box_module` instead of private shadow loops.
Callers with custom chrome can combine `box_shadow`, `box_fill`, `box_row`, and
`box_rule`; conventional dialogs should use `box_frame`. Always pass terminal
bounds when the renderer has them, and wrap interactive redraws in synchronized
terminal output.
