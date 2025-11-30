# Compiler Warnings Audit & Resolution Roadmap

**Date:** 2025-11-05
**Compiler:** gfortran with `-Wall -Wextra -pedantic -Wunused-variable -Wuninitialized`
**Total Warnings:** ~120+

---

## Risk Classification

### 🔴 CRITICAL (Requires Careful Surgery)
**These need sophisticated implementation changes - DO NOT bulk fix**

1. **src/clipboard/clipboard_module.f90:40** - 1MB stack buffer
   - **Issue:** `character(len=1000000) :: buffer` allocates 1MB on stack
   - **Risk:** Stack overflow crashes
   - **Solution:** Redesign to use ALLOCATABLE array
   - **Status:** 🔖 BOOKMARKED for manual surgery

---

## 🟡 LOW RISK (Straightforward Fixes)

### Category: Format/Standards Issues (3 warnings)

2. **src/commands/command_handler_module.f90:2352** - Intrinsic shadow
   - **Issue:** `function getpid()` shadows intrinsic
   - **Fix:** Rename to `get_process_id()` or add explicit INTRINSIC declaration
   - **Lines:** 2352

3. **src/workspace/file_tree_module.f90:253** - Character truncation
   - **Issue:** Assignment truncates 1024 chars to 512
   - **Fix:** Increase destination buffer size or truncate explicitly
   - **Lines:** 253

4. **src/workspace/file_tree_module.f90:525** - GNU extension
   - **Issue:** Missing positive width after L descriptor in format
   - **Fix:** Use proper format: `(A,A,A,L1,A,L1)`
   - **Lines:** 525

---

## 🟢 SAFE BULK FIXES

### Category A: Unused Local Variables (~60 warnings)
**These are safe to remove - no function signature changes**

#### src/clipboard/yank_stack_module.f90 (1)
- Line 49: `new_entries` - unused allocatable array

#### src/clipboard/clipboard_module.f90 (1)
- Line 41: `n_read` - unused integer

#### src/workspace/file_tree_renderer_module.f90 (3)
- Line 116: `i` - unused loop variable
- Line 116: `prefix_len` - unused integer
- Line 22: `visible_items` - unused integer

#### src/workspace/file_tree_module.f90 (2)
- Line 611: `prev` - unused pointer
- Line 151: `i` - unused integer

#### src/terminal/renderer_module.f90 (5)
- Line 69: `buffer_pos` - unused integer
- Line 68: `ch` - unused character
- Line 69: `col` - unused integer
- Line 69: `line_start_pos` - unused integer

#### src/ui/help_display_module.f90 (1)
- Line 101: `section_start` - unused integer

#### src/ui/search_prompt_module.f90 (3)
- Line 21: `use_regex` - unused module variable (PRIVATE)
- Line 32: `ios` - unused integer
- Line 31: `options_str` - unused character

#### src/ui/replace_prompt_module.f90 (5)
- Line 133: `should_continue` - unused logical
- Line 22: `found` - unused logical
- Line 23: `found_col` - unused integer
- Line 23: `found_line` - unused integer
- Line 20: `ios` - unused integer

#### src/ui/goto_prompt_module.f90 (4)
- Line 22: `col_str` - unused allocatable string
- Line 20: `colon_pos` - unused integer
- Line 18: `ios` - unused integer
- Line 22: `line_str` - unused allocatable string

#### src/commands/command_handler_module.f90 (~35+ unused local variables)
- Line 3906: `status` - unused integer
- Line 3568: `in_word` - unused logical
- Line 3494: `in_word` - unused logical (duplicate name different function)
- Line 2527: `is_alt_click` - unused logical
- Line 2294: `error_msg` - unused character(1024)
- Line 2295: `has_write_permission` - unused logical
- Line 2292: `temp_unit` - unused integer
- Line 1577: `cursors_before` - unused integer
- *(~27 more throughout the file)*

---

### Category B: Unused Dummy Arguments (~15 warnings)
**Safe to remove, but changes function signatures - check call sites**

#### src/terminal/input_handler_module.f90 (1)
- Line 419: `handle_alt_modified_key()` parameter `first_char` unused

#### src/terminal/renderer_module.f90 (1)
- Line 943: `render_single_pane()` parameter `buffer` unused

#### src/commands/command_handler_module.f90 (~13 warnings)
- Line 3453: `extend_selection_page_up()` parameter `line_count` unused
- Line 3295: `extend_selection_up()` parameter `line_count` unused
- Line 3199: `add_cursor_above()` parameter `buffer` unused
- *(~10 more throughout the file)*

---

### Category C: Dead Code (~2 warnings)
**Safe to remove - unused functions**

#### src/terminal/renderer_module.f90 (1)
- Line 200: `render_line()` - defined but never called

#### src/ui/help_display_module.f90 (1)
- Line 261: `display_section()` - defined but never called

---

## Resolution Strategy

### Phase 1: Safe Bulk Fixes (Category A)
- Remove unused local variables
- No signature changes, minimal risk
- Can be done file-by-file systematically
- **Estimated:** 60 simple deletions

### Phase 2: Function Signature Fixes (Category B)
- Remove unused dummy arguments
- Must verify call sites (use compiler to check)
- Compiler will catch any mistakes
- **Estimated:** 15 parameter removals + call site updates

### Phase 3: Dead Code Removal (Category C)
- Delete unused functions
- Verify no indirect calls (callbacks, etc.)
- **Estimated:** 2 function deletions

### Phase 4: Low-Risk Fixes (🟡)
- Fix intrinsic shadow (rename)
- Fix character truncation (increase buffer)
- Fix format descriptor (add width)
- **Estimated:** 3 targeted fixes

### Phase 5: BOOKMARKED for Surgery (🔴)
- Redesign clipboard buffer to use ALLOCATABLE
- Requires careful testing of clipboard operations
- **Status:** Deferred for manual implementation

---

## Verification Plan

After each phase:
```bash
make clean
FC=gfortran make dev 2>&1 | tee /tmp/gfortran_warnings.log
grep -i warning /tmp/gfortran_warnings.log | wc -l
```

Final target: **0 warnings** (except bookmarked items)

---

## Notes

- All fixes preserve functionality
- Compiler errors will catch any mistakes in signature changes
- No automated sed/awk scripts - manual edits only
- Bookmark items require design discussion before implementation
