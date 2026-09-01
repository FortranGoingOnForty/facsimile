module fgof_screen
  use fgof_screen_types, only : &
    SCREEN_COLOR_MONO, &
    SCREEN_COLOR_BASIC, &
    SCREEN_COLOR_256, &
    SCREEN_COLOR_TRUECOLOR, &
    screen_buffer, &
    screen_cell, &
    screen_damage, &
    screen_damage_run, &
    screen_diff, &
    screen_render_options, &
    screen_size, &
    screen_style
  implicit none
  private

  public :: &
    allocate_screen, &
    clear_screen, &
    clear_screen_buffer, &
    clear_screen_cell, &
    clear_screen_damage, &
    clear_screen_diff, &
    clear_screen_size, &
    clear_screen_style, &
    diff_screen, &
    fill_screen, &
    put_cell, &
    put_glyph, &
    put_text, &
    render_cursor_ansi, &
    render_screen_ansi, &
    render_screen_diff_ansi, &
    resize_screen, &
    set_cursor, &
    screen_buffer, &
    screen_cell, &
    screen_damage_run, &
    screen_damage, &
    screen_diff, &
    screen_render_options, &
    screen_size, &
    screen_style, &
    SCREEN_COLOR_MONO, &
    SCREEN_COLOR_BASIC, &
    SCREEN_COLOR_256, &
    SCREEN_COLOR_TRUECOLOR

contains

  function clear_screen_style() result(style)
    type(screen_style) :: style

    style%fg = -1
    style%bg = -1
    style%fg_truecolor = .false.
    style%bg_truecolor = .false.
    style%fg_rgb = [0, 0, 0]
    style%bg_rgb = [0, 0, 0]
    style%bold = .false.
    style%dim = .false.
    style%italic = .false.
    style%underline = .false.
    style%inverse = .false.
    style%strikethrough = .false.
  end function clear_screen_style

  function clear_screen_cell() result(cell)
    type(screen_cell) :: cell

    cell%glyph = " "
    cell%style = clear_screen_style()
    cell%width = 1
    cell%continuation = .false.
  end function clear_screen_cell

  function clear_screen_size() result(size_value)
    type(screen_size) :: size_value

    size_value%width = 0
    size_value%height = 0
  end function clear_screen_size

  function clear_screen_damage() result(damage)
    type(screen_damage) :: damage

    damage%active = .false.
    damage%row_first = 0
    damage%row_last = 0
    damage%col_first = 0
    damage%col_last = 0
    damage%changed_cells = 0
    damage%run_count = 0
  end function clear_screen_damage

  function clear_screen_diff() result(diff)
    type(screen_diff) :: diff

    diff%changed = .false.
    diff%size_changed = .false.
    diff%cursor_changed = .false.
    diff%cursor_visibility_changed = .false.
    diff%previous_size = clear_screen_size()
    diff%current_size = clear_screen_size()
    diff%damage = clear_screen_damage()
  end function clear_screen_diff

  function clear_screen_buffer() result(buffer)
    type(screen_buffer) :: buffer

    buffer%size = clear_screen_size()
    buffer%cursor_row = 1
    buffer%cursor_col = 1
    buffer%cursor_visible = .true.
  end function clear_screen_buffer

  function allocate_screen(width, height) result(buffer)
    integer, intent(in) :: width
    integer, intent(in) :: height
    type(screen_buffer) :: buffer

    buffer = clear_screen_buffer()
    if (width <= 0 .or. height <= 0) return

    buffer%size%width = width
    buffer%size%height = height
    allocate(buffer%cells(height, width))
    call clear_screen(buffer)
    call clamp_cursor(buffer)
  end function allocate_screen

  subroutine resize_screen(buffer, width, height)
    type(screen_buffer), intent(inout) :: buffer
    integer, intent(in) :: width
    integer, intent(in) :: height
    type(screen_cell), allocatable :: grown(:, :)
    integer :: copy_height
    integer :: copy_width

    if (width <= 0 .or. height <= 0) then
      if (allocated(buffer%cells)) deallocate(buffer%cells)
      buffer%size = clear_screen_size()
      call clamp_cursor(buffer)
      return
    end if

    allocate(grown(height, width))
    grown = clear_screen_cell()

    if (allocated(buffer%cells)) then
      copy_height = min(size(buffer%cells, 1), height)
      copy_width = min(size(buffer%cells, 2), width)
      if (copy_height > 0 .and. copy_width > 0) then
        grown(:copy_height, :copy_width) = buffer%cells(:copy_height, :copy_width)
      end if
      deallocate(buffer%cells)
    end if

    call move_alloc(grown, buffer%cells)
    buffer%size%width = width
    buffer%size%height = height
    call clamp_cursor(buffer)
  end subroutine resize_screen

  subroutine fill_screen(buffer, glyph, style)
    type(screen_buffer), intent(inout) :: buffer
    character(len=*), intent(in) :: glyph
    type(screen_style), intent(in), optional :: style
    type(screen_cell) :: fill_cell
    integer :: row
    integer :: col

    if (.not. allocated(buffer%cells)) return

    fill_cell = clear_screen_cell()
    fill_cell%glyph = glyph_for_cell(glyph)
    if (present(style)) fill_cell%style = style

    do row = 1, size(buffer%cells, 1)
      do col = 1, size(buffer%cells, 2)
        buffer%cells(row, col) = fill_cell
      end do
    end do
  end subroutine fill_screen

  subroutine clear_screen(buffer, style)
    type(screen_buffer), intent(inout) :: buffer
    type(screen_style), intent(in), optional :: style

    call fill_screen(buffer, " ", style)
  end subroutine clear_screen

  subroutine put_cell(buffer, row, col, cell)
    type(screen_buffer), intent(inout) :: buffer
    integer, intent(in) :: row
    integer, intent(in) :: col
    type(screen_cell), intent(in) :: cell

    if (.not. screen_index_in_bounds(buffer, row, col)) return
    buffer%cells(row, col) = cell
  end subroutine put_cell

  subroutine put_glyph(buffer, row, col, glyph, style)
    type(screen_buffer), intent(inout) :: buffer
    integer, intent(in) :: row
    integer, intent(in) :: col
    character(len=*), intent(in) :: glyph
    type(screen_style), intent(in), optional :: style
    type(screen_cell) :: cell
    type(screen_cell) :: trailing
    integer :: cells

    if (.not. screen_index_in_bounds(buffer, row, col)) return

    call clear_glyph_at(buffer, row, col)
    cell = clear_screen_cell()
    cell%glyph = glyph_for_cell(glyph)
    if (present(style)) cell%style = style
    cells = glyph_cell_width(cell%glyph)
    if (cells < 1) cells = 1
    if (cells == 2 .and. col == size(buffer%cells, 2)) then
      cell%glyph = "?"
      cells = 1
    end if
    cell%width = cells
    call put_cell(buffer, row, col, cell)

    if (cells == 2) then
      trailing = clear_screen_cell()
      trailing%style = cell%style
      trailing%width = 0
      trailing%continuation = .true.
      call put_cell(buffer, row, col + 1, trailing)
    end if
  end subroutine put_glyph

  subroutine put_text(buffer, row, col, text, style, cells_written)
    type(screen_buffer), intent(inout) :: buffer
    integer, intent(in) :: row
    integer, intent(in) :: col
    character(len=*), intent(in) :: text
    type(screen_style), intent(in), optional :: style
    integer, intent(out), optional :: cells_written
    character(len=:), allocatable :: cluster
    integer :: byte_pos
    integer :: screen_col
    integer :: width

    byte_pos = 1
    screen_col = col
    do while (byte_pos <= len(text))
      cluster = next_grapheme(text, byte_pos)
      if (len(cluster) == 0) exit
      width = glyph_cell_width(cluster)
      if (width <= 0) cycle
      if (screen_col > buffer%size%width) exit
      if (present(style)) then
        call put_glyph(buffer, row, screen_col, cluster, style)
      else
        call put_glyph(buffer, row, screen_col, cluster)
      end if
      screen_col = screen_col + width
    end do
    if (present(cells_written)) cells_written = max(0, screen_col - col)
  end subroutine put_text

  subroutine set_cursor(buffer, row, col)
    type(screen_buffer), intent(inout) :: buffer
    integer, intent(in) :: row
    integer, intent(in) :: col

    buffer%cursor_row = row
    buffer%cursor_col = col
    call clamp_cursor(buffer)
  end subroutine set_cursor

  function diff_screen(previous, current) result(diff)
    type(screen_buffer), intent(in) :: previous
    type(screen_buffer), intent(in) :: current
    type(screen_diff) :: diff
    integer :: max_rows
    integer :: max_cols
    integer :: row
    integer :: col

    diff = clear_screen_diff()
    diff%previous_size = previous%size
    diff%current_size = current%size
    diff%size_changed = .not. screen_sizes_equal(previous%size, current%size)
    diff%cursor_changed = previous%cursor_row /= current%cursor_row .or. &
                          previous%cursor_col /= current%cursor_col
    diff%cursor_visibility_changed = previous%cursor_visible .neqv. current%cursor_visible

    max_rows = max(previous%size%height, current%size%height)
    max_cols = max(previous%size%width, current%size%width)

    do row = 1, max_rows
      do col = 1, max_cols
        if (screen_cell_changed(previous, current, row, col)) then
          call record_damage(diff%damage, row, col)
        end if
      end do
    end do

    diff%changed = diff%damage%active .or. diff%size_changed .or. &
                   diff%cursor_changed .or. diff%cursor_visibility_changed
  end function diff_screen

  function render_screen_ansi(buffer, options) result(output)
    type(screen_buffer), intent(in) :: buffer
    type(screen_render_options), intent(in), optional :: options
    character(len=:), allocatable :: output
    integer :: row
    type(screen_render_options) :: opts

    opts = default_render_options(options)

    output = hide_cursor_ansi() // clear_screen_ansi()

    if (allocated(buffer%cells)) then
      do row = 1, size(buffer%cells, 1)
        output = output // move_cursor_ansi(row, 1)
        output = output // render_current_row_ansi(buffer, row, 1, size(buffer%cells, 2), opts)
      end do
    end if

    output = output // reset_style_ansi() // render_cursor_ansi(buffer)
  end function render_screen_ansi

  function render_screen_diff_ansi(previous, current, options) result(output)
    type(screen_buffer), intent(in) :: previous
    type(screen_buffer), intent(in) :: current
    type(screen_render_options), intent(in), optional :: options
    character(len=:), allocatable :: output
    type(screen_diff) :: diff
    integer :: i
    type(screen_render_options) :: opts

    opts = default_render_options(options)

    diff = diff_screen(previous, current)
    if (.not. diff%changed) then
      output = ""
      return
    end if

    output = ""
    if (diff%damage%active) then
      output = hide_cursor_ansi()
      do i = 1, diff%damage%run_count
        output = output // move_cursor_ansi(diff%damage%runs(i)%row, &
                                            diff%damage%runs(i)%col_first)
        output = output // render_diff_row_ansi(previous, current, &
          diff%damage%runs(i)%row, diff%damage%runs(i)%col_first, &
          diff%damage%runs(i)%col_last, opts)
      end do
      output = output // reset_style_ansi()
    end if

    output = output // render_cursor_ansi(current)
  end function render_screen_diff_ansi

  function render_cursor_ansi(buffer) result(output)
    type(screen_buffer), intent(in) :: buffer
    character(len=:), allocatable :: output

    if (buffer%cursor_visible) then
      output = move_cursor_ansi(buffer%cursor_row, buffer%cursor_col) // show_cursor_ansi()
    else
      output = hide_cursor_ansi()
    end if
  end function render_cursor_ansi

  logical function screen_index_in_bounds(buffer, row, col) result(in_bounds)
    type(screen_buffer), intent(in) :: buffer
    integer, intent(in) :: row
    integer, intent(in) :: col

    in_bounds = allocated(buffer%cells)
    if (.not. in_bounds) return

    in_bounds = row >= 1 .and. row <= size(buffer%cells, 1) .and. &
                col >= 1 .and. col <= size(buffer%cells, 2)
  end function screen_index_in_bounds

  subroutine clamp_cursor(buffer)
    type(screen_buffer), intent(inout) :: buffer

    if (.not. allocated(buffer%cells)) then
      buffer%cursor_row = 1
      buffer%cursor_col = 1
      return
    end if

    buffer%cursor_row = max(1, min(buffer%cursor_row, size(buffer%cells, 1)))
    buffer%cursor_col = max(1, min(buffer%cursor_col, size(buffer%cells, 2)))
  end subroutine clamp_cursor

  logical function screen_cell_changed(previous, current, row, col) result(changed)
    type(screen_buffer), intent(in) :: previous
    type(screen_buffer), intent(in) :: current
    integer, intent(in) :: row
    integer, intent(in) :: col

    changed = screen_index_in_bounds(previous, row, col) .neqv. &
              screen_index_in_bounds(current, row, col)
    if (changed) return

    changed = .not. screen_cells_equal(screen_cell_at(previous, row, col), &
                                       screen_cell_at(current, row, col))
  end function screen_cell_changed

  function screen_cell_at(buffer, row, col) result(cell)
    type(screen_buffer), intent(in) :: buffer
    integer, intent(in) :: row
    integer, intent(in) :: col
    type(screen_cell) :: cell

    cell = clear_screen_cell()
    if (.not. screen_index_in_bounds(buffer, row, col)) return
    cell = buffer%cells(row, col)
  end function screen_cell_at

  logical function screen_cells_equal(left, right) result(equal)
    type(screen_cell), intent(in) :: left
    type(screen_cell), intent(in) :: right

    equal = glyph_text(left) == glyph_text(right) .and. &
            left%width == right%width .and. &
            (left%continuation .eqv. right%continuation) .and. &
            screen_styles_equal(left%style, right%style)
  end function screen_cells_equal

  logical function screen_styles_equal(left, right) result(equal)
    type(screen_style), intent(in) :: left
    type(screen_style), intent(in) :: right

    equal = left%fg == right%fg
    equal = equal .and. left%bg == right%bg
    equal = equal .and. (left%fg_truecolor .eqv. right%fg_truecolor)
    equal = equal .and. (left%bg_truecolor .eqv. right%bg_truecolor)
    equal = equal .and. all(left%fg_rgb == right%fg_rgb)
    equal = equal .and. all(left%bg_rgb == right%bg_rgb)
    equal = equal .and. (left%bold .eqv. right%bold)
    equal = equal .and. (left%dim .eqv. right%dim)
    equal = equal .and. (left%italic .eqv. right%italic)
    equal = equal .and. (left%underline .eqv. right%underline)
    equal = equal .and. (left%inverse .eqv. right%inverse)
    equal = equal .and. (left%strikethrough .eqv. right%strikethrough)
  end function screen_styles_equal

  logical function screen_sizes_equal(left, right) result(equal)
    type(screen_size), intent(in) :: left
    type(screen_size), intent(in) :: right

    equal = left%width == right%width .and. left%height == right%height
  end function screen_sizes_equal

  subroutine record_damage(damage, row, col)
    type(screen_damage), intent(inout) :: damage
    integer, intent(in) :: row
    integer, intent(in) :: col
    type(screen_damage_run), allocatable :: grown(:)

    if (.not. damage%active) then
      damage%active = .true.
      damage%row_first = row
      damage%row_last = row
      damage%col_first = col
      damage%col_last = col
    else
      damage%row_first = min(damage%row_first, row)
      damage%row_last = max(damage%row_last, row)
      damage%col_first = min(damage%col_first, col)
      damage%col_last = max(damage%col_last, col)
    end if

    damage%changed_cells = damage%changed_cells + 1

    if (damage%run_count > 0) then
      if (damage%runs(damage%run_count)%row == row .and. &
          damage%runs(damage%run_count)%col_last + 1 == col) then
        damage%runs(damage%run_count)%col_last = col
        return
      end if
    end if

    allocate(grown(damage%run_count + 1))
    if (damage%run_count > 0) grown(:damage%run_count) = damage%runs
    grown(damage%run_count + 1)%row = row
    grown(damage%run_count + 1)%col_first = col
    grown(damage%run_count + 1)%col_last = col
    call move_alloc(grown, damage%runs)
    damage%run_count = damage%run_count + 1
  end subroutine record_damage

  function render_current_row_ansi(buffer, row, col_first, col_last, options) result(output)
    type(screen_buffer), intent(in) :: buffer
    integer, intent(in) :: row
    integer, intent(in) :: col_first
    integer, intent(in) :: col_last
    type(screen_render_options), intent(in) :: options
    character(len=:), allocatable :: output
    type(screen_cell) :: cell
    character(len=:), allocatable :: current_key
    character(len=:), allocatable :: cell_key
    integer :: col

    output = ""
    current_key = ""

    do col = col_first, col_last
      cell = buffer%cells(row, col)
      cell_key = style_key(cell%style)
      if (cell_key /= current_key) then
        if (len(cell_key) > 0) then
          output = output // style_ansi(cell%style, options)
        else if (len(current_key) > 0) then
          output = output // reset_style_ansi()
        end if
        current_key = cell_key
      end if
      output = output // renderable_glyph(cell, options)
    end do

    if (len(current_key) > 0) then
      output = output // reset_style_ansi()
    end if
  end function render_current_row_ansi

  function render_diff_row_ansi(previous, current, row, col_first, col_last, options) result(output)
    type(screen_buffer), intent(in) :: previous
    type(screen_buffer), intent(in) :: current
    integer, intent(in) :: row
    integer, intent(in) :: col_first
    integer, intent(in) :: col_last
    type(screen_render_options), intent(in) :: options
    character(len=:), allocatable :: output
    type(screen_cell) :: cell
    character(len=:), allocatable :: current_key
    character(len=:), allocatable :: cell_key
    integer :: col

    output = ""
    current_key = ""

    do col = col_first, col_last
      cell = screen_cell_for_diff(previous, current, row, col)
      cell_key = style_key(cell%style)
      if (cell_key /= current_key) then
        if (len(cell_key) > 0) then
          output = output // style_ansi(cell%style, options)
        else if (len(current_key) > 0) then
          output = output // reset_style_ansi()
        end if
        current_key = cell_key
      end if
      output = output // renderable_glyph(cell, options)
    end do

    if (len(current_key) > 0) then
      output = output // reset_style_ansi()
    end if
  end function render_diff_row_ansi

  function screen_cell_for_diff(previous, current, row, col) result(cell)
    type(screen_buffer), intent(in) :: previous
    type(screen_buffer), intent(in) :: current
    integer, intent(in) :: row
    integer, intent(in) :: col
    type(screen_cell) :: cell

    cell = clear_screen_cell()
    if (screen_index_in_bounds(current, row, col)) then
      cell = current%cells(row, col)
      return
    end if

    if (screen_index_in_bounds(previous, row, col)) then
      cell = clear_screen_cell()
    end if
  end function screen_cell_for_diff

  logical function style_is_default(style) result(is_default)
    type(screen_style), intent(in) :: style

    is_default = style%fg < 0 .and. style%bg < 0 .and. &
                 (.not. style%fg_truecolor) .and. (.not. style%bg_truecolor) .and. &
                 (.not. style%bold) .and. (.not. style%dim) .and. &
                 (.not. style%italic) .and. (.not. style%underline) .and. &
                 (.not. style%inverse) .and. (.not. style%strikethrough)
  end function style_is_default

  function style_key(style) result(key)
    type(screen_style), intent(in) :: style
    character(len=:), allocatable :: key

    if (style_is_default(style)) then
      key = ""
      return
    end if

    key = integer_text(style%fg) // ":" // integer_text(style%bg) // ":" // &
          merge("1", "0", style%fg_truecolor) // ":" // merge("1", "0", style%bg_truecolor) // ":" // &
          rgb_key(style%fg_rgb) // ":" // rgb_key(style%bg_rgb) // ":" // &
          merge("1", "0", style%bold) // ":" // merge("1", "0", style%dim) // ":" // &
          merge("1", "0", style%italic) // ":" // merge("1", "0", style%underline) // ":" // &
          merge("1", "0", style%inverse) // ":" // merge("1", "0", style%strikethrough)
  end function style_key

  function rgb_key(rgb) result(key)
    integer, intent(in) :: rgb(3)
    character(len=:), allocatable :: key

    key = integer_text(rgb(1)) // "," // integer_text(rgb(2)) // "," // integer_text(rgb(3))
  end function rgb_key

  function style_ansi(style, options) result(output)
    type(screen_style), intent(in) :: style
    type(screen_render_options), intent(in) :: options
    character(len=:), allocatable :: output
    integer :: fg_index
    integer :: bg_index

    output = reset_style_ansi()
    if (style%bold) output = output // sgr_parameter("1")
    if (style%dim) output = output // sgr_parameter("2")
    if (style%italic) output = output // sgr_parameter("3")
    if (style%underline) output = output // sgr_parameter("4")
    if (style%inverse) output = output // sgr_parameter("7")
    if (style%strikethrough) output = output // sgr_parameter("9")
    if (options%color_mode == SCREEN_COLOR_MONO) return

    if (style%fg_truecolor .and. options%color_mode >= SCREEN_COLOR_TRUECOLOR) then
      output = output // sgr_parameter("38;2;" // rgb_ansi(style%fg_rgb))
    else if (style%fg_truecolor .or. style%fg >= 0) then
      fg_index = style%fg
      if (style%fg_truecolor) fg_index = rgb_to_xterm(style%fg_rgb)
      output = output // indexed_color_ansi(fg_index, .false., options%color_mode)
    end if
    if (style%bg_truecolor .and. options%color_mode >= SCREEN_COLOR_TRUECOLOR) then
      output = output // sgr_parameter("48;2;" // rgb_ansi(style%bg_rgb))
    else if (style%bg_truecolor .or. style%bg >= 0) then
      bg_index = style%bg
      if (style%bg_truecolor) bg_index = rgb_to_xterm(style%bg_rgb)
      output = output // indexed_color_ansi(bg_index, .true., options%color_mode)
    end if
  end function style_ansi

  function indexed_color_ansi(index, background, color_mode) result(output)
    integer, intent(in) :: index
    logical, intent(in) :: background
    integer, intent(in) :: color_mode
    character(len=:), allocatable :: output
    integer :: basic

    if (color_mode >= SCREEN_COLOR_256) then
      if (background) then
        output = sgr_parameter("48;5;" // integer_text(max(0, min(255, index))))
      else
        output = sgr_parameter("38;5;" // integer_text(max(0, min(255, index))))
      end if
      return
    end if

    basic = nearest_basic_index(index)
    if (background) then
      if (basic < 8) then
        output = sgr_parameter(integer_text(40 + basic))
      else
        output = sgr_parameter(integer_text(100 + basic - 8))
      end if
    else
      if (basic < 8) then
        output = sgr_parameter(integer_text(30 + basic))
      else
        output = sgr_parameter(integer_text(90 + basic - 8))
      end if
    end if
  end function indexed_color_ansi

  integer function rgb_to_xterm(rgb) result(index)
    integer, intent(in) :: rgb(3)
    integer :: r
    integer :: g
    integer :: b

    r = nint(real(color_component(rgb(1))) / 255.0 * 5.0)
    g = nint(real(color_component(rgb(2))) / 255.0 * 5.0)
    b = nint(real(color_component(rgb(3))) / 255.0 * 5.0)
    index = 16 + 36 * r + 6 * g + b
  end function rgb_to_xterm

  integer function nearest_basic_index(index) result(basic)
    integer, intent(in) :: index

    if (index < 0) then
      basic = 7
    else if (index < 16) then
      basic = index
    else
      ! The cube's brightest half maps to the bright ANSI bank.
      basic = modulo(index, 8)
      if (index >= 244) basic = basic + 8
    end if
  end function nearest_basic_index

  function rgb_ansi(rgb) result(text)
    integer, intent(in) :: rgb(3)
    character(len=:), allocatable :: text

    text = integer_text(color_component(rgb(1))) // ";" // &
           integer_text(color_component(rgb(2))) // ";" // &
           integer_text(color_component(rgb(3)))
  end function rgb_ansi

  integer function color_component(value) result(component)
    integer, intent(in) :: value

    component = max(0, min(255, value))
  end function color_component

  function sgr_parameter(parameter) result(output)
    character(len=*), intent(in) :: parameter
    character(len=:), allocatable :: output

    output = csi() // parameter // "m"
  end function sgr_parameter

  function reset_style_ansi() result(output)
    character(len=:), allocatable :: output

    output = csi() // "0m"
  end function reset_style_ansi

  function clear_screen_ansi() result(output)
    character(len=:), allocatable :: output

    output = csi() // "2J" // csi() // "H"
  end function clear_screen_ansi

  function move_cursor_ansi(row, col) result(output)
    integer, intent(in) :: row
    integer, intent(in) :: col
    character(len=:), allocatable :: output

    output = csi() // integer_text(max(1, row)) // ";" // integer_text(max(1, col)) // "H"
  end function move_cursor_ansi

  function hide_cursor_ansi() result(output)
    character(len=:), allocatable :: output

    output = csi() // "?25l"
  end function hide_cursor_ansi

  function show_cursor_ansi() result(output)
    character(len=:), allocatable :: output

    output = csi() // "?25h"
  end function show_cursor_ansi

  function csi() result(output)
    character(len=:), allocatable :: output

    output = achar(27) // "["
  end function csi

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, "(i0)") value
    text = trim(scratch)
  end function integer_text

  function glyph_text(cell) result(text)
    type(screen_cell), intent(in) :: cell
    character(len=:), allocatable :: text

    if (allocated(cell%glyph)) then
      text = cell%glyph
    else
      text = " "
    end if
  end function glyph_text

  function glyph_for_cell(glyph) result(output)
    character(len=*), intent(in) :: glyph
    character(len=:), allocatable :: output
    integer :: code
    integer :: byte_pos

    if (len(glyph) == 0) then
      output = " "
      return
    end if

    code = iachar(glyph(1:1))
    if (code < 32 .or. code == 127) then
      output = "?"
    else
      byte_pos = 1
      output = next_grapheme(glyph, byte_pos)
      if (len(output) == 0) output = "?"
    end if
  end function glyph_for_cell

  integer function utf8_sequence_length(first_byte) result(byte_count)
    integer, intent(in) :: first_byte

    if (first_byte >= 192 .and. first_byte <= 223) then
      byte_count = 2
    else if (first_byte >= 224 .and. first_byte <= 239) then
      byte_count = 3
    else if (first_byte >= 240 .and. first_byte <= 247) then
      byte_count = 4
    else
      byte_count = 0
    end if
  end function utf8_sequence_length

  function renderable_glyph(cell, options) result(output)
    type(screen_cell), intent(in) :: cell
    type(screen_render_options), intent(in) :: options
    character(len=:), allocatable :: output
    integer :: code
    character(len=:), allocatable :: glyph

    if (cell%continuation) then
      output = ""
      return
    end if

    glyph = glyph_text(cell)
    code = iachar(glyph(1:1))
    if (code < 32 .or. code == 127) then
      output = "?"
    else if (.not. options%unicode .and. code >= 128) then
      output = repeat("?", max(1, cell%width))
    else
      output = glyph_for_cell(glyph)
    end if
  end function renderable_glyph

  function default_render_options(options) result(resolved)
    type(screen_render_options), intent(in), optional :: options
    type(screen_render_options) :: resolved

    resolved%color_mode = SCREEN_COLOR_TRUECOLOR
    resolved%unicode = .true.
    if (present(options)) resolved = options
    resolved%color_mode = max(SCREEN_COLOR_MONO, &
      min(SCREEN_COLOR_TRUECOLOR, resolved%color_mode))
  end function default_render_options

  subroutine clear_glyph_at(buffer, row, col)
    type(screen_buffer), intent(inout) :: buffer
    integer, intent(in) :: row
    integer, intent(in) :: col
    type(screen_cell) :: blank

    if (.not. screen_index_in_bounds(buffer, row, col)) return
    blank = clear_screen_cell()

    if (buffer%cells(row, col)%continuation .and. col > 1) then
      buffer%cells(row, col - 1) = blank
    end if
    if (buffer%cells(row, col)%width == 2 .and. col < size(buffer%cells, 2)) then
      buffer%cells(row, col + 1) = blank
    end if
    if (col > 1) then
      if (buffer%cells(row, col - 1)%width == 2) buffer%cells(row, col - 1) = blank
    end if
    buffer%cells(row, col) = blank
  end subroutine clear_glyph_at

  function next_grapheme(text, byte_pos) result(cluster)
    character(len=*), intent(in) :: text
    integer, intent(inout) :: byte_pos
    character(len=:), allocatable :: cluster
    integer :: start
    integer :: n
    integer :: cp
    integer :: next_cp
    logical :: join_next

    cluster = ""
    if (byte_pos < 1 .or. byte_pos > len(text)) return

    start = byte_pos
    call decode_utf8_at(text, byte_pos, cp, n)
    if (n <= 0) then
      byte_pos = byte_pos + 1
      cluster = "?"
      return
    end if
    byte_pos = byte_pos + n
    join_next = .false.

    do while (byte_pos <= len(text))
      call decode_utf8_at(text, byte_pos, next_cp, n)
      if (n <= 0) exit
      if (is_combining(next_cp) .or. is_variation_selector(next_cp) .or. &
          next_cp == int(z'200D') .or. join_next) then
        join_next = next_cp == int(z'200D')
        byte_pos = byte_pos + n
      else
        exit
      end if
    end do
    cluster = text(start:byte_pos - 1)
  end function next_grapheme

  subroutine decode_utf8_at(text, pos, codepoint, byte_count)
    character(len=*), intent(in) :: text
    integer, intent(in) :: pos
    integer, intent(out) :: codepoint
    integer, intent(out) :: byte_count
    integer :: b1
    integer :: b2
    integer :: b3
    integer :: b4

    codepoint = -1
    byte_count = 0
    if (pos < 1 .or. pos > len(text)) return
    b1 = iachar(text(pos:pos))
    if (b1 < 128) then
      codepoint = b1
      byte_count = 1
    else if (b1 >= 194 .and. b1 <= 223 .and. pos + 1 <= len(text)) then
      b2 = iachar(text(pos + 1:pos + 1))
      if (.not. continuation_byte(b2)) return
      codepoint = 64 * (b1 - 192) + b2 - 128
      byte_count = 2
    else if (b1 >= 224 .and. b1 <= 239 .and. pos + 2 <= len(text)) then
      b2 = iachar(text(pos + 1:pos + 1))
      b3 = iachar(text(pos + 2:pos + 2))
      if (.not. continuation_byte(b2) .or. .not. continuation_byte(b3)) return
      codepoint = 4096 * (b1 - 224) + 64 * (b2 - 128) + b3 - 128
      byte_count = 3
    else if (b1 >= 240 .and. b1 <= 244 .and. pos + 3 <= len(text)) then
      b2 = iachar(text(pos + 1:pos + 1))
      b3 = iachar(text(pos + 2:pos + 2))
      b4 = iachar(text(pos + 3:pos + 3))
      if (.not. continuation_byte(b2) .or. .not. continuation_byte(b3) .or. &
          .not. continuation_byte(b4)) return
      codepoint = 262144 * (b1 - 240) + 4096 * (b2 - 128) + &
                  64 * (b3 - 128) + b4 - 128
      byte_count = 4
    end if
  end subroutine decode_utf8_at

  logical function continuation_byte(value) result(valid)
    integer, intent(in) :: value
    valid = value >= 128 .and. value <= 191
  end function continuation_byte

  integer function glyph_cell_width(glyph) result(width)
    character(len=*), intent(in) :: glyph
    integer :: cp
    integer :: n

    if (len(glyph) == 0) then
      width = 0
      return
    end if
    call decode_utf8_at(glyph, 1, cp, n)
    if (n <= 0 .or. cp < 32 .or. cp == 127) then
      width = 1
    else if (is_combining(cp) .or. is_variation_selector(cp)) then
      width = 0
    else if (is_wide(cp)) then
      width = 2
    else
      width = 1
    end if
  end function glyph_cell_width

  logical function is_combining(cp) result(combining)
    integer, intent(in) :: cp
    combining = (cp >= int(z'0300') .and. cp <= int(z'036F')) .or. &
      (cp >= int(z'1AB0') .and. cp <= int(z'1AFF')) .or. &
      (cp >= int(z'1DC0') .and. cp <= int(z'1DFF')) .or. &
      (cp >= int(z'20D0') .and. cp <= int(z'20FF')) .or. &
      (cp >= int(z'FE20') .and. cp <= int(z'FE2F'))
  end function is_combining

  logical function is_variation_selector(cp) result(selector)
    integer, intent(in) :: cp
    selector = (cp >= int(z'FE00') .and. cp <= int(z'FE0F')) .or. &
      (cp >= int(z'E0100') .and. cp <= int(z'E01EF'))
  end function is_variation_selector

  logical function is_wide(cp) result(wide)
    integer, intent(in) :: cp
    wide = cp >= int(z'1100') .and. ( &
      cp <= int(z'115F') .or. cp == int(z'2329') .or. cp == int(z'232A') .or. &
      (cp >= int(z'2E80') .and. cp <= int(z'A4CF') .and. cp /= int(z'303F')) .or. &
      (cp >= int(z'AC00') .and. cp <= int(z'D7A3')) .or. &
      (cp >= int(z'F900') .and. cp <= int(z'FAFF')) .or. &
      (cp >= int(z'FE10') .and. cp <= int(z'FE19')) .or. &
      (cp >= int(z'FE30') .and. cp <= int(z'FE6F')) .or. &
      (cp >= int(z'FF00') .and. cp <= int(z'FF60')) .or. &
      (cp >= int(z'FFE0') .and. cp <= int(z'FFE6')) .or. &
      (cp >= int(z'1F300') .and. cp <= int(z'1FAFF')) .or. &
      (cp >= int(z'20000') .and. cp <= int(z'3FFFD')))
  end function is_wide

end module fgof_screen
