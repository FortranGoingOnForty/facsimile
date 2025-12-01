module signature_tooltip_module
    use iso_fortran_env, only: int32
    use terminal_io_module, only: terminal_move_cursor, terminal_write
    implicit none
    private

    public :: signature_tooltip_t
    public :: init_signature_tooltip, cleanup_signature_tooltip
    public :: show_signature_tooltip, hide_signature_tooltip
    public :: is_signature_tooltip_visible, handle_signature_response
    public :: set_active_parameter

    ! Parameter information
    type :: parameter_info_t
        character(len=:), allocatable :: label
        character(len=:), allocatable :: documentation
    end type parameter_info_t

    ! Signature information
    type :: signature_info_t
        character(len=:), allocatable :: label
        character(len=:), allocatable :: documentation
        type(parameter_info_t), allocatable :: parameters(:)
        integer :: num_parameters = 0
    end type signature_info_t

    ! Signature tooltip
    type :: signature_tooltip_t
        logical :: visible = .false.
        integer :: row = 1
        integer :: col = 1

        ! Signature data
        type(signature_info_t), allocatable :: signatures(:)
        integer :: num_signatures = 0
        integer :: active_signature = 1
        integer :: active_parameter = 0

        ! Display settings
        integer :: max_width = 80
    end type signature_tooltip_t

contains

    subroutine init_signature_tooltip(tooltip)
        type(signature_tooltip_t), intent(out) :: tooltip

        tooltip%visible = .false.
        tooltip%num_signatures = 0
        tooltip%active_signature = 1
        tooltip%active_parameter = 0
        tooltip%max_width = 80
    end subroutine init_signature_tooltip

    subroutine cleanup_signature_tooltip(tooltip)
        type(signature_tooltip_t), intent(inout) :: tooltip
        call clear_signatures(tooltip)
        tooltip%visible = .false.
    end subroutine cleanup_signature_tooltip

    subroutine clear_signatures(tooltip)
        type(signature_tooltip_t), intent(inout) :: tooltip
        integer :: i, j

        if (allocated(tooltip%signatures)) then
            do i = 1, tooltip%num_signatures
                if (allocated(tooltip%signatures(i)%label)) &
                    deallocate(tooltip%signatures(i)%label)
                if (allocated(tooltip%signatures(i)%documentation)) &
                    deallocate(tooltip%signatures(i)%documentation)
                if (allocated(tooltip%signatures(i)%parameters)) then
                    do j = 1, tooltip%signatures(i)%num_parameters
                        if (allocated(tooltip%signatures(i)%parameters(j)%label)) &
                            deallocate(tooltip%signatures(i)%parameters(j)%label)
                        if (allocated(tooltip%signatures(i)%parameters(j)%documentation)) &
                            deallocate(tooltip%signatures(i)%parameters(j)%documentation)
                    end do
                    deallocate(tooltip%signatures(i)%parameters)
                end if
            end do
            deallocate(tooltip%signatures)
        end if

        tooltip%num_signatures = 0
        tooltip%active_signature = 1
        tooltip%active_parameter = 0
    end subroutine clear_signatures

    subroutine show_signature_tooltip(tooltip, row, col)
        type(signature_tooltip_t), intent(inout) :: tooltip
        integer, intent(in) :: row, col

        tooltip%row = row
        tooltip%col = col
        tooltip%visible = .true.
    end subroutine show_signature_tooltip

    subroutine hide_signature_tooltip(tooltip)
        type(signature_tooltip_t), intent(inout) :: tooltip
        tooltip%visible = .false.
    end subroutine hide_signature_tooltip

    function is_signature_tooltip_visible(tooltip) result(visible)
        type(signature_tooltip_t), intent(in) :: tooltip
        logical :: visible
        visible = tooltip%visible
    end function is_signature_tooltip_visible

    subroutine set_active_parameter(tooltip, param_index)
        type(signature_tooltip_t), intent(inout) :: tooltip
        integer, intent(in) :: param_index
        tooltip%active_parameter = param_index
    end subroutine set_active_parameter

    ! UNUSED: Render signature tooltip with parameter highlighting
    ! Kept for potential future use
    ! subroutine render_signature_tooltip(tooltip)
    !     type(signature_tooltip_t), intent(in) :: tooltip
    !     integer :: display_row, sig_idx
    !     character(len=512) :: line
    !     character(len=256) :: label
    !     integer :: param_start, param_end, i
    !
    !     if (.not. tooltip%visible .or. tooltip%num_signatures == 0) return
    !
    !     sig_idx = tooltip%active_signature
    !     if (sig_idx < 1 .or. sig_idx > tooltip%num_signatures) return
    !
    !     display_row = tooltip%row
    !
    !     ! Draw background
    !     call terminal_move_cursor(display_row, tooltip%col)
    !     call terminal_write(char(27) // '[48;5;238m')  ! Dark background
    !
    !     ! Build signature line with parameter highlighting
    !     if (allocated(tooltip%signatures(sig_idx)%label)) then
    !         label = tooltip%signatures(sig_idx)%label
    !
    !         ! If we have parameters, try to highlight the active one
    !         if (tooltip%signatures(sig_idx)%num_parameters > 0 .and. &
    !             tooltip%active_parameter > 0 .and. &
    !             tooltip%active_parameter <= tooltip%signatures(sig_idx)%num_parameters) then
    !
    !             ! Get the parameter label to highlight
    !             if (allocated(tooltip%signatures(sig_idx)%parameters(tooltip%active_parameter)%label)) then
    !                 block
    !                     character(len=:), allocatable :: param_label
    !                     param_label = tooltip%signatures(sig_idx)%parameters(tooltip%active_parameter)%label
    !
    !                     ! Find parameter in signature
    !                     param_start = index(label, trim(param_label))
    !                     if (param_start > 0) then
    !                         param_end = param_start + len_trim(param_label) - 1
    !
    !                         ! Build highlighted line
    !                         line = " "
    !                         if (param_start > 1) then
    !                             line = trim(line) // label(1:param_start-1)
    !                         end if
    !
    !                         ! Highlight the active parameter
    !                         line = trim(line) // char(27) // '[1;33m'  ! Bold yellow
    !                         line = trim(line) // label(param_start:param_end)
    !                         line = trim(line) // char(27) // '[0;48;5;238m'  ! Reset to dark bg
    !
    !                         if (param_end < len_trim(label)) then
    !                             line = trim(line) // label(param_end+1:len_trim(label))
    !                         end if
    !                         line = trim(line) // " "
    !                     else
    !                         ! Couldn't find parameter, show plain
    !                         line = " " // trim(label) // " "
    !                     end if
    !                 end block
    !             else
    !                 line = " " // trim(label) // " "
    !             end if
    !         else
    !             line = " " // trim(label) // " "
    !         end if
    !     else
    !         line = " (no signature) "
    !     end if
    !
    !     ! Truncate if too long
    !     if (len_trim(line) > tooltip%max_width) then
    !         line = line(1:tooltip%max_width-3) // "..."
    !     end if
    !
    !     call terminal_write(trim(line))
    !
    !     ! Show which signature if multiple
    !     if (tooltip%num_signatures > 1) then
    !         block
    !             character(len=20) :: sig_indicator
    !             write(sig_indicator, '(A,I0,A,I0,A)') " (", sig_idx, "/", tooltip%num_signatures, ")"
    !             call terminal_write(char(27) // '[90m')  ! Gray
    !             call terminal_write(trim(sig_indicator))
    !         end block
    !     end if
    !
    !     ! Reset colors
    !     call terminal_write(char(27) // '[0m')
    ! end subroutine render_signature_tooltip

    subroutine handle_signature_response(tooltip, response)
        use lsp_protocol_module, only: lsp_message_t
        use json_module, only: json_value_t, json_get_array, json_get_object, &
                               json_get_string, json_get_number, json_array_size, &
                               json_get_array_element, json_has_key
        type(signature_tooltip_t), intent(inout) :: tooltip
        type(lsp_message_t), intent(in) :: response
        type(json_value_t) :: result_obj, signatures_array, sig_obj
        type(json_value_t) :: params_array, param_obj
        integer :: num_signatures, num_params, i, j
        character(len=:), allocatable :: label, doc
        real(8) :: active_sig_real, active_param_real

        ! Clear existing signatures
        call clear_signatures(tooltip)

        ! Get result object
        result_obj = response%result

        ! Get signatures array
        if (.not. json_has_key(result_obj, 'signatures')) then
            tooltip%visible = .false.
            return
        end if

        signatures_array = json_get_array(result_obj, 'signatures')
        num_signatures = json_array_size(signatures_array)

        if (num_signatures == 0) then
            tooltip%visible = .false.
            return
        end if

        ! Allocate signatures
        allocate(tooltip%signatures(num_signatures))
        tooltip%num_signatures = num_signatures

        ! Parse each signature
        do i = 1, num_signatures
            ! json_get_array_element expects 0-based index
            sig_obj = json_get_array_element(signatures_array, i - 1)

            ! Get label (required)
            if (json_has_key(sig_obj, 'label')) then
                label = json_get_string(sig_obj, 'label', '')
                allocate(character(len=len(label)) :: tooltip%signatures(i)%label)
                tooltip%signatures(i)%label = label
            end if

            ! Get documentation (optional)
            if (json_has_key(sig_obj, 'documentation')) then
                doc = json_get_string(sig_obj, 'documentation', '')
                if (len(doc) > 0) then
                    allocate(character(len=len(doc)) :: tooltip%signatures(i)%documentation)
                    tooltip%signatures(i)%documentation = doc
                end if
            end if

            ! Get parameters (optional)
            if (json_has_key(sig_obj, 'parameters')) then
                params_array = json_get_array(sig_obj, 'parameters')
                num_params = json_array_size(params_array)

                if (num_params > 0) then
                    allocate(tooltip%signatures(i)%parameters(num_params))
                    tooltip%signatures(i)%num_parameters = num_params

                    do j = 1, num_params
                        ! json_get_array_element expects 0-based index
                        param_obj = json_get_array_element(params_array, j - 1)

                        ! Get parameter label
                        if (json_has_key(param_obj, 'label')) then
                            ! Label can be string or [start, end] range
                            label = json_get_string(param_obj, 'label', '')
                            if (len(label) > 0) then
                                allocate(character(len=len(label)) :: &
                                    tooltip%signatures(i)%parameters(j)%label)
                                tooltip%signatures(i)%parameters(j)%label = label
                            end if
                        end if

                        ! Get parameter documentation (optional)
                        if (json_has_key(param_obj, 'documentation')) then
                            doc = json_get_string(param_obj, 'documentation', '')
                            if (len(doc) > 0) then
                                allocate(character(len=len(doc)) :: &
                                    tooltip%signatures(i)%parameters(j)%documentation)
                                tooltip%signatures(i)%parameters(j)%documentation = doc
                            end if
                        end if
                    end do
                end if
            end if
        end do

        ! Get active signature
        if (json_has_key(result_obj, 'activeSignature')) then
            active_sig_real = json_get_number(result_obj, 'activeSignature', 0.0d0)
            tooltip%active_signature = int(active_sig_real) + 1  ! Convert 0-based to 1-based
        else
            tooltip%active_signature = 1
        end if

        ! Get active parameter
        if (json_has_key(result_obj, 'activeParameter')) then
            active_param_real = json_get_number(result_obj, 'activeParameter', 0.0d0)
            tooltip%active_parameter = int(active_param_real) + 1  ! Convert 0-based to 1-based
        else
            tooltip%active_parameter = 1
        end if

    end subroutine handle_signature_response

end module signature_tooltip_module
