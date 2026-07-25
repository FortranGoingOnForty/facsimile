! Ollama request construction and response reading.
!
! The single most important rule here: ALWAYS send `suffix`, even when empty.
!
! Verified against a live model with one prompt, three call shapes:
!   suffix present      -> "return a + b;"          (a real completion)
!   raw:true, no suffix -> overruns, then rambles
!   neither             -> "Certainly! It looks like you're starting to..."
!
! Without a suffix, ollama applies the model's chat template -- confirmed by
! the returned context beginning with the <|im_start|>system token. Prose
! landing in a source file is precisely the failure this feature must not
! have, so the request shape is a safety control, not a detail.
!
! Second rule: refuse models that do not advertise the `insert` capability.
! A chat model cannot see the code after the caret and will guess at it.
module ollama_client_module
    use ai_json_module, only: ai_json_escape, ai_json_get_string, &
                              ai_json_array_has_string, ai_json_get_logical
    implicit none
    private

    public :: ollama_generate_body, ollama_show_body
    public :: ollama_model_supports_fim, ollama_read_completion, ollama_read_error

contains

    ! Body for POST /api/generate. suffix is always present.
    function ollama_generate_body(model, prefix, suffix, num_predict, &
                                  temperature_x100, keep_alive) result(body)
        character(len=*), intent(in) :: model, prefix, suffix, keep_alive
        integer, intent(in) :: num_predict, temperature_x100
        character(len=:), allocatable :: body
        character(len=16) :: np, temp

        write(np, '(i0)') num_predict
        write(temp, '(f5.2)') real(temperature_x100) / 100.0

        body = '{"model":"' // trim(model) // '",' // &
               '"prompt":"' // ai_json_escape(prefix) // '",' // &
               '"suffix":"' // ai_json_escape(suffix) // '",' // &
               '"stream":false,' // &
               '"keep_alive":"' // trim(keep_alive) // '",' // &
               '"options":{' // &
               '"num_predict":' // trim(adjustl(np)) // ',' // &
               '"temperature":' // trim(adjustl(temp)) // ',' // &
               '"repeat_penalty":1.05' // &
               '}}'
    end function ollama_generate_body

    function ollama_show_body(model) result(body)
        character(len=*), intent(in) :: model
        character(len=:), allocatable :: body

        body = '{"model":"' // trim(model) // '"}'
    end function ollama_show_body

    ! True when /api/show reports the `insert` capability. Anything else
    ! cannot fill in the middle and must not be used for inline completion --
    ! it would be guessing at code it cannot see.
    function ollama_model_supports_fim(show_response) result(res)
        character(len=*), intent(in) :: show_response
        logical :: res

        res = ai_json_array_has_string(show_response, 'capabilities', 'insert')
    end function ollama_model_supports_fim

    ! Pull the completion text out of a /api/generate response.
    ! done_reason is reported so the caller knows whether generation stopped
    ! naturally or hit the token cap mid-token.
    subroutine ollama_read_completion(response, text, done_reason, ok)
        character(len=*), intent(in) :: response
        character(len=:), allocatable, intent(out) :: text, done_reason
        logical, intent(out) :: ok
        logical :: got

        text = ''
        done_reason = ''
        call ai_json_get_string(response, 'response', text, ok)
        if (.not. ok) return
        call ai_json_get_string(response, 'done_reason', done_reason, got)
        if (.not. got) done_reason = ''
    end subroutine ollama_read_completion

    ! Ollama reports failures as {"error": "..."} with a non-200 status.
    function ollama_read_error(response) result(msg)
        character(len=*), intent(in) :: response
        character(len=:), allocatable :: msg
        logical :: ok

        call ai_json_get_string(response, 'error', msg, ok)
        if (.not. ok) msg = ''
    end function ollama_read_error

end module ollama_client_module
