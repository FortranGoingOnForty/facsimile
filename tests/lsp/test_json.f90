program test_json
    use json_module
    use iso_fortran_env, only: real64
    implicit none

    type(json_value_t) :: json_obj
    character(len=100) :: json_str
    real(real64) :: id_value

    ! Test with a simple JSON response like what we get from clangd
    json_str = '{"id":1,"jsonrpc":"2.0","result":{"capabilities":{}}}'

    print *, "Testing JSON: ", trim(json_str)

    ! Parse it
    json_obj = json_parse(json_str)

    ! Check if we can find the id
    if (json_has_key(json_obj, "id")) then
        print *, "Found id key!"
        id_value = json_get_number(json_obj, "id", -1.0_real64)
        print *, "ID value: ", id_value
    else
        print *, "No id key found"
    end if

    ! Check for jsonrpc
    if (json_has_key(json_obj, "jsonrpc")) then
        print *, "Found jsonrpc key!"
    else
        print *, "No jsonrpc key found"
    end if

end program test_json