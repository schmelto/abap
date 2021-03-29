CLASS test_zcl_fi_edi_payext_2441 DEFINITION Duration short risk level harmless.

    PRIVATE SECTION.
    DATA: class TYPE REF TO class.

    METHODS: "! <p class="shorttext synchronized">setup testing class</p>
             setup.
    METHODS: "! <p class="shorttext synchronized">verify inputs from testing method</p>
             "! @parameter input | Input String
             "! @parameter num_last_chars | Number of characters
             "! @parameter expected | Expected Output
             verify
                IMPORTING
                    input          TYPE string
                    num_last_chars TYPE i
                    expected       TYPE string.

    METHODS: "! <p class="shorttext synchronized">Test method for getting x last characters</p>
             test_get_x_last_chars FOR TESTING.
    
ENDCLASS.

CLASS test_zcl_fi_edi_payext_2441 IMPLEMENTATION.

    METHOD setup.
        class = NEW class( ).
    ENDMETHOD.

    METHOD verify.
        cl_aunit_assert=>assert_equals( exp            = expected
                                        act            = class->get_x_last_chars( string = input
                                        num_last_chars = num_last_chars ) ).
    ENDMETHOD.

    METHOD test_get_x_last_chars.
        verify( input = '123'     num_last_chars = 5 expected = '123' ).
        verify( input = '1234567' num_last_chars = 6 expected = '234567' ).
        verify( input = '1'       num_last_chars = 8 expected = '1' ).
        verify( input = '12345'   num_last_chars = 5 expected = '12345' ).
    ENDMETHOD.

ENDCLASS.
