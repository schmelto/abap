
## Methods

### Get X last characters of a string

To get the x last characters of a string we can use following method:

```abap
string = substring( val = string
                    off = strlen( string ) - 8
                    len = 8 ).
```
**Problem:** Invalid access to a string using negative offset when the string is shorter then the offset!

To implement this in our own custom class we can use following code:

```abap
CLASS class DEFINITION.
  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Get the x last characters</p>
    "! @parameter string | <p class="shorttext synchronized">Input string</p>
    "! @parameter num_last_chars | <p class="shorttext synchronized">Number of digits</p>
    "! @parameter result | <p class="shorttext synchronized">Result</p>
    METHODS
      get_x_last_chars
        IMPORTING
          string         TYPE string
          num_last_chars TYPE i
        RETURNING
          VALUE(result)  TYPE string.
ENDCLASS.

CLASS class IMPLEMENTATION.

  METHOD get_x_last_chars.
    IF strlen( string ) < num_last_chars.
      result = string.
    ELSE.
      result = substring( val = string
                          off = strlen( string ) - num_last_chars
                          len = num_last_chars ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA string TYPE string VALUE '0123456789'.
  DATA(class) = NEW class( ).
  string = class->get_x_last_chars( string         = string
                                    num_last_chars = 5 ).
```

Why should we define a own method?
* Reducing parameters in the function
* make the code more readable
* catch "Invalid access to a string using negative offset" within the `IF`-Statement in the method

We also can use a subroutine to get the same result in the code:

```abap
FORM get_x_last_chars USING string
                            num_last_chars TYPE i
                      CHANGING result.
  IF strlen( string ) < num_last_chars.
    cv_result = iv_string.
  ELSE.
    result = substring( val = string
                        off = strlen( string ) - num_last_chars
                        len = num_last_chars ).
  ENDIF.
ENDFORM.
```
This approach is outdated and should not used anymore (see [here](https://answers.sap.com/questions/13218815/when-does-it-make-sense-to-use-subroutines-form-an.html))

An **other solution** for this problem can be solved like this:

```abap
SELECT SINGLE stcd1 FROM lfa1 INTO data(stcd1) WHERE lifnr = 123456789.
string = stcd1+8(8).
```
`stcd1` is a 16 character field, you can to take the rightmost 8 characters of stcd1 with `stcd1+8(8)` (remember 8 + 8 = 16).

**Unit test for this method:**

```abap
CLASS test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS: test_get_x_last_chars FOR TESTING.
ENDCLASS.
CLASS test IMPLEMENTATION.
  METHOD test_get_x_last_chars.
    DATA: string TYPE string VALUE '987654321'.
    DATA(class) = NEW class( ).
    cl_aunit_assert=>assert_equals( exp = '54321'
                                    act = class->get_x_last_chars( string         = string
                                                                   num_last_chars = 5 ) ).
  ENDMETHOD.
ENDCLASS.
```

### loop group by

```abap
CLASS lcl_loop_groupby DEFINITION CREATE PRIVATE FINAL.

PUBLIC SECTION.

    CLASS-METHODS: create
        RETURNING
            VALUE(ro_obj) TYPE REF TO lcl_loop_groupby.

    METHODS: run.

PROTECTED SECTION.
PRIVATE SECTION.

ENDCLASS.

CLASS lcl_loop_groupby IMPLEMENTATION.

    METHOD create.
        ro_obj = NEW lcl_loop_groupby( ).
    ENDMETHOD.

    METHOD run.

        SELECT *
        FROM spfli
        INTO TABLE @DATA(lt_spfli).
        DATA members LIKE lt_spfli.

        LOOP AT lt_spfli INTO DATA(ls_spfli)
            GROUP BY ( carrier = ls_spfli-carrid city_from = ls_spfli-cityfrom )
            ASCENDING
            ASSIGNING FIELD-SYMBOL(<lfs_group>).

            CLEAR members.
            LOOP AT GROUP <lfs_group> ASSIGNING FIELD-SYMBOL(<lfs_spfli_group>).
                members = VALUE #( BASE members ( <lfs_spfli_group> ) ).
            ENDLOOP.
                cl_demo_output=>write( members ).
        ENDLOOP.
        cl_demo_output=>display( ).

    ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

lcl_loop_groupby=>create( )->run( ).

```

#### What's "!" in method declaration?

**Example:**

```abap
METHODS set_item_zz_sgtxt
    IMPORTING
      !is_input TYPE any
    CHANGING
      !cs_linetype_data TYPE any.
```

**Explanation:**

The exclamation mark is nothing more than a hint to the ABAP compiler.
It is required when you want use keywords like export, exceptions and so on as formal/actual argument.

### Method chaining

```abap
CATCH zcx_excel INTO exception.
zcl_bc_screen_message=>output( text = exeption->get_text( ) ).
```

### Remove leading zeros for WRITE statement

**New:**
```abap
message = |{ delivery ALPHA = OUT }|.
```

**Old:** 
```abap
CALL FUNCTION 'CONVERSTION_EXIT_ALPHA_OUTPUT'
    EXPORTING in = delivery
    IMPORTING = delivery.
" and for further processing add the leading zeros back
CALL FUNCTION 'CONVERSTION_EXIT_ALPHA_INPUT'
    EXPORTING in = delivery
    IMPORTING = delivery.
```
