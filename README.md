# ABAP

## Eclipse Shortcuts

| shortcut | what it does |
|----|----|
| `str` + `1` | add ABAP Doc |

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
CLASS helper DEFINITION.
  PUBLIC SECTION.
    METHODS
      get_x_last_chars
        IMPORTING
          string         TYPE string
          num_last_chars TYPE i
        RETURNING
          VALUE(result)  TYPE string.
ENDCLASS.

CLASS helper IMPLEMENTATION.

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
  DATA(helper) = NEW helper( ).
  string = helper->get_x_last_chars( string         = string
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
