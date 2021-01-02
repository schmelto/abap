# abap-templates
ABAP-Templates



## Forms


### Get X last characters of a string
```abap
FORM get_x_last_chars USING iv_string
                            iv_num_last_chars TYPE i
                      CHANGING cv_result.
  IF strlen( iv_string ) < iv_num_last_chars.
    cv_result = iv_string.
  ELSE.
    cv_result = substring( val = iv_string
                           off = strlen( iv_string ) - iv_num_last_chars
                           len = iv_num_last_chars ).
  ENDIF.
ENDFORM.
```

## Classes

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



# ABAP Basics




btw regarding question number 2, point 1. 
stcd1 is a 16 character field, and ls_e1edka1-partn is a 17 character, 

if you still want to take the rightmost 8 characters of stcd1 and assign them to ls_e1edka1-partn, 

do this. 

```abap
SELECT SINGLE stcd1 FROM lfa1 INTO data(stcd1) WHERE lifnr = im_reguh-lifnr.

ls_e1edka1-partn = stcd1+8(8).
```

