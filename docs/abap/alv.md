## ALV - ABAP List Viewer

### Basic ALV Functions

To define a new ALV Grid you can use the type `cl_salv_table`.

```abap
DATA: alv TYPE REF TO cl_salv_table.
```

Of course we need some data to display something. In this case we select enteries from table `t001`.

```abap
SELECT * FROM t001 INTO TABLE @DATA(t001).
```

To fill the ALV Grid with data we can use following coding:

```abap
TRY.
  cl_salv_table=>factory(
    IMPORTING
      r_salv_table   = alv
    CHANGING
      t_table        = t001 ).
  CATCH cx_salv_msg INTO DATA(msg).
    cl_demo_output=>display( msg ).
ENDTRY.
```

Now we can display the ALV Grid with the `display( )`-function:

```abap
alv->display( ).
```

### Basic ALV Grid

Following coding shows how to display a basic ALV Grid. Further it modifies some ALV-functions and rename some columns.

```abap
CLASS t001_alv DEFINITION.

    PUBLIC SECTION.
        METHODS:
            "! <p class="shorttext synchronized">Get all data of table T001</p>
            get_all_t001_data,
            "! <p class="shorttext synchronized">Show ALV functions</p>
            set_alv_functions,
            "! <p class="shorttext synchronized">Edit ALV columns</p>
            set_alv_columns,
            "! <p class="shorttext synchronized">Display table T001 as ALV-Grid</p>
            display_t001_data.
    PROTECTED SECTION.

    PRIVATE SECTION.
        DATA: t001 TYPE TABLE OF t001,
              alv TYPE REF TO cl_salv_table.

ENDCLASS.

CLASS t001_alv IMPLEMENTATION.

  METHOD display_t001_data.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = alv
          CHANGING
            t_table        = t001
        ).
      CATCH cx_salv_msg INTO DATA(msg).
        cl_demo_output=>display( msg ).
    ENDTRY.

    set_alv_functions( ).
    set_alv_columns( ).

    alv->display( ).
  ENDMETHOD.

  METHOD get_all_t001_data.
    SELECT * FROM t001 INTO TABLE t001.
  ENDMETHOD.

  METHOD set_alv_functions.
    DATA(alv_functions_list) = alv->get_functions( ).
    " show toolbar
    alv_functions_list->set_all( value = if_salv_c_bool_sap=>true ).
    " don't display sort ascending
    alv_functions_list->set_sort_asc( value = if_salv_c_bool_sap=>false ).
  ENDMETHOD.

  METHOD set_alv_columns.
    TRY.
        " get all columns of the ALV
        DATA(columns) = alv->get_columns( ).

        " get the MANDT column of the ALV
        DATA(colum_mandt) = columns->get_column( columnname = 'MANDT' ).
        " hide MANDT column
        colum_mandt->set_visible( value = if_salv_c_bool_sap=>false ).

        " get the BUKRS column of the ALV
        DATA(colum_company) = columns->get_column( columnname = 'BUKRS' ).
        " change column name of BUKRS
        colum_company->set_short_text( 'Company' ).
        colum_company->set_medium_text( 'Company Nr.' ).
        colum_company->set_long_text( 'Company Number' ).

      CATCH cx_salv_not_found INTO DATA(msg).
        cl_demo_output=>display( msg ).

    ENDTRY.
  ENDMETHOD.

ENDCLASS.

" here the report start
START-OF-SELECTION.
DATA(t001_alv) = NEW t001_alv( ).

t001_alv->get_all_t001_data( ).
t001_alv->display_t001_data( ).
```

### Translate columns

see [here](https://stackoverflow.com/questions/73231945/translate-custom-alv-columns/73232101#73232101)
