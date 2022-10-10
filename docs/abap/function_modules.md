## Function Modules

Function modules are outdated. If you have to use one because you have tp implement a Busniess Transaction Event (BTE) or something else create the function module and call a global class within it. The class should handle all the logic, the function module is only for "calling" the global class.

**Example:**

We have following function module:

```abap
FUNCTION z_fi_edi_payext_2441
  IMPORTING
    im_mestyp LIKE edidc-mestyp
    im_reguh LIKE reguh
    im_regud LIKE regud
    im_flag_no_replace TYPE c
  EXPORTING
    ex_fimsg LIKE fimsg
  CHANGING
    ch_xavis TYPE c
    ch_edidc LIKE edidc
  TABLES
    t_regup LIKE regup
    t_edidd LIKE edidd
  EXCEPTIONS
    dont_create_idoc.

    " place for some coding

ENDFUNCTION.
```

We have to create a new object with the type of out global class in which our further coding will take place.

```abap
DATA(zcl_fi_edi_payext_2441) = NEW zcl_fi_edi_payext_2441( ).
```

> **Note:** Use inline declarations!

Now we can call a public method of this class/object in which the logic part will be implemented.
As best practice should we put all parameters from the function module in the call of the method (and made some initial exception handling here).

```abap
zcl_fi_edi_payext_2441->change_idoc(
    EXPORTING
      im_mestyp          = im_mestyp
      im_reguh           = im_reguh
      im_regud           = im_regud
      im_flag_no_replace = im_flag_no_replace
    IMPORTING
      ex_fimsg           = ex_fimsg
    CHANGING
      ch_xavis           = ch_xavis
      ch_edidc           = ch_edidc
      t_regup            = t_regup[]
      t_edidd            = t_edidd[]
    EXCEPTIONS
      dont_create_idoc   = 1
      OTHERS             = 2
  ).
  IF sy-subrc <> 0.
    RAISE dont_create_idoc.
  ENDIF.
```

> The `[]` in `t_regup[]` is because we do not only want to pass the header line to the class rather then the entire table.

The first draft of the global class can look something like this:

```abap
CLASS zcl_fi_edi_payext_2441 DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: t_regup TYPE TABLE OF regup,
           t_edidd TYPE TABLE OF edidd.

    METHODS change_idoc
      IMPORTING
        im_mestyp          TYPE edidc-mestyp
        im_reguh           TYPE reguh
        im_regud           TYPE regud
        im_flag_no_replace TYPE c
      EXPORTING
        ex_fimsg           TYPE fimsg
      CHANGING
        ch_xavis           TYPE c
        ch_edidc           TYPE edidc
        t_regup            TYPE t_regup
        t_edidd            TYPE t_edidd
      EXCEPTIONS
        dont_create_idoc.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_fi_edi_payext_2441 IMPLEMENTATION.
  METHOD change_idoc.
    " Here the logic of the program
  ENDMETHOD.
ENDCLASS.
```

> **Note:** We have to define `t_regup TYPE TABLE OF regup` and `t_edidd TYPE TABLE OF edidd` for using them in the class.

> **Note:** The method which is also called in the function module has to be in the `PUBLIC SECTION` of the class.
