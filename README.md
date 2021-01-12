# ABAP

## Eclipse Shortcuts

| shortcut | what it does |
|----|----|
| `str` + `1` | add ABAP Doc |
| `str` + `shift` + `alt` | Search |
| `alt` + `shift` + `A` | Eclipse Box Selection |

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

Unit test for this method:

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
                                    act = class->get_x_last_chars( string = string
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

## Function Modules

Function modules are outdated. If you have to use one cause of implementing a BTE or something else create this function module and call a global class in it.
**Example:**

We have following function module:
```abap
FUNCTION Z_FI_EDI_PAYEXT_2441
  IMPORTING
    IM_MESTYP LIKE EDIDC-MESTYP
    IM_REGUH LIKE REGUH
    IM_REGUD LIKE REGUD
    IM_FLAG_NO_REPLACE TYPE C
  EXPORTING
    EX_FIMSG LIKE FIMSG
  CHANGING
    CH_XAVIS TYPE C
    CH_EDIDC LIKE EDIDC
  TABLES
    T_REGUP LIKE REGUP
    T_EDIDD LIKE EDIDD
  EXCEPTIONS
    DONT_CREATE_IDOC.
    
    " place for some coding or the creation of the object and calling the method
    
ENDFUNCTION.
```
In here we generate a new object from a global class in which our futher coding will be placed.

```abap
DATA(zcl_fi_edi_payext_2441) = NEW zcl_fi_edi_payext_2441( ).
```
**Note:** Use inline declarations!

Now we can call a public method of this class/object and can implement all further logic in here.
We should put all parameters from the function module in the call of this method and made some exception handling here.
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
The `[]` in `t_regup[]` is because we do not only want to pass the header line to the class rather then entire table.

The global class can look something like this:
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
**Note:** We have to define `t_regup TYPE TABLE OF regup` and `t_edidd TYPE TABLE OF edidd.` for using them in the class.

To make the method more readable we can use ABAP doc expressions like this:
```abap
"! <p class="shorttext synchronized">Modify payment IDOCs</p>
"! @exception dont_create_idoc | <p class="shorttext synchronized">Don't created a payment IDOC</p>
"! @parameter im_mestyp | <p class="shorttext synchronized">Message type</p>
"! @parameter im_reguh | <p class="shorttext synchronized">Payment data from the payment program</p>
"! @parameter im_regud | <p class="shorttext synchronized">Transfer data form printing</p>
"! @parameter im_flag_no_replace | <p class="shorttext synchronized">SPACE=Special characters conversion required in texts</p>
"! @parameter ex_fimsg | <p class="shorttext synchronized">FI-messages</p>
"! @parameter ch_xavis | <p class="shorttext synchronized">Flag: Advice required</p>
"! @parameter ch_edidc | <p class="shorttext synchronized">Control record (IDoc)</p>
"! @parameter t_regup | <p class="shorttext synchronized">Processed items from the payment program</p>
"! @parameter t_edidd | <p class="shorttext synchronized">Data record (IDoc)</p>
```

The text between those bracets (`<p class="shorttext synchronized">Here some explanation text</p>`) is also shown in SAP Gui or in the ABAP Element Info in Eclipse.

Types can be documented like:

```abap
TYPES:
   "! <p class="shorttext synchronized">Explanation text</p>
   type TYPE TABLE OF spfli.
```

## ABAP Unit Tests

For ABAP Unit Tests you have to declare a testing class (`CLASS test DEFINITION`) with the additive `FOR TESTING`.

Here we have an example of a normal class with two methods `set_text_to_x` and `minus_ten_percent` for wich we want to define some unit tests.
```abap
CLASS class DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA text TYPE string.
    CLASS-METHODS set_text_to_x.
    METHODS minus_ten_percent CHANGING price TYPE p.
ENDCLASS.

CLASS class IMPLEMENTATION.
  METHOD set_text_to_x.
    text = 'U'. " should be 'X'
  ENDMETHOD.
  METHOD minus_ten_percent.
    price = price * '0.9'.
  ENDMETHOD.
ENDCLASS.
```

Now we can declare a test class and tests for the two methodes above.

```abap
CLASS test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_for_x FOR TESTING.
    METHODS test_minus_ten_percent FOR TESTING.
ENDCLASS.

CLASS test IMPLEMENTATION.
  METHOD test_for_x.
    class=>set_text_to_x( ).
    cl_aunit_assert=>assert_equals( act = class=>text
                                    exp = 'X'
                                    msg = 'Text "' && class=>text && '" is not equals "X".').
  ENDMETHOD.
  METHOD test_minus_ten_percent.

    DATA: price TYPE p VALUE 200.
    DATA(class) = NEW class( ).

    class->minus_ten_percent(
      CHANGING
        price = price
    ).

    cl_aunit_assert=>assert_equals( act = price
                                    exp = 180
                                    msg = 'Ninty percent not calculated correctly').

  ENDMETHOD.
ENDCLASS.
```

| **Risk Level** | Description |
|----|----|
| CRITICAL | The test could change system settings or the Customizing, for example. |
| DANGEROUS | The test could change persistent application data, for example. |
| HARMLESS | The test has no effect on persistent data or system settings. |

`... RISK LEVEL {CRITICAL|DANGEROUS|HARMLESS}`

| **Execution Duration** | Description |
|---|---|
| SHORT | an imperceptibly short execution duration is expected. This is the default value. |
| MEDIUM | a noticeable execution duration is expected. |
| LONG | a very noticeable execution duration is expected. |

`... DURATION {SHORT|MEDIUM|LONG}`
