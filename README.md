# ABAP

## Eclipse Shortcuts

| shortcut | what it does |
|----|----|
| <kbd>alt</kbd> + <kbd>shift</kbd> + <kbd>A</kbd> | Eclipse Box Selection |
| <kbd>alt</kbd> + <kbd>str</kbd> + <kbd>&#8595;</kbd> | duplicate lines |
| <kbd>alt</kbd> + <kbd>U</kbd> | delete unused variables |
| <kbd>shift</kbd> + <kbd>F1</kbd> | Pretty Printer |
| <kbd>str</kbd> + <kbd>1</kbd> | Opens Quickfix/Quickassist Dialog on the selected element |
| <kbd>str</kbd> + <kbd>H</kbd> | Abap Source Text Search |
| <kbd>str</kbd> + <kbd>shift</kbd> + <kbd>alt</kbd> | Search |
| <kbd>str</kbd> + <kbd>shift</kbd> + <kbd>L</kbd> | List all available Keybord-Shortcuts |
| <kbd>str</kbd> + <kbd>space</kbd> | Auto complete |
| <kbd>str</kbd> + <kbd><</kbd> | Comment |
| <kbd>str</kbd> + <kbd><</kbd> | Undo comment |

&#8594; more shortcuts can be found [here](https://blogs.sap.com/2021/07/16/abap-adt-frequently-used-short-cuts/?utm_campaign=ABAPWeekly&utm_medium=email&utm_source=ABAPWeekly_50)

## DATA and Types

### Use inline declaration

This: 
```abap
DATA(name) = 'something'.
```
is better than this:
```abap
" anti-pattern
DATA: name Type string.
name = 'something'.
```

but unfortunately not always applicable...

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

## ABAP Docs

To make the method more readable/descriptive ABAP Doc expressions should be used:

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

Types can be documented like:

```abap
TYPES:
   "! <p class="shorttext synchronized">Explanation text</p>
   type TYPE TABLE OF spfli.
```

Same for `BEGIN OF...`-Structures.

```abap
TYPES:
	"! <p class="shorttext synchronized">Explanation text</p>
	BEGIN OF structure,
   		field TYPE OF type.
	TYPES END OF structure.
```

### Short Texts and their Synchronization

The text between those bracets (`<p class="shorttext synchronized">Here some explanation text</p>`) is also shown in SAP GUI or in the ABAP Element Info in Eclipse.

### Documentation Links

In an ABAP Doc comment, the following syntax can be used to refer to the documentation of other repository objects:

```abap
... {@link [[[kind:]name.]...][kind:]name} ...
```

Example:
```abap
"! {@link PROG:z_test.class.METH:get_x_last_chars}
```

### Test Relations

The following syntax can be used to define so called test relations in front of the declaration of a test class or a test method:

```abap
"! @testing [kind:]name
```

Example:
```abap
"! @testing my_cds_view
```

## ABAP Unit Tests

For ABAP Unit Tests you have to declare a testing class (`CLASS test DEFINITION`) with the additive `FOR TESTING`.

Here we have an example of a normal class with two methods `set_text_to_x` and `minus_ten_percent` for which we want to define some unit tests.

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

**An other Unit Test:**

```abap
CLASS money_machine DEFINITION.

    PUBLIC SECTION.
        METHODS get_ammount_in_coins
            IMPORTING ammount TYPE i
            RETURNING VALUE(value) TYPE i.
    PROTECTED SECTION.
    PRIVATE SECTION.
ENDCLASS.

CLASS money_machine IMPLEMENTATION.

  METHOD get_ammount_in_coins.

    value = COND #( WHEN ammount <= 0
                    THEN -1
                    ELSE ammount MOD 5 ).

  ENDMETHOD.

ENDCLASS.

CLASS test_get_ammount_in_coins DEFINITION FOR TESTING
                                RISK LEVEL HARMLESS
                                DURATION SHORT.

    PRIVATE SECTION.

        DATA cut TYPE REF TO money_machine.
        "! <p class="shorttext synchronized">Instance method, called before each test method</p>
        METHODS setup.
        "! <p class="shorttext synchronized">Amount of 1 EUR results in 1 EUR coin</p>
        METHODS ammount_1_coin_1 FOR TESTING.
        "! <p class="shorttext synchronized">Amount of 2 EUR results in 2 EUR coin</p>
        METHODS ammount_2_coin_2 FOR TESTING.

ENDCLASS.

CLASS test_get_ammount_in_coins IMPLEMENTATION.

  METHOD setup.
    "given
    "member variable which can be used in every other method.
    cut = NEW money_machine( ).
  ENDMETHOD.

  METHOD ammount_1_coin_1.
    "given not needed cause of setup method
    "DATA(cut) = new money_machine( ).
    "when
    DATA(coin_amount) = cut->get_ammount_in_coins( 1 ).
    "then
    cl_abap_unit_assert=>assert_equals( act = coin_amount
                                        exp = 1 ).
  ENDMETHOD.

  METHOD ammount_2_coin_2.
    "when
    DATA(coin_amount) = cut->get_ammount_in_coins( 2 ).
    "then
    cl_abap_unit_assert=>assert_equals( act = coin_amount
                                        exp = 2 ).
  ENDMETHOD.

ENDCLASS.
```

**Simulate different inputs in one testing method:**

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

CLASS test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
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
CLASS test IMPLEMENTATION.

  METHOD setup.
    class = NEW class( ).
  ENDMETHOD.

  METHOD verify.
    cl_aunit_assert=>assert_equals( exp = expected
                                    act = class->get_x_last_chars( string         = input
                                                                   num_last_chars = num_last_chars ) ).
  ENDMETHOD.

  METHOD test_get_x_last_chars.
    verify( input = '123'     num_last_chars = 5 expected = '123' ).
    verify( input = '1234567' num_last_chars = 6 expected = '234567' ).
    verify( input = '1'       num_last_chars = 8 expected = '1' ).
    verify( input = '12345'   num_last_chars = 5 expected = '12345' ).
  ENDMETHOD.
ENDCLASS.
```
### TDF - Test Double Framework

The Test Double Framework (TDF for short) provides tools to create so-called doubles for the objects class, table and core data service (CDS) at test runtime to test against them.

**General:**

The SQL Double has the task to replace the database layer and thus to replace all queries (CRUD) to one or more tables by a corresponding double. The tables can be specified during creation.

1. normal test scenario

`Test Class` &rarr; `Code Under Test` &rarr; `Database Table`

2. test scenario where table will be replaced by a double

`Test Class` &rarr; `Code Under Test` &rarr; `Double`

`Test Class` &rarr; `Double`

The double replaces the data in the table. All CRUD operations run against the double.

**Data exchange:**

However, we do not exchange the data directly on the database, these are available at any time and remain consistent. The generated test double is available to us for our test class and is removed again after execution.

The advantage of this technique is the stability of our test cases, because we cannot be sure that our test data on the database will not change. With this technique, data for our test always remain the same and you don't have to outsource or even adapt the accesses to the database to other classes.

**Example:**

In this example we want to replace the tables `LFB1` and `KNB1` with doubles.

**Definition:**
```abap
PRIVATE SECTION.
	CLASS-DATA:
		"! <p class="shorttext synchronized">Interface to use SQL fakes / Test doubles</p>
		go_environment TYPE REF TO if_osql_test_environment.
```

```abap
CLASS-METHODS: class_setup.
CLASS-METHODS: class_teardown.
```

**Implementation:**

```abap
METHOD class_setup.

	DATA: lt_lfb1 TYPE STANDARD TABLE OF lfb1 WITH EMPTY KEY,
	  lt_knb1 TYPE STANDARD TABLE OF knb1 WITH EMPTY KEY.

	go_environment = cl_osql_test_environment=>create( VALUE #( ( 'LFB1' )
								( 'KNB1' ) ) ).

	lt_lfb1 = VALUE #(
			( mandt = sy-mandt lifnr = '230002' bukrs = '0001' intad = 'test@test.com')
		     ).
	lt_knb1 = VALUE #(
			( mandt = sy-mandt kunnr = '220002' bukrs = '0001' intad = 'test@test.com')
		     ).

	go_environment->insert_test_data( lt_lfb1 ).
	go_environment->insert_test_data( lt_knb1 ).
ENDMETHOD.
```

```abap
METHOD class_teardown.

	go_environment->destroy( ).

ENDMETHOD.
```

* In the setup method, the SQL environment is built first and the tables to be exchanged are specified.
* Transfer of the test data to the environment
* Execution of the test cases against the test data
* Dismantling of the environment when the test class is dismantled

### How to implement Unit Tests to a global Class in Eclipse?

Declare Test Classes: 
![declare_test_class](https://user-images.githubusercontent.com/30869493/116395736-cc037600-a824-11eb-88cb-d40260edc36a.png)

### How to test private methods?

To test private methods of the class we can use the `LOCAL FRIENDS` declaration.

```abap
CLASS zcl_class_test DEFINITION DEFERRED.
CLASS zcl_class DEFINITION LOCAL FRIENDS zcl_class_test.

CLASS zcl_class_test DEFINITION FOR TESTING
[...]
PRIVATE SECTION.
	DATA:
		zcl_class TYPE REF TO zcl_class.  "class under test
```

```abap
METHOD setup.
	CREATE OBJECT zcl_fi_edi_payext_2441.
ENDMETHOD.
```

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

## ABAP daemon

A daemon is a utility program that runs continuously by itself and without the control of an interactive user on a multi-tasking operating system in the background to monitor and take care of certain subsystems or show immediate reaction to events. Daemons perform explicit actions at predefined times or in response to certain events.

Superclass: `CL_ABAP_DAEMON_EXT_BASE`

Full sample programm see [here](./docs/zcl_tutorial_simple_daemon.abap).

**How to start the deamon in another program?**

```abap
REPORT z_tutorial_simple_daemon_start.

zcl_tutorial_simple_daemon=>start( iv_daemon_name = 'simple_daemon' iv_timeout = 10000 ).

DATA(lv_text) = `This is a simple ABAP Daemon message sent via PCP.`.
zcl_tutorial_simple_daemon=>send( iv_daemon_name = 'simple_daemon' iv_text = lv_text ).

zcl_tutorial_simple_daemon=>stop( iv_daemon_name = 'simple_daemon' ).
```

You can monitor all running ABAP Daemons using transaction `SMDAEMON` in SAPGUI. There you can see their state, check for errors, and also restart and terminate them.

To stop your daemon, select it from the list and go to `ABAP Daemon` > `Terminate Daemon`. Alternatively, you can also create a static STOP method.

## Sample Programs

### Change Language without new Login to SAP GUI

```abap
REPORT z_change_langu.

DATA: server  TYPE          msxxlist,
      servers TYPE TABLE OF msxxlist.

PARAMETERS: language LIKE sy-langu MATCHCODE OBJECT h_t002
                                  OBLIGATORY
                                  DEFAULT sy-langu.

START-OF-SELECTION.

* initialize internal table for server list 
  CLEAR: servers[].

* get server list
  CALL FUNCTION 'TH_SERVER_LIST'
    TABLES
      list           = servers
    EXCEPTIONS
      no_server_list = 1
      OTHERS         = 2.

* process server list
  LOOP AT servers INTO  server
                    WHERE host = sy-host.

* set new language
    SET LOCALE LANGUAGE language.

* start remote transaction
    CALL FUNCTION 'TH_REMOTE_TRANSACTION'
      EXPORTING
        tcode = space
        dest  = server-name.

* exit loop
    EXIT.

  ENDLOOP.

* leave program
  LEAVE PROGRAM.
```

### Credit Management in S/4HANA

```abap
PARAMETERS: p_part TYPE bu_partner,
            p_bukrs TYPE bukrs.

DATA: gt_totals TYPE TABLE OF ukm_totals,

      gv_limit  TYPE ukm_credit_limit,
      gv_waers  TYPE ukm_sgm_currency.

SELECT SINGLE FROM ukm_kkber2sgm
  INNER JOIN t001
    ON t001~kkber = ukm_kkber2sgm~kkber
  FIELDS ukm_kkber2sgm~credit_sgmnt
  WHERE t001~bukrs = @p_bukrs
  INTO @DATA(gv_credit_sgmnt).

IF sy-subrc = 0.

  CALL FUNCTION 'UKM_GET_COMMTS_RULEBASED'
    EXPORTING
      i_partner         = p_part"'' "Business Partner
      i_segment         = gv_credit_sgmnt
    IMPORTING
      e_credit_limit    = gv_limit
      e_currency        = gv_waers
    EXCEPTIONS
      partner_not_found = 1
      segment_not_found = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
  ENDIF.

  CALL FUNCTION 'UKM_COMMTS_READ'
    EXPORTING
      i_partner     = p_part"'' "Business Partner
      i_segment     = gv_credit_sgmnt
      i_date        = '99991231' "Date
    TABLES
      et_ukm_totals = gt_totals.

* GT_TOTALS-COMM_TYP
* 100 = Offene Aufträge   - SD_CREDIT_EXPOSURE -> OPEN_ORDER
* 200 = Offene Rechnungen = KNKK-SKFOR
* 300 = Sonderobligo      = KNKK-SSOBL
* 400 = Lieferwert        = SD_CREDIT_EXPOSURE -> OPEN_DELIVERY
* 500 = Fakturawert       = SD_CREDIT_EXPOSURE -> OPEN_INVOICE

ENDIF.
```

#### Get values of UKM_S_ADD_INFO

![image](https://user-images.githubusercontent.com/30869493/129559125-d26f2275-b5e9-4e0c-95fa-46ea06231fa1.png)

```abap
DATA: lr_mo       TYPE REF TO   fsbp_memory_object,
      lt_bp3100   TYPE TABLE OF bp3100,
      ls_bp3100   LIKE LINE  OF lt_bp3100,
      ls_add_info TYPE          ukm_s_add_info.

  lr_mo ?= fsbp_memory_factory=>get_instance(
             i_partner    = p_part
             i_table_name = if_fsbp_const_xo_objects=>mo_bp3100 ).
  lr_mo->get_data_new( IMPORTING e_data_new = lt_bp3100 ).

  LOOP AT lt_bp3100 INTO ls_bp3100.
    IF ls_bp3100-addtype = '10'.
        DATA(ammount) = ls_bp3100-amnt.
    ENDIF.
  ENDLOOP.
```

### Import Excel into internal Table

See [here](docs/z_import_excel_into_table.abap).

### ABAP Extended Expressions

* Run transaction `SE38`
* `DEMO_EXPRESSIONS` -> <kbd>F8</kbd>

![DEMO_EXPRESSIONS](https://user-images.githubusercontent.com/30869493/133969858-3037542b-f9d4-45bf-948b-164281b7c712.png)


## Sample Classes

### Check for System ID / System

```abap
CLASS zcl_bc_sysid_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized">SYSID PS4 (Productive)</p>
    CONSTANTS sysid_ps4 TYPE syst_sysid VALUE 'PS4' ##NO_TEXT.

    "! <p class="shorttext synchronized">SYSID TS4 (Test)</p>
    CONSTANTS sysid_ts4 TYPE syst_sysid VALUE 'TS4' ##NO_TEXT.

    "! <p class="shorttext synchronized">SYSID DS4 (Development)</p>
    CONSTANTS sysid_ds4 TYPE syst_sysid VALUE 'DS4' ##NO_TEXT.


    CLASS-METHODS:
      "! <p class="shorttext synchronized">Returns, if system is productive system</p>
      "! @parameter i_sysid | <p class="shorttext synchronized">System ID</p>
      "! @parameter r_productive | <p class="shorttext synchronized">Is productive system?</p>
      is_productive_system
        IMPORTING
          i_sysid             TYPE syst_sysid DEFAULT sy-sysid
        RETURNING
          VALUE(r_productive) TYPE abap_bool.

    CLASS-METHODS:
      "! <p class="shorttext synchronized">Returns, if system is test system</p>
      "! @parameter i_sysid | <p class="shorttext synchronized">System ID</p>
      "! @parameter r_test | <p class="shorttext synchronized">Is test system?</p>
      is_test_system
        IMPORTING
          i_sysid       TYPE syst_sysid DEFAULT sy-sysid
        RETURNING
          VALUE(r_test) TYPE abap_bool.

    CLASS-METHODS:
      "! <p class="shorttext synchronized">Returns, if system is development system</p>
      "! @parameter i_sysid | <p class="shorttext synchronized">System ID</p>
      "! @parameter r_development | <p class="shorttext synchronized">Is development system?</p>
      is_development_system
        IMPORTING
          i_sysid              TYPE syst_sysid DEFAULT sy-sysid
        RETURNING
          VALUE(r_development) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_bc_sysid_tools IMPLEMENTATION.

  METHOD is_productive_system.

    r_productive = xsdbool( i_sysid = sysid_ps4 ).

  ENDMETHOD. "is_productive_system


  METHOD is_test_system.

    r_test = xsdbool( i_sysid = sysid_ts4 ).

  ENDMETHOD. "is_test_system


  METHOD is_development_system.

    r_development = xsdbool( i_sysid = sysid_ds4 ).

  ENDMETHOD. "is_development_system

ENDCLASS.
```

> **Note:** `##NO_TEXT` is a pragmas for hiding warnings

### ABAP Hot Expresions

```abap
SELECT *
 FROM scarr
 WHERE carrid <> @( VALUE #( ) )
 INTO TABLE @DATA(result).
```

## Sample Functions

This function will popup an alert window for a given User with a given text.

![TH_POPUP](https://user-images.githubusercontent.com/30869493/133611044-1bc87866-5289-4e19-8c7c-8edd9adcecb7.png)

```abap
CALL FUNCTION 'TH_POPUP'
  EXPORTING
    client         = 
    user           = 
    message        = 
*    message_len    = 0
*    cut_blanks     = ' '
*  EXCEPTIONS
*    user_not_found = 1
*    others         = 2
  .
IF SY-SUBRC <> 0.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
```

# SAP

## SAP Gui Shortcuts

| shortcut | what it does |
|----|----|
| <kbd>str</kbd> + <kbd>shift</kbd> + <kbd>7</kbd> | focus on execution bar |
| <kbd>str</kbd> + <kbd>N</kbd> | open new window |
| <kbd>str</kbd> + <kbd>Y</kbd> | mark in SAP Gui |

## Transactions (prefix)

| shortcut | what it does |
|----|----|
| `/n` | execute transaction in current window |
| `/o` | execute transaction in a new window |
| `/i` | closes the current window |
| `/nex` | closses all windows and log off from the SAP System |
| `/*` | execute transaction with history data |
| `ANST` | Find all the Enhancements that are Implemented for a Transaction Code in SAP |
| `CODE_SCANNER` | Check for Code |
| `S_BCE_68001398` | Check wether a user have authorization to use a transaction or not |
| `SAT` | Runtime Analysis |
| `SCC4` | Open System for Customizing | 
| `SE93` | Maintain transactions |
| `SEARCH_SAP_MENU` | search though the SAP Menu |
| `SM04` | End User Sessions |
| `STMS` | Transport Management System (check if a transport went into the system) |

## Customizing transactions/reports

| shortcut | what it does |
|----|----|
| `OBB8` | Maintain Payment terms |
| `FBMP` | Dunning |
| `FBZP` | Payment run |
| `RFVITXBA` (Report) | Transport SO10 text |
| `VKOA` | Accountfinding |

## SAP Tables
| table | what it does |
|----|----|
| ABLM_BLACKLIST | Blacklist Items of Executables from Applicaitons |
| ACDOCA | Universal Journal Entry Line Items |
| T001 | Company Codes |
| TSTC | SAP Transaction Codes |

## Edit SAP Table entries

### SE16

- run transaction `SE16`
- Enter Table
- Fill selection screen -> <kbd>F8</kbd>
- Select Table entries
- "View" with the glasses
- start debugger with `/h` in command line
- click in the relevant field and hit <kbd>ENTER</kbd>
- Aktivate Variables
  - EDIT -> Modify line
  - INSR -> Insert line
  - DELE -> Delete line

### SE16N

- run transaction `SE16N`
- Enter Table
- `/h`
- in the variables fill variables and change the value to `x` with the pen icon
  - gd-edit -> x
  - gd-sapedit -> x
  
  ![se16n debugger variables](https://user-images.githubusercontent.com/30869493/124892270-041ff580-dfda-11eb-81c8-fa1359b1bba4.png)

- <kbd>ENTER</kbd> -> <kbd>F8</kbd>

## Transport SAP Table entries

- run transaction `SE37`
- Execute Function Module `SE16N_INTERFACE`
  - I_TAB -> set table
	- I_EDIT -> x
	- I_SAPEDIT -> x
  - I_DISPLAY-> x
- <kbd>ENTER</kbd> -> <kbd>F8</kbd>

Select the enteries you want to transport
-> `Table Entry` -> `Tranport`
![image](https://user-images.githubusercontent.com/30869493/126965425-8a5e7477-4f25-46a8-ae55-2825a648a473.png)

## Debug Payment Run

1. Set a Break-Point in the program / Function module
2. Start the Payment Proposal Run in F110 without "Start immediately", for example using start time 12pm
3. go to the Job Overview page using transaction `SM37` / `SMX` -> mark the F110 Job -> enter `JDBG` in the execution bar, this will start the Job
4. It should stop at the Break-Point

## Find BADIs for an excuted transaction -> `CL_EXITHANDLER`

* Go to transaction `SE80` Class `CL_EXITHANDLER`
* navigate to method `get_instance` and set a Break-Point in the first line of `cl_exithandler=>get_class_name_by_interface`
* Use the transaction in which you are BAdI-hunting
* Examine the contents of the field exit_name whenever the processing stops at the breakpoint. I have found a case where exit_name was an unknown field. Then class_name gave a good clue to the name of the BAdI.
