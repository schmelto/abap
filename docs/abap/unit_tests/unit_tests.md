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
