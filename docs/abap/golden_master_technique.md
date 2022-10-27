## Golden Master Technique

```mermaid
graph LR
    A[Generate Input] --> B[Snapshot Output]
    B --> C[check Coverage]
    C --> D[test Tests]

```
```mermaid
graph LR
    E[make adjustements] --> F[new Snapshot Output]
    F --> G[Compare]
```

### The simplest case

![image](https://user-images.githubusercontent.com/30869493/198228486-f315c565-a20c-45d4-afbf-3bbc6705f0f5.png)

Coverage Analysis with transaction **SCOV**
![image](https://user-images.githubusercontent.com/30869493/198228631-14c022de-f2ed-43c8-8e1a-68fbcc87069e.png)
![image](https://user-images.githubusercontent.com/30869493/198228680-652ca97f-7fba-4abb-a517-5ba5b91752a2.png)
![image](https://user-images.githubusercontent.com/30869493/198228741-2b6f7879-70ec-41cf-bfee-6c27fa09938e.png)

Now the screenshotet outputs could be compared with [WinMerge](https://winmerge.org/).
![image](https://user-images.githubusercontent.com/30869493/198228952-a9c97ce4-887a-4fe5-be69-34ea55fae551.png)

### Automation with ABAP Unit and classic reports
Testing isolated form with return

```abap
REPORT zunit_test_forms.

START-OF-SELECTION.
  DATA: summe TYPE int4.
  PERFORM addiere USING 1 2 CHANGING summe.

FORM addiere USING sum1 sum2 CHANGING summe.
  summe = sum1 + sum2.
ENDFORM.

CLASS addierer_tests DEFINITION FINAL FOR TESTING
RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS: akz_add_1_and_2_equals_3 FOR TETING RAISING cx_static_check.
ENDCLASS.

CLASS addiere_tests IMPLEMENTATION.
  METHOD akz_add_1_and_2_equals_3.
    DATA: sum_actual TYPE int4.
    
    PERFORM addiere IN PROGRAM zunit_test_forms USING 1 2 CHANGING sum_actual.
    
    cl_abap_unit_assert=>assert_equals( exp = 3 act = sum_actual ).
  ENDMETHOD.
ENDCLASS.

```
### Automation with ABAP Unit and classic reports
Testing local class with return

```abap
CLASS addierer DEFINITION.
  PUBLIC SECTION.
    METHODS addiere IMPORTING sum1 TYPE int4
                              sum2 TYPE int4
                    RETURNING VALUE(summe) TYPE int4.
ENDCLASS.

CLASS addierer IMPLEMENTATION.
  METHOD addiere.
    summe = sum1 + sum2.
  ENDMETHOD.
ENDLCASS.

START-OF-SELECTION.
  DATA(addierer) = NEW addierer().
  DATA(summe) = addierer->addiere(
                            sum1 = 1
                            sum2 = 2 ).
  WRITE summe.

CLASS addierer_tests DEFINITION FINAL FOR TESTING
RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS: akz_add_1_and_2_equals_3 FOR TETING RAISING cx_static_check.
ENDCLASS.

CLASS addiere_tests IMPLEMENTATION.
  METHOD akz_add_1_and_2_equals_3.
    DATA(sum_actual) = addierer->addiere(
                                  sum1 = 1
                                  sum2 = 2 ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = sum_actual ).
  ENDMETHOD.
ENDCLASS.
```
