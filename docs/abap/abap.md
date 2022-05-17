# ABAP

**A**dvanced **B**usiness **A**pplication **P**rogramming

For getting started you can checkout the [Hello World](/abap/hello_world.md)-snippet.

For more references when coding in ABAP you can checkout the [ABAP Styleguides](https://github.com/SAP/styleguides).



### Overwrite BP also when containing an error

```abap
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
  EXCEPTIONS
    error_message = 1.
```

### cl_abap_context_info

Collected information about the user and the system was previously only obtained from the SYST structure.

```abap
cl_abap_context_info=>
```
For example:

| New Method | Old Environemnt |
| ---------- | -------------- |
| ET_SYSTEM_DATE | SYST-DATUM |
| GET_SYSTEM_TIME | SYST-UZEIT |
| GET_USER_TECHNICAL_NAME | SYST-UNAME |
| GET_USER_LANGUAGE_ABAP_FORMAT | SYST-LANGU |
| GET_USER_LANGUAGE_ISO_FORMAT | ISO format for language |
| GET_USER_TIME_ZONE | SYST-TZONE |
| GET_USER_ALIAS | Alias of user master data (Cloud) |
| GET_SYSTEM_URL | URL of the system (Cloud) |
| GET_USER_FORMATTED_NAME | BAPI_USER_GET_DETAIL |
| GET_USER_DESCRIPTION | BAPI_USER_GET_DETAIL |
| GET_USER_BUSINESS_PARTNER_ID | Business-Partner-ID of the user (Cloud) |

### Set additional credit management data in S/4HANA

See [zfi_change_ukmbp_data](abap/samples/zfi_change_ukmbp_data.md).

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

### MODIFY table FROM line TRANSPORTING x WHERE x = y

Modify all lines excluding one

```abap
DATA: t_pbank_line TYPE f110_pbank.
t_pbank_line-xstda = ''.
MODIFY t_pbank[] FROM t_pbank_line TRANSPORTING xstda WHERE bvtyp <> i_waers.
```

### replace characters

```abap
    "! <p class="shorttext synchronized">Replace characters in a given string</p>
    "! <p class="shorttext synchronized">Please note that strings should be placed in `` and not ''</p>
    "! @parameter input | <p class="shorttext synchronized">Input string</p>
    "! @parameter characters | <p class="shorttext synchronized">Characters which should be replaced</p>
    "! @parameter replace_character | <p class="shorttext synchronized">Character with which the given characters are replaced</p>
    "! @parameter result | <p class="shorttext synchronized">Modified string</p>
    METHODS replace_chars
      IMPORTING
                input             TYPE string
                characters        TYPE string
                replace_character TYPE string
      RETURNING VALUE(result)     TYPE string.
```

```abap
  METHOD replace_chars.

    result = input.
    DO strlen( characters ) TIMES.
      DATA(counter) = sy-index - 1.
      REPLACE ALL OCCURRENCES OF characters+counter(1) IN result WITH replace_character.
    ENDDO.

  ENDMETHOD.
```

or

```abap
replace( val = <lv_idocfeld> regex = '\&|\+' with = `` occ = 0 ).
```

### The SWITCH Statement as a Replacement for CASE

How many times have you seen code like that? Here you’re getting one value and using a CASE statement to translate that value. The problem is that you need to keep mentioning what variable you’re filling in every branch of your CASE statement.

```abap
* Use adapter pattern to translate human readable CRUD
* standard values to the BOPF equivalent
DATA: bopf_edit_mode TYPE /bobf/conf_edit_mode.
CASE id_edit_mode.
 WHEN 'R'."Read
   bopf_edit_mode = /bobf/if_conf_c=>sc_edit_read_only.
 WHEN 'U'."Update
   bopf_edit_mode = /bobf/if_conf_c=>sc_edit_exclusive.
 WHEN OTHERS.
     "Unexpected Situation
     RAISE EXCEPTION TYPE zcx_4_monster_exceptions.
ENDCASE.
```

As mentioned in the code, this is the adapter pattern, very common in OO programming. In 7.4, this can be slightly simplified by using the new SWITCH constructor operator, as shown here.

```abap
* Use adapter pattern to translate human readable CRUD
* standard values to the BOPF equivalent
DATA(bopf_edit_mode) =
SWITCH /bobf/conf_edit_mode( id_edit_mode
WHEN 'R' THEN /bobf/if_conf_c=>sc_edit_read_only "Read
WHEN 'U' THEN /bobf/if_conf_c=>sc_edit_exclusive "Update
ELSE THROW zcx_4_monster_exceptions( ) ). "Unexpected
 ```

As you can see from this example, the data definition for BOPF_EDIT_MODE (/bobf/conf_edit_mode in this case) has moved into the body of the expression, thus dramatically reducing the lines of code needed. In addition, Java fans will jump up and down with joy to see that instead of the ABAP term RAISE EXCEPTION TYPE we now have the equivalent Java term, THROW. The usage is identical, however; the compiler evaluates the keywords RAISE EXCEPTION TYPE and THROW as if they were one and the same. As an added bonus, this actually makes more grammatical sense, because THROW and CATCH go together better than RAISE EXCEPTION TYPE and CATCH. (It’s lucky that exception classes have to start with CX; otherwise some witty programmer at SAP would create an exception class called UP.)

It’s important to note that the values in the WHEN statements have to be constants, as in the preceding example. If you put something like MONSTER->HEAD_COUNT after the WHEN statement, then the SWITCH statement as a whole explodes and gives an incorrect error message saying “HEAD_COUNT” is unknown, when what it really means is that MONSTER->HEAD_COUNT isn’t a constant. If you need a WHEN statement with variables, you have to use the COND statement described in the next section.

 

To move away from monsters for a second, as painful as that is, here’s an example of combing two new ABAP constructs together. Let’s say you wanted to merge some values of different lengths into a uniform format. In standard SAP, table VBPA is a good example. It has customer (KUNNR) values, which are 10 characters long, and personnel number (PERNR) values, which are eight characters long. In the listing below, we fill new table LT_VAKPA with the data from VBPA, except the KUNDE field will always come out as a 10-character field no matter if a customer number or personnel number was in VBPA.

 
```abap
LOOP AT lt_vbpa ASSIGNING FIELD-SYMBOL(<ls_vbpa>).
INSERT VALUE #(
vbeln = <ls_vbpa>-vbeln
parvw = <ls_vbpa>-parvw
kunde = SWITCH #( <ls_vbpa>-parvw
              WHEN 'AG' OR 'WE' OR 'RG' OR 'RE'
              THEN <ls_vbpa>-kunnr
              ELSE '00' && <ls_vbpa>-pernr )
adrnr = <ls_vbpa>-adrnr )
INTO TABLE gt_vakpa.

ENDLOOP.
```

https://blog.sap-press.com/conditional-logic-in-abap