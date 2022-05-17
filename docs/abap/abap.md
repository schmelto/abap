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

## MODIFY table FROM line TRANSPORTING x WHERE x = y

Modify all lines excluding one

```abap
DATA: t_pbank_line TYPE f110_pbank.
t_pbank_line-xstda = ''.
MODIFY t_pbank[] FROM t_pbank_line TRANSPORTING xstda WHERE bvtyp <> i_waers.
```