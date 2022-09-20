## TVARVC - Table of Variant Variables (Client-Specific)

Some programs are only supporting the selection variable T (T: Table Variable from TVARVC) when saving a variant for given programs.

For those kind of programs following TVARVC-Variables should be implemented in the system.

For example:

- Z_FI_CURRENT_YEAR
- Z_FI_CURRENT_MONTH
- U_FI_PREVIOUS_YEAR
- Z_FI_PREVIOUS_MONTH

These variables can be used in the variant selection as followed.

![image](https://user-images.githubusercontent.com/30869493/191276823-e9286e25-214a-4d13-8174-7926cc530855.png)

Since the variables in the table are static entries, they should be updated periodically using the following program.

```abap
REPORT zfi_update_tvarvc.

DATA: previous_month TYPE syst_datum.
CALL FUNCTION 'OIL_GET_PREV_MONTH'
  EXPORTING
    i_date = sy-datum
  IMPORTING
    e_date = previous_month.

SELECT SINGLE * FROM tvarvc WHERE name = 'Z_FI_CURRENT_YEAR' AND type = 'P' INTO @DATA(tvarvc_line).
UPDATE tvarvc FROM @( VALUE #(  BASE tvarvc_line low = sy-datum+0(4) ) ).
CLEAR tvarvc_line.

SELECT SINGLE * FROM tvarvc WHERE name = 'Z_FI_CURRENT_MONTH' AND type = 'P' INTO @tvarvc_line.
UPDATE tvarvc FROM @( VALUE #( BASE tvarvc_line low = sy-datum+4(2) ) ).
CLEAR tvarvc_line.

SELECT SINGLE * FROM tvarvc WHERE name = 'Z_FI_YEAR_OF_PREVIOUS_MONTH' AND type = 'P' INTO @tvarvc_line.
UPDATE tvarvc FROM @( VALUE #(  BASE tvarvc_line low = previous_month+0(4) ) ).
CLEAR tvarvc_line.

SELECT SINGLE * FROM tvarvc WHERE name = 'Z_FI_PREVIOUS_MONTH' AND type = 'P' INTO @tvarvc_line.
UPDATE tvarvc FROM @( VALUE #( BASE tvarvc_line low = previous_month+4(2) ) ).
CLEAR tvarvc_line.
```

Further variables can easily be added using following syntax:

```abap
SELECT SINGLE * FROM tvarvc WHERE name = 'variable_name' AND type = 'P' INTO @tvarvc_line.
UPDATE tvarvc FROM @( VALUE #( BASE tvarvc_line low = xxx ) ).
```
**Warning:** Please make sure to maintain the variables in transaction **STVARV** in each system.
