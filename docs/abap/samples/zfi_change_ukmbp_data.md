## zfi_change_ukmbp_data.abap

```abap
REPORT zfi_change_ukmbp_data.

PARAMETERS: p_partn TYPE bu_partner OBLIGATORY.

SELECT SINGLE FROM but000
  FIELDS partner,
         partner_guid
  WHERE partner = @p_partn
  INTO @DATA(ls_but000).


DATA: lr_mo       TYPE REF TO   fsbp_memory_object,
      lt_bp3100   TYPE TABLE OF bp3100,
      ls_bp3100   TYPE bp3100.

"read BP3100 data from memory instance
lr_mo ?= fsbp_memory_factory=>get_instance(
           i_partner    = ls_but000-partner
           i_table_name = if_fsbp_const_xo_objects=>mo_bp3100 ).
           
lr_mo->get_data_new( IMPORTING e_data_new = lt_bp3100 ).

ls_bp3100-mandt = '100'.
ls_bp3100-partner = ls_but000-partner.
ls_bp3100-criter = '3700'.
ls_bp3100-amnt = '1000'.
ls_bp3100-curr = 'EUR'.
ls_bp3100-addtype = '10'.
ls_bp3100-data_type = '10'.

APPEND ls_bp3100 TO lt_bp3100.

lr_mo->set_data_new( i_data_new = lt_bp3100 ).
lr_mo->validate( ).
lr_mo->save_data( ).


CALL FUNCTION 'FS_API_EXPL_SAVE'.
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
```