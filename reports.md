```abap
REPORT z_dec4_to_dec2.

DATA: dec2 TYPE p DECIMALS 2,
      dec4 TYPE p DECIMALS 4.

dec2 = 0.
dec4 = '23.1234'.

dec2 = dec4 - ( frac( dec4 * 100 ) / 100 ).

WRITE dec2.
```

```abap
*                     \PR:SAPLACEPS3\FO:FILL_PS_LINE_ITEM_PER
*** AA 17.05.2019 lang,j.: Manuelle Abgrenzung
DATA: LV_DATUM TYPE DATS.
DATA: LV_REFKEY Type ACE_OBJ_ID. " 6500000009 schmelzer,t.

MOVE IS_ITEM-LASTEFFDATE TO LV_DATUM.

SELECT SINGLE REF_KEY FROM ACESOBJ INTO LV_REFKEY WHERE OBJID EQ IS_ITEM-OBJID. " 6500000009 schmelzer,t. Get REF_KEY to select in next step from ACESOBJ_ASSGMT
SELECT SINGLE SGTXT FROM ACESOBJ_ASSGMT INTO CS_DOCITEM-SGTXT WHERE COMP EQ IS_ITEM-COMP AND REF_KEY EQ LV_REFKEY AND DATE_TO > LV_DATUM. " 6500000009 schmelzer,t.
*SELECT SINGLE SGTXT FROM ACESOBJ_ASSGMT INTO CS_DOCITEM-SGTXT WHERE COMP EQ IS_ITEM-COMP AND OBJID EQ IS_ITEM-OBJID AND DATE_TO > LV_DATUM.
```

```abap
REPORT z_write_options.

DATA: text TYPE string VALUE '0123456789ABCDEF',
      col  TYPE i VALUE 25,
      len  TYPE i VALUE 5.

WRITE text.
WRITE /5(16) text.
WRITE AT col(len) text.
```

```abap
REPORT z_select_demo_delete.

SELECT * FROM spfli
INTO TABLE @DATA(result).

DELETE result WHERE carrid = 'LH'.
cl_demo_output=>display_data( result ).
```

```abap
REPORT z_excel_download.

CONSTANTS: lc_separ VALUE ';'. "separator

DATA: lt_output TYPE TABLE OF spfli WITH HEADER LINE,
      lt_iout   TYPE TABLE OF string,
      ls_xout   TYPE string,
      ls_str    TYPE string. "convert P to string, otherwise concatenate is not possible
FIELD-SYMBOLS: <fs> TYPE any.

SELECT * INTO TABLE lt_output FROM spfli.

LOOP AT lt_output.
  CLEAR ls_xout.
  DO.
    ASSIGN COMPONENT sy-index OF STRUCTURE lt_output TO <fs>.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    IF sy-index = 1.
      ls_str = <fs>.
      ls_xout = ls_str.
    ELSE.
      ls_str = <fs>.
      CONCATENATE ls_xout ls_str INTO ls_xout SEPARATED BY lc_separ.
    ENDIF.
  ENDDO.
  APPEND ls_xout TO lt_iout.
ENDLOOP.
DATA: lv_action   TYPE i,
      lv_filename TYPE string,
      lv_fullpath TYPE string,
      lv_path     TYPE string.
*Save dialog
cl_gui_frontend_services=>file_save_dialog( EXPORTING
                                              default_file_name = 'Excel.csv'
                                              default_extension = 'csv'
                                            CHANGING
                                              filename          = lv_filename
                                              path              = lv_path
                                              fullpath          = lv_fullpath
                                              user_action       = lv_action ).

cl_gui_frontend_services=>gui_download( EXPORTING filename     = lv_fullpath
                                        CHANGING  data_tab     = lt_iout ).
LEAVE SCREEN.
```

```abap
REPORT z_alv_deb_salesdoc.


*&---------------------------------------------------------------------*
*& TABLES
*&---------------------------------------------------------------------*

TABLES: kna1, vbak. "announces the tables

*&---------------------------------------------------------------------*
*& CLASS
*&---------------------------------------------------------------------*

CLASS localclass DEFINITION.

  PUBLIC SECTION.
    "GET_KUNNR
    METHODS: get_kunnr
      IMPORTING
        ip_kunnr TYPE kunnr.
  PRIVATE SECTION.
    "SHOW
    METHODS: show. "show ALV-Grid

    DATA: lt_vbak TYPE TABLE OF vbak.
*    DATA: ls_vbak TYPE vbak.

ENDCLASS.

CLASS localclass IMPLEMENTATION.

  "GET_KUNNR
  METHOD get_kunnr.
    SELECT * "'*' no good programming
    FROM vbak
    INTO TABLE lt_vbak "INTO TABLE -> for internal table
    WHERE kunnr = ip_kunnr.

*    SELECT SINGLE kunnr, vbeln
*    FROM vbak
*    INTO @DATA(ls_kunden) "internal table (lt) internal structure (ls) | SINGLE -> structure
**    INTO TABLE @lt_kna1
*    WHERE kunnr = @ip_kunnr.

    IF sy-subrc = 0. "checks if database access worked
      me->show( ). "call method show | with 'me' you cann call methods within the class
    ELSE.
      WRITE: / TEXT-t02.
    ENDIF.
  ENDMETHOD.

  "SHOW
  METHOD show.
    DATA: lo_alv       TYPE REF TO cl_salv_table,
          lo_functions TYPE REF TO cl_salv_functions_list,
          lo_columns   TYPE REF TO cl_salv_columns_table,
          lo_display   TYPE REF TO cl_salv_display_settings.
*   create instance of class 'cl_salv_table'
    TRY.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = lo_alv
          CHANGING  t_table = lt_vbak ). "here he needs the table
      CATCH cx_salv_msg.
    ENDTRY.

*   function keys (sorting, filtering, Excel export, etc.)
    lo_functions = lo_alv->get_functions( ).
    lo_functions->set_all( abap_true ).

*   optimal column width
    lo_columns = lo_alv->get_columns( ).
    lo_columns->set_optimize( abap_true ).

*   title and / or stripe pattern
    lo_display = lo_alv->get_display_settings( ).
    lo_display->set_list_header( value = TEXT-t01 ). "title | textsymbol
    lo_display->set_striped_pattern( abap_true ).

*   show list
    lo_alv->display( ).
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& SELECTION_SCREEN
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_kunnr TYPE kunnr OBLIGATORY. "OBLIGATORY -> required field
*  SELECT-OPTIONS: p_kunnr FOR KNA1-kunnr. "after FOR first table than field connected with '-'

SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  DATA: lref_local TYPE REF TO localclass. "lref -> local Reference

  CREATE OBJECT lref_local. "create object

  lref_local->get_kunnr( ip_kunnr =  p_kunnr ). "call method
  "between braces method call parameter
  "a global method you can call with '=>'
```
