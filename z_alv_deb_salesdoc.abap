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
