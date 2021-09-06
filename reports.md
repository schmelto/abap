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

## How to find the Table that stores multiple Field values?

![image](https://user-images.githubusercontent.com/30869493/109816607-3935c900-7c31-11eb-80e1-d25cb74ff6fd.png)

![image](https://user-images.githubusercontent.com/30869493/109816742-5b2f4b80-7c31-11eb-8fdb-3b614acd080a.png)


```abap
TABLES: DD03L,
        BOOLE.

DATA:DREF TYPE REF TO DATA.
FIELD-SYMBOLS:<TABLE> TYPE STANDARD TABLE.
TYPE-POOLS: SLIS, ICON.

* Internal Tables
TYPES: BEGIN OF T_IALV,
         TABNAME TYPE DD03L-TABNAME,
         FIELD1  TYPE DD03L-FIELDNAME,
         FIELD2  TYPE DD03L-FIELDNAME,
         FIELD3  TYPE DD03L-FIELDNAME,
         FIELD4  TYPE DD03L-FIELDNAME,
       END OF T_IALV .

DATA: IALV   TYPE STANDARD TABLE OF T_IALV,
      WA_ALV TYPE T_IALV.

DATA: ALV_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      ALV_GRID      TYPE REF TO CL_GUI_ALV_GRID,
      OK_CODE       LIKE SY-UCOMM,
      FIELDCAT      TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.
SELECT-OPTIONS:S_FIELD1 FOR DD03L-FIELDNAME NO-EXTENSION,
S_FIELD2 FOR DD03L-FIELDNAME NO-EXTENSION,
S_FIELD3 FOR DD03L-FIELDNAME NO-EXTENSION,
S_FIELD4 FOR DD03L-FIELDNAME NO-EXTENSION,
S_EXC FOR BOOLE-BOOLE.

FIELD-SYMBOLS:<FS-TAB13> TYPE MANDT.
DATA:WA_DD1      TYPE DD03L,
     WA_DD2      TYPE DD03L,
     WA_DD3      TYPE DD03L,
     WA_DD4      TYPE DD03L,
     LV_TAB      TYPE DD03L-TABNAME,
     WA_DD_FINAL TYPE DD03L.
IF S_EXC-LOW IS NOT INITIAL.
  LV_TAB = '/%'.
ENDIF.
IF S_FIELD1-LOW IS NOT INITIAL.
  SELECT C~TABNAME
    FROM ( ( DD03L AS C
         INNER JOIN DD02V AS P ON P~TABNAME  = C~TABNAME
                              AND P~TABCLASS = 'TRANSP')
*                              AND p~cityto   = @cityto )
         INNER JOIN DD09L AS F ON F~TABNAME = P~TABNAME )
    WHERE ( ( C~ROLLNAME EQ @S_FIELD1-LOW )
    OR ( C~FIELDNAME EQ @S_FIELD1-LOW  ) )
    AND P~TABNAME NOT LIKE @LV_TAB
       INTO TABLE @DATA(ITAB13).
ENDIF.
IF S_FIELD2-LOW IS NOT INITIAL.
  SELECT C~TABNAME
  FROM ( ( DD03L AS C
       INNER JOIN DD02V AS P ON P~TABNAME  = C~TABNAME
                            AND P~TABCLASS = 'TRANSP')
*                              AND p~cityto   = @cityto )
       INNER JOIN DD09L AS F ON F~TABNAME = P~TABNAME )
  WHERE ( ( C~ROLLNAME EQ @S_FIELD2-LOW )
  OR ( C~FIELDNAME EQ @S_FIELD2-LOW  )    )
  AND P~TABNAME NOT LIKE @LV_TAB
     INTO TABLE @DATA(ITAB14).
ENDIF.
IF S_FIELD3-LOW IS NOT INITIAL.
  SELECT C~TABNAME
  FROM ( ( DD03L AS C
       INNER JOIN DD02V AS P ON P~TABNAME  = C~TABNAME
                            AND P~TABCLASS = 'TRANSP')
       INNER JOIN DD09L AS F ON F~TABNAME = P~TABNAME )
  WHERE ( ( C~ROLLNAME EQ @S_FIELD3-LOW )

  OR ( C~FIELDNAME EQ @S_FIELD3-LOW  )    )
  AND P~TABNAME NOT LIKE @LV_TAB
     INTO TABLE @DATA(ITAB15).
ENDIF.
IF S_FIELD4-LOW IS NOT INITIAL.
  SELECT C~TABNAME
FROM ( ( DD03L AS C
     INNER JOIN DD02V AS P ON P~TABNAME  = C~TABNAME
                          AND P~TABCLASS = 'TRANSP')
*                              AND p~cityto   = @cityto )
     INNER JOIN DD09L AS F ON F~TABNAME = P~TABNAME )
WHERE ( ( C~ROLLNAME EQ @S_FIELD4-LOW )
OR ( C~FIELDNAME EQ @S_FIELD4-LOW  )
)
AND P~TABNAME NOT LIKE @LV_TAB
*                                AND f~connid = p~connid )
*       ORDER BY c~carrname, p~connid, f~fldate
   INTO TABLE @DATA(ITAB16).
ENDIF.
SORT ITAB13 BY TABNAME.
DELETE ADJACENT DUPLICATES FROM ITAB13 COMPARING TABNAME..
SORT ITAB14 BY TABNAME.
DELETE ADJACENT DUPLICATES FROM ITAB14 COMPARING TABNAME..
SORT ITAB15 BY TABNAME.
DELETE ADJACENT DUPLICATES FROM ITAB15 COMPARING TABNAME..
SORT ITAB16 BY TABNAME.
DELETE ADJACENT DUPLICATES FROM ITAB16 COMPARING TABNAME.
IF S_FIELD1-LOW IS NOT INITIAL.

  LOOP AT ITAB13 INTO DATA(WA_TAB133).
    IF ITAB14[] IS NOT INITIAL.
      READ TABLE ITAB14  INTO DATA(WA_TAB134) WITH KEY TABNAME = WA_TAB133-TABNAME.
      IF SY-SUBRC = 0.
        CREATE DATA DREF TYPE TABLE OF (WA_TAB134-TABNAME).
        IF ITAB15[] IS NOT INITIAL.
          READ TABLE ITAB15  INTO DATA(WA_TAB135) WITH KEY TABNAME = WA_TAB133-TABNAME.
          IF SY-SUBRC = 0.
            IF ITAB16[] IS NOT INITIAL.
              READ TABLE ITAB16  INTO DATA(WA_TAB136) WITH KEY TABNAME = WA_TAB133-TABNAME.
              IF SY-SUBRC = 0.
                ASSIGN DREF->* TO <TABLE>.
                SELECT  *
              FROM (WA_TAB134-TABNAME)
                  INTO TABLE @<TABLE>.
                IF <TABLE> IS NOT INITIAL.

                  WA_ALV-TABNAME =  WA_TAB134-TABNAME.
                  WA_ALV-FIELD1 =  S_FIELD1-LOW.
                  WA_ALV-FIELD2 =  S_FIELD2-LOW.
                  WA_ALV-FIELD3 =  S_FIELD3-LOW.
                  WA_ALV-FIELD4 =  S_FIELD4-LOW.
                  APPEND WA_ALV TO IALV.
                  CLEAR WA_ALV.
*      exit.
                ENDIF.
              ELSE.

                ASSIGN DREF->* TO <TABLE>.
                SELECT  *
              FROM (WA_TAB134-TABNAME)
                  INTO TABLE @<TABLE>.
                IF <TABLE> IS NOT INITIAL.

                  WA_ALV-TABNAME =  WA_TAB134-TABNAME.
                  WA_ALV-FIELD1 =  S_FIELD1-LOW.
                  WA_ALV-FIELD2 =  S_FIELD2-LOW.
                  WA_ALV-FIELD3 =  S_FIELD3-LOW.
*                  WA_ALV-FIELD4 =  S_FIELD4-LOW.
                  APPEND WA_ALV TO IALV.
                  CLEAR WA_ALV.
*      exit.
                ENDIF.
              ENDIF.
            ELSE.

              ASSIGN DREF->* TO <TABLE>.
              SELECT  *
            FROM (WA_TAB134-TABNAME)
                INTO TABLE @<TABLE>.
              IF <TABLE> IS NOT INITIAL.

                WA_ALV-TABNAME =  WA_TAB134-TABNAME.
                WA_ALV-FIELD1 =  S_FIELD1-LOW.
                WA_ALV-FIELD2 =  S_FIELD2-LOW.
                WA_ALV-FIELD3 =  S_FIELD3-LOW.

                APPEND WA_ALV TO IALV.
                CLEAR WA_ALV.

              ENDIF.
            ENDIF.
          ELSE.
            ASSIGN DREF->* TO <TABLE>.
            SELECT  *
          FROM (WA_TAB134-TABNAME)
              INTO TABLE @<TABLE>.
            IF <TABLE> IS NOT INITIAL.

              WA_ALV-TABNAME =  WA_TAB134-TABNAME.
              WA_ALV-FIELD1 =  S_FIELD1-LOW.
              WA_ALV-FIELD2 =  S_FIELD2-LOW.

              APPEND WA_ALV TO IALV.
              CLEAR WA_ALV.

            ENDIF.
          ENDIF.


        ELSE.
          ASSIGN DREF->* TO <TABLE>.
          SELECT  *
        FROM (WA_TAB134-TABNAME)
            INTO TABLE @<TABLE>.
          IF <TABLE> IS NOT INITIAL.
            WA_ALV-TABNAME =  WA_TAB134-TABNAME.
            WA_ALV-FIELD1 =  S_FIELD1-LOW.
            WA_ALV-FIELD2 =  S_FIELD2-LOW.

            APPEND WA_ALV TO IALV.
            CLEAR WA_TAB134.

          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.
*  endif.
  ENDLOOP.


ENDIF.


*  Populate Field Catalog
PERFORM GET_FIELDCATALOG.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    I_CALLBACK_PROGRAM = SY-REPID
*   is_layout          = w_layout
    IT_FIELDCAT        = FIELDCAT[]
*   it_events          = i_events
  TABLES
    T_OUTTAB           = IALV
  EXCEPTIONS
    PROGRAM_ERROR      = 1
    OTHERS             = 2.
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.


************************************************************************
*      Form  Get_Fieldcatalog - Set Up Columns/Headers
************************************************************************
FORM GET_FIELDCATALOG.

  CLEAR: FIELDCAT.
  FIELDCAT-SELTEXT_M    = 'Tablename'.
  FIELDCAT-FIELDNAME  = 'TABNAME'.
  FIELDCAT-OUTPUTLEN  = '36'.
  APPEND FIELDCAT TO FIELDCAT.

  CLEAR: FIELDCAT.
  FIELDCAT-SELTEXT_M   = 'Field1'.
  FIELDCAT-FIELDNAME  = 'FIELD1'.
  FIELDCAT-OUTPUTLEN  = '12'.
  APPEND FIELDCAT TO FIELDCAT.

  CLEAR: FIELDCAT.
  FIELDCAT-SELTEXT_M   = 'Field2'.
  FIELDCAT-FIELDNAME  = 'FIELD2'.
  FIELDCAT-OUTPUTLEN  = '12'.
  APPEND FIELDCAT TO FIELDCAT.

  CLEAR: FIELDCAT.
  FIELDCAT-SELTEXT_M   = 'Field3'.
  FIELDCAT-FIELDNAME  = 'FIELD3'.
  FIELDCAT-OUTPUTLEN  = '12'.
  APPEND FIELDCAT TO FIELDCAT.

  CLEAR: FIELDCAT.
  FIELDCAT-SELTEXT_M    = 'Field4'.
  FIELDCAT-FIELDNAME  = 'FIELD4'.
  FIELDCAT-OUTPUTLEN  = '12'.
  APPEND FIELDCAT TO FIELDCAT.

ENDFORM.
```
found [here](https://blogs.sap.com/2020/08/22/how-to-find-the-table-that-stores-multiple-field-values-thats-what-abaper-and-functional-does-in-wriceff-development/#)
