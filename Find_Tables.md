
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