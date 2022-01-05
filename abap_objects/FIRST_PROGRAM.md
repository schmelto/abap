# First Program

## Main

```abap
REPORT z_first_program.

TABLES spfli.

CLASS application DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
             read_data IMPORTING carrid TYPE spfli-carrid,
             fill_list.
  PRIVATE SECTION.
    DATA: spfli_tab TYPE TABLE OF spfli,
          container TYPE REF TO cl_gui_custom_container,
          alv_list  TYPE REF TO cl_gui_alv_grid.
ENDCLASS.

CLASS application IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT container
      EXPORTING container_name = 'LIST_AREA'.
    CREATE OBJECT alv_list
      EXPORTING i_parent = container.
    CALL METHOD alv_list->set_table_for_first_display
      EXPORTING i_structure_name = 'SPFLI'
      CHANGING it_outtab = spfli_tab.
  ENDMETHOD.
  METHOD read_data.
      SELECT * FROM spfli
               INTO TABLE spfli_tab
               WHERE carrid = carrid.
  ENDMETHOD.
  METHOD fill_list.
    CALL METHOD alv_list->refresh_table_display.
  ENDMETHOD.
ENDCLASS.

DATA object_ref TYPE REF TO application.

START-OF-SELECTION.

  CREATE OBJECT object_ref.
  CALL SCREEN 100.

MODULE status_0100 OUTPUT.
    SET PF-STATUS 'SCREEN_100'.
    CALL METHOD object_ref->fill_list.
ENDMODULE.

MODULE user_command_0100 INPUT.
  IF sy-ucomm = 'BACK' OR
     sy-ucomm = 'EXIT' OR
     sy-ucomm = 'CANCEL'.
    LEAVE PROGRAM.
  ELSE.
    CALL METHOD object_ref->read_data
      EXPORTING carrid = spfli-carrid.
  ENDIF.
ENDMODULE.
```

## Screen 0100

```abap
PROCESS BEFORE OUTPUT.
  MODULE STATUS_0100.

PROCESS AFTER INPUT.
  MODULE USER_COMMAND_0100.
```

## SCREEN_100

![image](https://user-images.githubusercontent.com/30869493/148220037-34c3cc9c-778a-4781-997c-aaac7efaf344.png)

