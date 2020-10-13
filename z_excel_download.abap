REPORT z_excel_download.

CONSTANTS: lc_separ VALUE ';'. "separator

DATA: lt_output TYPE TABLE OF zfi_elko_int WITH HEADER LINE,
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
