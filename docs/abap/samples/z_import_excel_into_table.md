## z_import_excel_into_table.abap

```abap
REPORT z_import_excel_into_table.

* With the help of the class cl_fdt_xl_spreadsheet, XML transformations
* data can be extracted from XLSX files and converted into an internal table
TRY.
    DATA: lv_rc TYPE i.
    DATA: it_files TYPE filetable.
    DATA: lv_action TYPE i.

* Call up the FileOpen dialog
    cl_gui_frontend_services=>file_open_dialog( EXPORTING
                                                  file_filter = |xlsx (*.xlsx)\|*.xlsx\|{ cl_gui_frontend_services=>filetype_all }|
                                                CHANGING
                                                  file_table  = it_files
                                                  rc          = lv_rc
                                                  user_action = lv_action ).

    IF lv_action = cl_gui_frontend_services=>action_ok.
* when file is selected
      IF lines( it_files ) > 0.
* read first table entry
        DATA: lv_filesize TYPE w3param-cont_len.
        DATA: lv_filetype TYPE w3param-cont_type.
        DATA: it_bin_data TYPE w3mimetabtype.

* Image on appl. Upload server (binary)
        cl_gui_frontend_services=>gui_upload( EXPORTING
                                                filename   = |{ it_files[ 1 ]-filename }|
                                                filetype   = 'BIN'
                                              IMPORTING
                                                filelength = lv_filesize
                                              CHANGING
                                                data_tab   = it_bin_data ).

* solix -> xstring
        DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring( it_solix = it_bin_data ).

* Excel (itab) -> XML -> Ref object
* Attention: memory-intensive and rel. slow! Large amounts of data should not be processed.
        DATA(o_excel) = NEW cl_fdt_xl_spreadsheet( document_name = CONV #( it_files[ 1 ]-filename )
                                                   xdocument     = lv_bin_data ).

        DATA: it_worksheet_names TYPE if_fdt_doc_spreadsheet=>t_worksheet_names.

* Determine worksheet name
        o_excel->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = it_worksheet_names ).

        IF lines( it_worksheet_names ) > 0.
* get the first worksheet and -> create REF to itab
          DATA(o_worksheet_itab) = o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( it_worksheet_names[ 1 ] ).

* Map reference to generic field symbol
          ASSIGN o_worksheet_itab->* TO FIELD-SYMBOL(<worksheet>).
        ENDIF.
      ENDIF.
    ENDIF.

  CATCH cx_root INTO DATA(e_text).
    MESSAGE e_text->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.
```