CLASS zcl_tutorial_simple_daemon DEFINITION
  PUBLIC
  INHERITING FROM cl_abap_daemon_ext_base
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.
    CLASS-METHODS start
      IMPORTING
        iv_daemon_name TYPE string
        iv_timeout     TYPE i
      RAISING
        cx_abap_daemon_error
        cx_ac_message_type_pcp_error.
    CLASS-METHODS send
      IMPORTING
        iv_daemon_name TYPE string
        iv_text        TYPE string
      RAISING
        cx_abap_daemon_error
        cx_ac_message_type_pcp_error.
    CLASS-METHODS stop
      IMPORTING
        iv_daemon_name TYPE string
      RAISING
        cx_abap_daemon_error.
    METHODS: if_abap_daemon_extension~on_error REDEFINITION,
             if_abap_daemon_extension~on_message REDEFINITION,
             if_abap_daemon_extension~on_restart REDEFINITION,
             if_abap_daemon_extension~on_server_shutdown REDEFINITION,
             if_abap_daemon_extension~on_accept REDEFINITION,
             if_abap_daemon_extension~on_start REDEFINITION,
             if_abap_daemon_extension~on_stop REDEFINITION,
             if_abap_daemon_extension~on_system_shutdown REDEFINITION,
             if_abap_daemon_extension~on_before_restart_by_system REDEFINITION.

    INTERFACES if_abap_timer_handler.
PROTECTED SECTION.
PRIVATE SECTION.
    DATA: mv_timeout TYPE i,
          mo_timer   TYPE REF TO if_abap_timer_manager,
          mv_counter TYPE i.
ENDCLASS.



CLASS zcl_tutorial_simple_daemon IMPLEMENTATION.
    METHOD start.
      " set ABAP Daemon start parameters
      DATA(lo_pcp) = cl_ac_message_type_pcp=>create( ).
      lo_pcp->set_field( i_name = 'timeout' i_value = CONV #( iv_timeout ) ).

      " start the daemon application using the ABAP Daemon Manager
      cl_abap_daemon_client_manager=>start(
          i_class_name = 'ZCL_TUTORIAL_SIMPLE_DAEMON'
          i_name       = CONV #( iv_daemon_name )
          i_priority   = cl_abap_daemon_client_manager=>co_session_priority_low
          i_parameter  = lo_pcp ).
    ENDMETHOD.

    METHOD send.
      " retrieve the list of ABAP Daemon instances
      DATA(lt_ad_info) = cl_abap_daemon_client_manager=>get_daemon_info( i_class_name = 'ZCL_TUTORIAL_SIMPLE_DAEMON').

      " create PCP message with text
      DATA(lo_pcp) = cl_ac_message_type_pcp=>create( ).
      lo_pcp->set_text( iv_text ).

      " for each running daemon instance of this class
      LOOP AT lt_ad_info ASSIGNING FIELD-SYMBOL(<ls_info>).

        " send a message if the names match
        IF iv_daemon_name = <ls_info>-name.
          cl_abap_daemon_client_manager=>attach( <ls_info>-instance_id )->send( lo_pcp ).
        ENDIF.

      ENDLOOP.
    ENDMETHOD.

    METHOD stop.
      " retrieve the list of ABAP Daemon instances
      DATA(lt_ad_info) = cl_abap_daemon_client_manager=>get_daemon_info( i_class_name = 'ZCL_TUTORIAL_SIMPLE_DAEMON').

      " for each running daemon instance of this class
      LOOP AT lt_ad_info ASSIGNING FIELD-SYMBOL(<ls_info>).

        " stop the daemon if the names match
        IF iv_daemon_name = <ls_info>-name.
            cl_abap_daemon_client_manager=>stop( i_instance_id = <ls_info>-instance_id ).
        ENDIF.

      ENDLOOP.
    ENDMETHOD.

    METHOD if_abap_daemon_extension~on_accept.
      TRY.
          DATA lv_program_name TYPE program.
          lv_program_name = cl_oo_classname_service=>get_classpool_name( 'ZCL_TUTORIAL_SIMPLE_DAEMON' ).

          IF i_context_base->get_start_caller_info( )-program = lv_program_name.
            e_setup_mode = co_setup_mode-accept.
          ELSE.
            e_setup_mode = co_setup_mode-reject.
          ENDIF.
        CATCH cx_abap_daemon_error.
          " to do: error handling, e.g. write error log!
          e_setup_mode = co_setup_mode-reject.
      ENDTRY.
    ENDMETHOD.

  METHOD if_abap_daemon_extension~on_before_restart_by_system.

  ENDMETHOD.

  METHOD if_abap_daemon_extension~on_error.

  ENDMETHOD.

    METHOD if_abap_daemon_extension~on_message.
      TRY.
          " get text from PCP message
          DATA(lv_text) = i_message->get_text( ).

          " display popup
          CALL FUNCTION 'TH_POPUP'
            EXPORTING
              client  = sy-mandt
              user    = sy-uname
              message = CONV th_popup( |Message received: { lv_text }| ).
        CATCH cx_ac_message_type_pcp_error.
          " to do: error handling, e.g. write error log!
      ENDTRY.
    ENDMETHOD.

  METHOD if_abap_daemon_extension~on_restart.

  ENDMETHOD.

  METHOD if_abap_daemon_extension~on_server_shutdown.

  ENDMETHOD.

    METHOD if_abap_daemon_extension~on_start.
      TRY.
          " retrieve timeout from PCP start parameters
          mv_timeout = i_context->get_start_parameter( )->get_field( 'timeout' ).

          " start timer for displaying messages
          mo_timer = cl_abap_timer_manager=>get_timer_manager( ).
          mo_timer->start_timer( i_timeout = mv_timeout i_timer_handler = me ).

        CATCH cx_abap_daemon_error cx_ac_message_type_pcp_error cx_abap_timer_error.
          " to do: error handling, e.g. write error log!
      ENDTRY.
    ENDMETHOD.

  METHOD if_abap_daemon_extension~on_stop.

  ENDMETHOD.

  METHOD if_abap_daemon_extension~on_system_shutdown.

  ENDMETHOD.

  METHOD if_abap_timer_handler~on_timeout.
  " increment the loop counter
  ADD 1 TO mv_counter.

  " display popup message
  CALL FUNCTION 'TH_POPUP'
    EXPORTING
      client  = sy-mandt
      user    = sy-uname
      message = CONV th_popup( |Timeout triggered. Number of loops: { mv_counter }| ).

  " restart the timer if any loops are remaining
  IF mv_counter < 5.
    TRY.
        mo_timer->start_timer( i_timeout = mv_timeout i_timer_handler = me ).
      CATCH cx_abap_timer_error.
        " to do: error handling, e.g. write error log!
    ENDTRY.
  ENDIF.
ENDMETHOD.

ENDCLASS.
