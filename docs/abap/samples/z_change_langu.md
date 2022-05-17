## z_change_langu.abap

Change the logon language without new Login to SAP GUI.

```abap
REPORT z_change_langu.

DATA: server  TYPE          msxxlist,
      servers TYPE TABLE OF msxxlist.

PARAMETERS: language LIKE sy-langu MATCHCODE OBJECT h_t002
                                  OBLIGATORY
                                  DEFAULT sy-langu.

START-OF-SELECTION.

* initialize internal table for server list 
  CLEAR: servers[].

* get server list
  CALL FUNCTION 'TH_SERVER_LIST'
    TABLES
      list           = servers
    EXCEPTIONS
      no_server_list = 1
      OTHERS         = 2.

* process server list
  LOOP AT servers INTO  server
                    WHERE host = sy-host.

* set new language
    SET LOCALE LANGUAGE language.

* start remote transaction
    CALL FUNCTION 'TH_REMOTE_TRANSACTION'
      EXPORTING
        tcode = space
        dest  = server-name.

* exit loop
    EXIT.

  ENDLOOP.

* leave program
  LEAVE PROGRAM.
```
