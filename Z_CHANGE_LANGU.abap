*&---------------------------------------------------------------------*
*& Report Z_CHANGE_LANGU                                               *
*&---------------------------------------------------------------------*
*& Mit diesem Report kann die Sprache in SAP geändert werden           *
*&---------------------------------------------------------------------*
*& 2021-03-22 Paul Salaz                                               *
*&            Erstellen des Programm                                   *
*&---------------------------------------------------------------------*
REPORT z_change_langu.


*&--------------------------------------------------------------------&*
*&  Daten-Definition                                                  &*
*&--------------------------------------------------------------------&*
DATA: gs_server TYPE          msxxlist,
      gt_server TYPE TABLE OF msxxlist.


*&--------------------------------------------------------------------&*
*&  Selections-Screen                                                 &*
*&--------------------------------------------------------------------&*
PARAMETERS: p_langu LIKE sy-langu MATCHCODE OBJECT h_t002
                                  OBLIGATORY
                                  DEFAULT sy-langu.


*&--------------------------------------------------------------------&*
*&  Start of Selektion                                                &*
*&--------------------------------------------------------------------&*
START-OF-SELECTION.

* Int. Tabelle für Server-Liste initialisieren
  CLEAR: gt_server[].

* Server-Liste besorgen
  CALL FUNCTION 'TH_SERVER_LIST'
    TABLES
      list           = gt_server
    EXCEPTIONS
      no_server_list = 1
      OTHERS         = 2.

* Verarbeiten der Server-Liste
  LOOP AT gt_server INTO  gs_server
                    WHERE host = sy-host.

*   Neue Sprache setzen
    SET LOCALE LANGUAGE p_langu.

*   Remote Transaktion anstarten
    CALL FUNCTION 'TH_REMOTE_TRANSACTION'
      EXPORTING
        tcode = space
        dest  = gs_server-name.

*   Schleife verlassen
    EXIT.

  ENDLOOP.

* Programm verlassen
  LEAVE PROGRAM.
