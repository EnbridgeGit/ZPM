*----------------------------------------------------------------------*
***INCLUDE ZCPOE_GUI_STATUS_9000 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  SELECT SINGLE *
         FROM zntlogo
         WHERE tcode = sy-tcode.

*- set GUI status *-
  SET PF-STATUS 'PF9000'.

*- set screen title *-
  SET TITLEBAR 'TITL9000' WITH zntlogo-title.

ENDMODULE.                 " STATUS_9000  OUTPUT
