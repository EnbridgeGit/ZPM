*----------------------------------------------------------------------*
***INCLUDE ZCPOE_USER_COMMAND_9000 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
*- store user command in a local variable *-
  ok_code = sy-ucomm.

*- check user command and navigate accordingly *-
  CASE ok_code.
    WHEN 'BACK'.
*- leave to previous screen *-
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
*- leave program *-
      LEAVE PROGRAM.
    WHEN 'SMM'.
*- go to SAP Main Menu *-
      LEAVE TO TRANSACTION 'SESSION_MANAGER'.
    WHEN 'MCP'.
*- go to main control panel *-
      CALL TRANSACTION 'ZCPMAIN'.
    WHEN 'DEF'.
*- defaults popup *-
      CALL FUNCTION 'Z_NT_GET_DEFAULTS_DIALOG'.
    WHEN 'USER'.
      CALL TRANSACTION 'ZCPUSER'.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
