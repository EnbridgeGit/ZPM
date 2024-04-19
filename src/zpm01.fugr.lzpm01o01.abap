*----------------------------------------------------------------------*
***INCLUDE LZPM01O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1000 OUTPUT.
  SET PF-STATUS 'ZMASS'.
  SET TITLEBAR 'ZTITLE'.
ENDMODULE.                 " STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1000 INPUT.
  CLEAR : gv_exit.

  SET PARAMETER ID 'EXT' FIELD gv_exit.
  IF ok_code EQ 'OK'.
*chek if every field is blank , no process throw error.
    IF ( afvgd-arbpl IS INITIAL ) AND
      ( afvgd-werks IS INITIAL ) AND
      ( gv_dispatch IS INITIAL ).
      MESSAGE text-001 TYPE 'E'.
    ENDIF.

    SET PARAMETER ID 'WRK' FIELD afvgd-arbpl.
    SET PARAMETER ID 'PLT' FIELD afvgd-werks.
    SET PARAMETER ID 'DIS' FIELD gv_dispatch.
    CLEAR : afvgd-arbpl, afvgd-werks,gv_dispatch.
    SET SCREEN 0.
    LEAVE SCREEN.

  ELSEIF ok_code EQ 'CANCEL'.

    gv_exit = 'X'.
    SET PARAMETER ID 'EXT' FIELD gv_exit.
    CLEAR : gv_exit.
    SET SCREEN 0.
    LEAVE SCREEN.

  ENDIF.

ENDMODULE.                 " USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*&      Module  VALIDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validate INPUT.
  DATA : lv_arbpl TYPE arbpl,
        lv_werks TYPE werks_d.

  SELECT SINGLE arbpl werks INTO (lv_arbpl,lv_werks)
    FROM crhd WHERE arbpl = afvgd-arbpl AND
                    werks = afvgd-werks.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE e051(iw) WITH afvgd-arbpl afvgd-werks.
  ENDIF.

ENDMODULE.                 " VALIDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command INPUT.

  gv_exit = 'X'.
  SET PARAMETER ID 'EXT' FIELD gv_exit.
  CLEAR : gv_exit.
  SET SCREEN 0.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT_COMMAND  INPUT
