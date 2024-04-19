*&---------------------------------------------------------------------*
*&  Include           ZNTCONFIGI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  HANDLE_USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE handle_user_command_9000 INPUT.

  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM sub_save_config_settings.
    WHEN 'FILT'.
      PERFORM sub_filter_records.

    WHEN OTHERS.
  ENDCASE.

  CLEAR ok_code.

ENDMODULE.                 " HANDLE_USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  UPDATE_REPVAR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_repvar INPUT.

  READ TABLE t_prog WITH KEY tcode = t_config-tcode.
  IF sy-subrc = 0.
    SELECT SINGLE variant
           FROM varid
           INTO w_variant
           WHERE report  = t_prog-pgmna
           AND   variant = t_config-repvar.

    IF sy-subrc <> 0.
      MESSAGE e002 WITH t_config-repvar t_config-tcode.
    ENDIF.

  ENDIF.

  MODIFY t_config INDEX tabconfig-current_line
                  TRANSPORTING repvar.
ENDMODULE.                 " UPDATE_REPVAR  INPUT
*&---------------------------------------------------------------------*
*&      Module  HANDLE_EXIT_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE handle_exit_command_9000 INPUT.

  CASE ok_code.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " HANDLE_EXIT_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  HANDLE_HELP_REQUEST_SECTION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE handle_help_request_section INPUT.

  IF t_tab_help[] IS INITIAL.

    LOOP AT t_config.
      t_tab_help-helptxt = t_config-tabtxt.
      COLLECT t_tab_help.
    ENDLOOP.

  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'HELPTXT'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'W_FILT_TAB'
      value_org       = 'S'
    TABLES
      value_tab       = t_tab_help
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDMODULE.                 " HANDLE_HELP_REQUEST_SECTION  INPUT
*&---------------------------------------------------------------------*
*&      Module  UPDATE_TEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_text INPUT.
  MODIFY t_config TRANSPORTING btntxt WHERE fcode = t_config-fcode.
ENDMODULE.                 " UPDATE_TEXT  INPUT
*&---------------------------------------------------------------------*
*&      Module  UPDATE_AUTO_EXEC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_auto_exec INPUT.
  MODIFY t_config INDEX tabconfig-current_line
                  TRANSPORTING autoexec .
ENDMODULE.                 " UPDATE_AUTO_EXEC  INPUT
