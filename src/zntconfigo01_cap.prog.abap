*&---------------------------------------------------------------------*
*&  Include           ZNTCONFIGO01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'GUI9000'.
  SET TITLEBAR  'TITLE9000'.
  DESCRIBE TABLE t_config LINES tabconfig-lines.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  READ_CONFIG_SETTINGS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_config_settings OUTPUT.

  DATA : w_domval TYPE domvalue_l,
         w_text   TYPE val_text.

  CHECK t_config[] IS INITIAL.

*  SELECT *
*         FROM zntconfig
*         INTO TABLE t_config.

  SELECT *
         FROM zntconfig_cap
         INTO TABLE t_config.

  IF sy-subrc = 0.

    SELECT tcode
           pgmna
           FROM tstc
           INTO TABLE t_prog
           FOR ALL ENTRIES IN t_config
           WHERE tcode = t_config-tcode.

    LOOP AT t_config.

      CLEAR : w_domval, w_text.

      w_domval = t_config-function.

      CALL FUNCTION 'DOMAIN_VALUE_GET'
        EXPORTING
          i_domname  = 'ZZFUNC'
          i_domvalue = w_domval
        IMPORTING
          e_ddtext   = w_text
        EXCEPTIONS
          not_exist  = 1
          OTHERS     = 2.

      t_config-functxt = w_text.
      MODIFY t_config TRANSPORTING functxt.

    ENDLOOP.

    t_config_cp[] = t_config[].

  ENDIF.

ENDMODULE.                 " READ_CONFIG_SETTINGS  OUTPUT
