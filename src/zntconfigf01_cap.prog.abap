*&---------------------------------------------------------------------*
*&  Include           ZNTCONFIGF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SUB_SAVE_CONFIG_SETTINGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_save_config_settings .
*  DATA : l_t_config TYPE STANDARD TABLE OF zntconfig WITH HEADER LINE.
  DATA : l_t_config TYPE STANDARD TABLE OF zntconfig_cap WITH HEADER LINE.

  FIELD-SYMBOLS : <fs_config> TYPE ty_config.

  LOOP AT t_config_cp ASSIGNING <fs_config>.

    READ TABLE t_config WITH KEY fcode    = <fs_config>-fcode
                                 function = <fs_config>-function.
    IF sy-subrc = 0.
      <fs_config>-repvar   = t_config-repvar.
      <fs_config>-autoexec = t_config-autoexec.
    ENDIF.

  ENDLOOP.

  LOOP AT t_config_cp.
    CLEAR l_t_config.
    MOVE-CORRESPONDING t_config_cp TO l_t_config.
    APPEND l_t_config.
  ENDLOOP.

  MODIFY zntconfig_cap FROM TABLE l_t_config.
  IF sy-subrc = 0.
    MESSAGE s000 WITH 'Configuration Updated'(001).
  ENDIF.

  COMMIT WORK.

ENDFORM.                    " SUB_SAVE_CONFIG_SETTINGS
*&---------------------------------------------------------------------*
*&      Form  SUB_FILTER_RECORDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_filter_records .

  FIELD-SYMBOLS : <fs_config> TYPE ty_config.

  LOOP AT t_config_cp ASSIGNING <fs_config>.

    READ TABLE t_config WITH KEY fcode    = <fs_config>-fcode
                                 function = <fs_config>-function.
    IF sy-subrc = 0.
      <fs_config>-repvar   = t_config-repvar.
      <fs_config>-autoexec = t_config-autoexec.
    ENDIF.

  ENDLOOP.

  t_config[] = t_config_cp[].
  IF w_filt_tab IS NOT INITIAL.
    DELETE t_config WHERE tabtxt <> w_filt_tab.
  ENDIF.

ENDFORM.                    " SUB_FILTER_RECORDS
