*&---------------------------------------------------------------------*
*&  Include           ZCPMAINF06
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_CREATE_BASE_TRANS
*&---------------------------------------------------------------------*
*       Handle push button action Create for
*       Order/Notification/Functional Location
*----------------------------------------------------------------------*
FORM sub_handle_create_base_trans.

  DATA :
         l_disp_tcode TYPE sy-tcode,

         l_tcode      TYPE sy-tcode.

  DATA : l_function TYPE zzfunction,
         l_program  TYPE sy-cprog,
         l_ret_code TYPE sy-subrc.
  REFRESH : t_selparams.
  CLEAR t_config.

  CASE 'X'.
    WHEN w_x_order. " IW31
      l_function = 'ORDR'.
    WHEN w_x_notif." IW21
      l_function = 'NOTF'.
  ENDCASE.

  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code
                               function = l_function.
  IF t_config-tcode IS NOT INITIAL.

    PERFORM sub_check_tcode_authority USING    t_config-tcode
                                      CHANGING l_ret_code.

    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003.
    ENDIF.

    PERFORM sub_get_prog_name USING   t_config-tcode
                                  CHANGING l_program.
    IF t_config-repvar IS NOT INITIAL.
      PERFORM sub_submit_report TABLES t_selparams
                                USING  l_program
                                       t_config-repvar
                                       t_config-autoexec.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.

  ELSE.
    MESSAGE 'Tcode is not exist in the table ZNTCONFIG' TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_HANDLE_CREATE_BASE_TRANS
*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_CHANGE_BASE_TRANS
*&---------------------------------------------------------------------*
*       Handle push button action Change for
*       Order/Notification/Functional Location
*----------------------------------------------------------------------*
FORM sub_handle_change_base_trans .

  DATA : l_ret_code   TYPE sy-subrc,
         l_disp_tcode TYPE sy-tcode,
         l_function TYPE zzfunction,

         l_program  TYPE sy-cprog,
         l_tcode      TYPE sy-tcode.


  CASE 'X'.
    WHEN w_x_order. " IW32
      l_function = 'ORDR'.
    WHEN w_x_notif." IW22
      l_function = 'NOTF'.
  ENDCASE.

  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code
                               function = l_function.
  IF t_config-tcode IS NOT INITIAL.

    PERFORM sub_check_tcode_authority USING    t_config-tcode
                                      CHANGING l_ret_code.

    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003.
    ENDIF.

    PERFORM sub_get_prog_name USING   t_config-tcode
                                  CHANGING l_program.
    IF t_config-repvar IS NOT INITIAL.
      PERFORM sub_submit_report TABLES t_selparams
                                USING  l_program
                                       t_config-repvar
                                       t_config-autoexec.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.
  ELSE.
    MESSAGE 'Tcode is not exist in the table ZNTCONFIG' TYPE 'I'.
  ENDIF.


ENDFORM.                    " SUB_HANDLE_CHANGE_BASE_TRANS
*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_DISPLAY_BASE_TRANS
*&---------------------------------------------------------------------*
*       Handle push button action Display for
*       Order/Notification/Functional Location
*----------------------------------------------------------------------*
FORM sub_handle_display_base_trans .

  DATA : l_ret_code   TYPE sy-subrc,
         l_disp_tcode TYPE sy-tcode,
         l_function TYPE zzfunction,
         l_program  TYPE sy-cprog,
         l_tcode      TYPE sy-tcode.


  CASE 'X'.
    WHEN w_x_order. " IW33
      l_function = 'ORDR'.
    WHEN w_x_notif." IW22
      l_function = 'NOTF'.
  ENDCASE.

  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code
                               function = l_function.
  IF t_config-tcode IS NOT INITIAL.

    PERFORM sub_check_tcode_authority USING    t_config-tcode
                                      CHANGING l_ret_code.

    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003.
    ENDIF.

    PERFORM sub_get_prog_name USING   t_config-tcode
                                   CHANGING l_program.
    IF t_config-repvar IS NOT INITIAL.
      PERFORM sub_submit_report TABLES t_selparams
                                USING  l_program
                                       t_config-repvar
                                       t_config-autoexec.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.
  ELSE.
    MESSAGE 'Tcode is not exist in the table ZNTCONFIG' TYPE 'I'.
  ENDIF.


ENDFORM.                    " SUB_HANDLE_DISPLAY_BASE_TRANS
*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_LISTEDIT_BASE_TRANS
*&---------------------------------------------------------------------*
*       Handle push button action List Edit for
*       Order/Notification/Functional Location
*----------------------------------------------------------------------*
FORM sub_handle_listedit_base_trans.

  DATA : l_ret_code   TYPE sy-subrc,
         l_function   TYPE zzfunction,
         l_program   TYPE sy-cprog,
         l_tcode      TYPE sy-tcode.

  CASE 'X'.
    WHEN w_x_order.
      l_function   = c_func_ordr.
    WHEN w_x_notif.
      l_function   = c_func_notf.
  ENDCASE.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code
                               function = l_function.

  IF t_config-tcode IS NOT INITIAL.
    PERFORM sub_get_prog_name USING   t_config-tcode
                               CHANGING l_program.

*{   REPLACE        D30K925363                                        1
*\    PERFORM sub_populate_seltab USING 'ARBPL' 'STRNO' space 'QMART' space.
      IF w_x_order IS NOT INITIAL.
*-- Fix the issue with Order type value not beign picked up from the
*--        defaults section.
*          PERFORM sub_populate_seltab USING 'GEWRK' 'STRNO' space 'QMART' space.
          PERFORM sub_populate_seltab USING 'GEWRK' 'STRNO' 'AUART' space space.
      ELSE.
          PERFORM sub_populate_seltab USING 'ARBPL' 'STRNO' space 'QMART' space.
      ENDIF.

*}   REPLACE

    PERFORM sub_check_tcode_authority USING   t_config-tcode
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003.
      RETURN.
    ENDIF.

    IF x_defaults-zzunit1 IS NOT INITIAL .
*--  Populate parameter PLANNING PLANT
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF x_defaults-zzunit2 IS NOT INITIAL.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.

***populate date
    IF s_date[] IS NOT INITIAL.
      LOOP AT s_date.
        CLEAR t_selparams.
        MOVE-CORRESPONDING s_date TO t_selparams.
        t_selparams-selname = 'ERDAT'.
        t_selparams-kind    = 'S'.
        APPEND t_selparams.
      ENDLOOP.

    ENDIF.

    PERFORM sub_get_prog_name USING   t_config-tcode
                                  CHANGING l_program.
    IF t_config-repvar IS NOT INITIAL.
      PERFORM sub_submit_report TABLES t_selparams
                                USING  l_program
                                       t_config-repvar
                                       t_config-autoexec.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.
  ELSE.
    MESSAGE 'Tcode is not exist in the table ZNTCONFIG' TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_HANDLE_LISTEDIT_BASE_TRANS
*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_ORDER_OPERATIONS
*&---------------------------------------------------------------------*
*       Handle push button action "Order Operations"
*----------------------------------------------------------------------*
FORM sub_handle_order_operations .

  DATA : l_ret_code  TYPE sy-subrc,
         l_prog_name TYPE sy-cprog.


  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
    PERFORM sub_check_tcode_authority USING    t_config-tcode"c_tcode_iw37n
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003.
      RETURN.
    ENDIF.

    PERFORM sub_populate_seltab USING 'S_VARBPL' 'S_STRNO'
                                      'S_AUART'  space space.
    IF x_defaults-zzunit1 IS NOT INITIAL .

*--  Populate parameter PLANNING PLANT
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF x_defaults-zzunit2 IS NOT INITIAL.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.

***populate date
    IF s_date[] IS NOT INITIAL.
      LOOP AT s_date.
        CLEAR t_selparams.
        MOVE-CORRESPONDING s_date TO t_selparams.
        t_selparams-selname = 'ERDAT'.
        t_selparams-kind    = 'S'.
        APPEND t_selparams.
      ENDLOOP.

    ENDIF.

    PERFORM sub_get_prog_name USING    t_config-tcode " RI_ORDER_OPERATION_LIST
                              CHANGING l_prog_name.
    IF t_config-repvar IS NOT INITIAL.
      PERFORM sub_submit_report TABLES t_selparams
                                USING  l_prog_name
                                       t_config-repvar
                                       t_config-autoexec.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.

  ELSE.
    MESSAGE 'Tcode is not exist in the table ZNTCONFIG' TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_HANDLE_ORDER_OPERATIONS
*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_REPORT_PROBLEM
*&---------------------------------------------------------------------*
*       Handle push button action "Report a Problem"
*----------------------------------------------------------------------*
FORM sub_handle_report_problem .
**new code added jsharma
  DATA x_batch_opt TYPE ctu_params.
  DATA : lv_type TYPE riwo00-qmart.
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
    IF t_config-tcode = 'IW21'.
      REFRESH t_bdcdata.

      PERFORM sub_bdc_dynpro USING 'SAPLIQS0' '0100'.
      PERFORM sub_bdc_field  USING 'BDC_OKCODE' '/00'.
      PERFORM sub_bdc_field  USING 'RIWO00-QMART' 'M1'.
*
      x_batch_opt-updmode = c_sync.
      x_batch_opt-dismode = c_mode_error.
      x_batch_opt-nobinpt = 'X'.

      CALL TRANSACTION t_config-tcode
           USING t_bdcdata
           OPTIONS FROM x_batch_opt.
*    lv_type = 'M1'.
*    SET PARAMETER ID 'QMR' FIELD lv_type.
*    GET PARAMETER ID 'QMR' FIELD lv_type.
*    CALL TRANSACTION 'IW21'.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.
  ELSE.
    MESSAGE 'Tcode is not exist in the table ZNTCONFIG' TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_HANDLE_REPORT_PROBLEM
*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_MASTER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_handle_master_data .
*new code added jsharma
  DATA x_batch_opt TYPE ctu_params.
  DATA : lv_type TYPE riwo00-qmart.
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
    IF t_config-tcode = 'IW21'.
      REFRESH t_bdcdata.

      PERFORM sub_bdc_dynpro USING 'SAPLIQS0' '0100'.
      PERFORM sub_bdc_field  USING 'BDC_OKCODE' '/00'.
      PERFORM sub_bdc_field  USING 'RIWO00-QMART' 'M4'.
*
      x_batch_opt-updmode = c_sync.
      x_batch_opt-dismode = c_mode_error.
      x_batch_opt-nobinpt = 'X'.

      CALL TRANSACTION t_config-tcode
           USING t_bdcdata
           OPTIONS FROM x_batch_opt.
*    lv_type = 'M1'.
*    SET PARAMETER ID 'QMR' FIELD lv_type.
*    GET PARAMETER ID 'QMR' FIELD lv_type.
*    CALL TRANSACTION 'IW21'.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.
  ELSE.
    MESSAGE 'Tcode is not exist in the table ZNTCONFIG' TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_HANDLE_MASTER_DATA
