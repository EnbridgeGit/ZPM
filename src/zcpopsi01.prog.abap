*&---------------------------------------------------------------------*
*&  Include           ZCPOPSI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SUB_RES_OPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_res_ops .

** new code added by sghosh
  DATA :   l_program  TYPE sy-cprog,
           l_space    TYPE char1,
           l_function TYPE zzfunction,
           l_tcode    TYPE sy-tcode,
           l_ret_code TYPE sy-subrc.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN..
    ENDIF.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'IWERK'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'P107'.
    APPEND t_selparams.

    PERFORM sub_get_prog_name USING t_config-tcode
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_RES_OPS

*&---------------------------------------------------------------------*
*&      Form  SUB_RES_ENGR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_res_engr .
** new code added by sghosh
  DATA :   l_program  TYPE sy-cprog,
           l_space    TYPE char1,
           l_function TYPE zzfunction,
           l_ret_code TYPE sy-subrc.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN..
    ENDIF.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'IWERK'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'P107'.
    APPEND t_selparams.

    t_selparams-selname = 'AUART'.
    t_selparams-kind    = 'S'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'PM60'.
    t_selparams-high    = ''.
    APPEND t_selparams.

    PERFORM sub_get_prog_name USING t_config-tcode
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_RES_ENGR

*&---------------------------------------------------------------------*
*&      Form  SUB_RES_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_res_all .
** new code added by sghosh
  DATA :   l_program  TYPE sy-cprog,
           l_space    TYPE char1,
           l_function TYPE zzfunction,
           l_ret_code TYPE sy-subrc.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN..
    ENDIF.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'IWERK'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'P107'.
    APPEND t_selparams.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'DATUV'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'BT'.
    t_selparams-low     = ''.
    APPEND t_selparams.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'DATUB'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'BT'.
    t_selparams-low     = ''.
    APPEND t_selparams.


    PERFORM sub_get_prog_name USING t_config-tcode
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_RES_ALL

*&---------------------------------------------------------------------*
*&      Form  SUB_NOTIF_7DAYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_notif_7days .
  DATA : l_ret_code  TYPE sy-subrc,
         l_prog_name TYPE sy-cprog,
         lv_date TYPE sy-datum,
         l_space     TYPE char1.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN..
    ENDIF.

    CLEAR lv_date.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL_SG'
      EXPORTING
        date      = sy-datum
        days      = '0007' " 7days
        months    = '0000'
        signum    = '-'
        years     = '0000'
      IMPORTING
        calc_date = lv_date.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'ERDAT'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'BT'.
    t_selparams-low     = lv_date.
    t_selparams-high    = sy-datum.
    APPEND t_selparams.

    t_selparams-kind    = 'S'.
    t_selparams-selname = 'IWERK'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'P107'.
    APPEND t_selparams.

    PERFORM sub_get_prog_name USING    t_config-tcode
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_NOTIF_7DAYS

*&---------------------------------------------------------------------*
*&      Form  SUB_NOTIF_24HRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_notif_24hrs .
  DATA : l_ret_code  TYPE sy-subrc,
         l_prog_name TYPE sy-cprog,
         lv_date TYPE sy-datum,
         l_space     TYPE char1.
  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.

    CLEAR lv_date.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL_SG'
      EXPORTING
        date      = sy-datum
        days      = '0001' " 1days
        months    = '0000'
        signum    = '-'
        years     = '0000'
      IMPORTING
        calc_date = lv_date.


    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'ERDAT'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'BT'.
    t_selparams-low     = lv_date.
    t_selparams-high    = sy-datum.
    APPEND t_selparams.

    t_selparams-kind    = 'S'.
    t_selparams-selname = 'IWERK'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'P107'.
    APPEND t_selparams.

    PERFORM sub_get_prog_name USING    t_config-tcode
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_NOTIF_24HRS

*&---------------------------------------------------------------------*
*&      Form  SUB_NOTIF_PRTSHP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_notif_prtshp .
** new code added by sghosh
  DATA :   l_program  TYPE sy-cprog,
           l_space    TYPE char1,
           l_function TYPE zzfunction,
           l_ret_code TYPE sy-subrc.

  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_NOTIF_PRTSHP

*&---------------------------------------------------------------------*
*&      Form  SUB_NOTIF_BATCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_notif_batch .
  DATA : l_ret_code  TYPE sy-subrc,
         l_prog_name TYPE sy-cprog,
         lv_date TYPE sy-datum,
         l_space     TYPE char1.
  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.
    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'IWERK'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'P107'.
    APPEND t_selparams.

    PERFORM sub_get_prog_name USING    t_config-tcode
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_NOTIF_BATCH

*&---------------------------------------------------------------------*
*&      Form  SUB_ORDERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_orders .
** new code added by sghosh
  DATA :   l_program  TYPE sy-cprog,
           l_space    TYPE char1,
           l_function TYPE zzfunction,
           l_ret_code TYPE sy-subrc.

  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.
*    CALL TRANSACTION c_tcode_iw3d.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_ORDERS

*&---------------------------------------------------------------------*
*&      Form  SUB_ORDERS_BATCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_orders_batch .
  DATA : l_ret_code  TYPE sy-subrc,
         l_program TYPE sy-cprog,
         lv_date TYPE sy-datum,
         l_space     TYPE char1.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.
    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'IWERK'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'P107'.
    APPEND t_selparams.

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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_ORDERS_BATCH

*&---------------------------------------------------------------------*
*&      Form  SUB_TASKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_tasks .

  DATA : l_program  TYPE sy-cprog,
         l_space    TYPE char1,
         l_function TYPE zzfunction,
         l_ret_code TYPE sy-subrc.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.
*--  Populate parameter Created by
    CLEAR t_selparams.
    t_selparams-kind    = 'P'.
    t_selparams-selname = 'M_PARNR'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = sy-uname.
    APPEND t_selparams.

    CLEAR t_selparams.
    t_selparams-kind    = 'P'.
    t_selparams-selname = 'M_PARVW'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'VU'.
    APPEND t_selparams.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'SWERK'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'P107'.
    APPEND t_selparams.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'DATUB'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = ''.
    APPEND t_selparams.

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
    MESSAGE 'Tcode is not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_TASKS

*&---------------------------------------------------------------------*
*&      Form  SUB_ORDERS_OPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_orders_ops .
  DATA : l_function  TYPE zzfunction,
         l_ret_code  TYPE sy-subrc,
         l_program TYPE sy-cprog.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.
    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'IWERK'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'P107'.
    APPEND t_selparams.

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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_ORDERS_OPS

*&---------------------------------------------------------------------*
*&      Form  SUB_MEAS_DSPLY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_meas_dsply .
  DATA : l_function  TYPE zzfunction,
         l_ret_code  TYPE sy-subrc,
         l_program TYPE sy-cprog.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_MEAS_DSPLY

*&---------------------------------------------------------------------*
*&      Form  SUB_FLOC_DSPLY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_floc_dsply .
  DATA : l_function  TYPE zzfunction,
         l_ret_code  TYPE sy-subrc,
         l_prog_name TYPE sy-cprog.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.
    CALL TRANSACTION t_config-tcode.
  ELSE.
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_FLOC_DSPLY

*&---------------------------------------------------------------------*
*&      Form  SUB_FLOC_REPT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_floc_rept .

  DATA : l_function  TYPE zzfunction,
         l_ret_code  TYPE sy-subrc,
         l_program TYPE sy-cprog,
         x_batch_opt TYPE ctu_params.

  REFRESH t_bdcdata[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.
*    PERFORM sub_bdc_dynpro USING 'RIIFLO20' '1000'.
*    PERFORM sub_bdc_field  USING 'BDC_OKCODE' '/00'.
*    PERFORM sub_bdc_field  USING 'IWERK-LOW' 'P107'.
*
*    x_batch_opt-updmode = c_sync.
*    x_batch_opt-dismode = c_mode_error.
*    x_batch_opt-nobinpt = 'X'.

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
*       USING t_bdcdata
*       OPTIONS FROM x_batch_opt.
  ELSE.
    MESSAGE 'Tcode is not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_FLOC_REPT

*&---------------------------------------------------------------------*
*&      Form  SUB_EQN_DSPLY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_eqn_dsply .
  DATA : l_function  TYPE zzfunction,
         l_ret_code  TYPE sy-subrc,
         l_program TYPE sy-cprog.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_EQN_DSPLY
*&---------------------------------------------------------------------*
*&      Form  SUB_EQN_REPT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_eqn_rept .
  DATA : l_function  TYPE zzfunction,
         l_ret_code  TYPE sy-subrc,
         l_program TYPE sy-cprog,
         x_batch_opt TYPE ctu_params.

*  REFRESH t_bdcdata[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.
*    PERFORM sub_bdc_dynpro USING 'RIEQUI20' '1000'.
*    PERFORM sub_bdc_field  USING 'BDC_OKCODE' '/00'.
*    PERFORM sub_bdc_field  USING 'IWERK-LOW' 'P107'.
*
*    x_batch_opt-updmode = c_sync.
*    x_batch_opt-dismode = c_mode_error.
*    x_batch_opt-nobinpt = 'X'.

*    CALL TRANSACTION t_config-tcode.
*       USING t_bdcdata
*       OPTIONS FROM x_batch_opt.

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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_EQN_REPT

*&---------------------------------------------------------------------*
*&      Form  SUB_MAINT_BKLG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_maint_bklg .
  DATA :    l_function TYPE zzfunction,
            l_ret_code TYPE sy-subrc,
            lv_date TYPE sy-datum,
            l_program  TYPE sy-cprog.
  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.
    CLEAR : t_selparams.
    t_selparams-selname = 'AUART'.
    t_selparams-kind    = 'P'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'PM10'.
    APPEND t_selparams.

    CLEAR : t_selparams.
    t_selparams-selname = 'DY_OFN'. " outstanding
    t_selparams-kind    = 'P'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'X'.
    APPEND t_selparams.

    CLEAR : t_selparams.
    t_selparams-selname = 'DY_IAR'. " in process
    t_selparams-kind    = 'P'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'X'.
    APPEND t_selparams.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'IWERK'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'P107'.
    APPEND t_selparams.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'DATUB'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'BT'.
    t_selparams-low     = '99991231'.
    APPEND t_selparams.

    PERFORM sub_get_prog_name USING t_config-tcode
                                CHANGING l_program.

    PERFORM sub_submit_report TABLES t_selparams
                              USING  l_program
                                     t_config-repvar
                                     t_config-autoexec.
  ELSE.
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_MAINT_BKLG

*&---------------------------------------------------------------------*
*&      Form  SUB_ORDER_CNF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_order_cnf .
  DATA :    l_function TYPE zzfunction,
            l_ret_code TYPE sy-subrc,
            lv_date TYPE sy-datum,
            l_program  TYPE sy-cprog.
  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.
    CLEAR t_selparams.
    t_selparams-selname = 'STAI1'.
    t_selparams-kind    = 'S'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low = 'CNF'.
    APPEND t_selparams.

    CLEAR t_selparams.
    t_selparams-selname = 'STAE1'. " exclusive status
    t_selparams-kind    = 'S'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low = 'TECO'.
    APPEND t_selparams.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'IWERK'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'P107'.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'DATUV'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'BT'.
    t_selparams-low     = ''.
    APPEND t_selparams.

    CONSTANTS: lc_date TYPE sy-datum VALUE '99991231'.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'DATUB'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'BT'.
    t_selparams-low     =  lc_date.
    APPEND t_selparams.

    PERFORM sub_get_prog_name USING t_config-tcode
                                CHANGING l_program.

    PERFORM sub_submit_report TABLES t_selparams
                              USING  l_program
                                     t_config-repvar
                                     t_config-autoexec.
  ELSE.
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_ORDER_CNF

*&---------------------------------------------------------------------*
*&      Form  SUB_FL_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_fl_bom .
  DATA : l_function TYPE zzfunction,
           l_ret_code TYPE sy-subrc,
           lv_date TYPE sy-datum,
           l_program  TYPE sy-cprog.
  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_FL_BOM

*&---------------------------------------------------------------------*
*&      Form  SUB_MAT_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_mat_bom .
  DATA : l_function TYPE zzfunction,
           l_ret_code TYPE sy-subrc,
           lv_date TYPE sy-datum,
           l_program  TYPE sy-cprog.
  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_MAT_BOM

*&---------------------------------------------------------------------*
*&      Form  SUB_MAT_AVAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_mat_avai .
*Material Availability is transaction MMBE plant should default from the defaults section of ZCPMAIN.
  DATA : l_function TYPE zzfunction,
          l_ret_code TYPE sy-subrc,
          lv_date TYPE sy-datum,
          l_program  TYPE sy-cprog.
  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.
    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'MS_WERKS'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'P107'.
    APPEND t_selparams.

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
    MESSAGE 'Tcode is not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_MAT_AVAI

*&---------------------------------------------------------------------*
*&      Form  SUB_SCH_ORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_sch_ord .
** new code added by sghosh
  DATA :   l_program  TYPE sy-cprog,
           l_space    TYPE char1,
           l_function TYPE zzfunction,
           l_ret_code TYPE sy-subrc,
           lv_mon     TYPE sy-datum,
           lv_sun     TYPE sy-datum,
           lv_week    TYPE scal-week,
           lv_mon1    TYPE sy-datum,
           lv_sun1    TYPE sy-datum,
           lv_mon2    TYPE sy-datum,
           lv_sun2    TYPE sy-datum,
           lv_mon3    TYPE sy-datum,
           lv_sun6    TYPE sy-datum,
           lv_mon7    TYPE sy-datum,
           lv_sun7    TYPE sy-datum.

  CONSTANTS: lc_days1 TYPE t5a4a-dlydy VALUE '01',
             lc_days2 TYPE t5a4a-dlydy VALUE '06',
             lc_days3 TYPE t5a4a-dlydy VALUE '27'.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.

    CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
      EXPORTING
        date   = sy-datum
      IMPORTING
        week   = lv_week
        monday = lv_mon
        sunday = lv_sun.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_sun
        days      = lc_days1
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_mon1.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_mon1
        days      = lc_days2
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_sun1.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_sun1
        days      = lc_days1
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_mon2.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_mon2
        days      = lc_days2
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_sun2.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_sun2
        days      = lc_days1
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_mon3.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_mon3
        days      = lc_days3
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_sun6.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_sun6
        days      = lc_days1
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_mon7.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_mon7
        days      = lc_days2
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_sun7.

    IF w_tr = 'X'.

      IF  w_rev_code IS NOT INITIAL.
        CLEAR t_selparams.
        t_selparams-kind    = 'S'.
        t_selparams-selname = 'REVNR'.
        t_selparams-sign    = 'I'.
        t_selparams-option  = 'EQ'.
        t_selparams-low     =  w_rev_code.
        APPEND t_selparams.
      ENDIF.
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'DATUV'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    APPEND t_selparams.
*
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'DATUB'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    APPEND t_selparams.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_t0 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'GSTRP'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'BT'.
      t_selparams-low     = lv_mon.
      t_selparams-high    = lv_sun.
      APPEND t_selparams.

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'DATUV'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    APPEND t_selparams.
*
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'DATUB'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    APPEND t_selparams.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.

*    IF NOT w_rev_code IS INITIAL.
*      CLEAR t_selparams.
*      t_selparams-kind    = 'S'.
*      t_selparams-selname = 'REVNR'.
*      t_selparams-sign    = 'I'.
*      t_selparams-option  = 'EQ'.
*      t_selparams-low     =  w_rev_code.
*      APPEND t_selparams.
*    ENDIF.
    ENDIF.


    IF w_t1 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'GSTRP'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'BT'.
      t_selparams-low     = lv_mon1.
      t_selparams-high    = lv_sun1.
      APPEND t_selparams.

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'DATUV'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    APPEND t_selparams.
*
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'DATUB'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    APPEND t_selparams.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.

    ENDIF.


    IF w_t2 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'GSTRP'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'BT'.
      t_selparams-low     = lv_mon2.
      t_selparams-high    = lv_sun2.
      APPEND t_selparams.

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'DATUV'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    APPEND t_selparams.
*
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'DATUB'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    APPEND t_selparams.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.

    ENDIF.

    IF w_t3 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'GSTRP'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'BT'.
      t_selparams-low     = lv_mon3.
      t_selparams-high    = lv_sun6.
      APPEND t_selparams.

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'DATUV'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    APPEND t_selparams.
*
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'DATUB'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    APPEND t_selparams.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.

    ENDIF.

*Greater than 6th Week
    IF w_t6 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'GSTRP'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'GE'.
      t_selparams-low     = lv_mon7.
*    t_selparams-high    = lv_sun7.
      APPEND t_selparams.

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'DATUV'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    APPEND t_selparams.
*
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'DATUB'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    APPEND t_selparams.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_tall = 'X'.

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'DATUV'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    APPEND t_selparams.
*
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'DATUB'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    APPEND t_selparams.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    PERFORM sub_get_prog_name USING   t_config-tcode
                                  CHANGING l_program.

*-- By default the current program is opening IW38 and need to use tcode IW39. So, passing the tcode.
      CLEAR t_selparams.
      t_selparams-kind    = 'P'.
      t_selparams-selname = 'DY_TCODE'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = t_config-tcode.
      APPEND t_selparams.



    IF t_config-repvar IS NOT INITIAL.
      PERFORM sub_submit_report TABLES t_selparams
                                USING  l_program
                                       t_config-repvar
                                       t_config-autoexec.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.
  ELSE.
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_SCH_ORD

*&---------------------------------------------------------------------*
*&      Form  SUB_SCH_OPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_sch_ops .
** new code added by sghosh
  DATA :   l_program  TYPE sy-cprog,
           l_space    TYPE char1,
           l_function TYPE zzfunction,
           l_ret_code TYPE sy-subrc,
           lv_mon     TYPE sy-datum,
           lv_sun     TYPE sy-datum,
           lv_week    TYPE scal-week,
           lv_mon1    TYPE sy-datum,
           lv_sun1    TYPE sy-datum,
           lv_mon2    TYPE sy-datum,
           lv_sun2    TYPE sy-datum,
           lv_mon3    TYPE sy-datum,
           lv_sun6    TYPE sy-datum,
           lv_mon7    TYPE sy-datum,
           lv_sun7    TYPE sy-datum.

  CONSTANTS: lc_days1 TYPE t5a4a-dlydy VALUE '01',
             lc_days2 TYPE t5a4a-dlydy VALUE '06',
             lc_days3 TYPE t5a4a-dlydy VALUE '27'.
  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.

    CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
      EXPORTING
        date   = sy-datum
      IMPORTING
        week   = lv_week
        monday = lv_mon
        sunday = lv_sun.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_sun
        days      = lc_days1
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_mon1.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_mon1
        days      = lc_days2
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_sun1.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_sun1
        days      = lc_days1
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_mon2.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_mon2
        days      = lc_days2
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_sun2.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_sun2
        days      = lc_days1
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_mon3.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_mon3
        days      = lc_days3
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_sun6.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_sun6
        days      = lc_days1
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_mon7.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_mon7
        days      = lc_days2
        months    = '00'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_sun7.

    IF w_tr = 'X'.
      IF  w_rev_code IS NOT INITIAL.
        CLEAR t_selparams.
        t_selparams-kind    = 'S'.
        t_selparams-selname = 'S_REVNR'.
        t_selparams-sign    = 'I'.
        t_selparams-option  = 'EQ'.
        t_selparams-low     =  w_rev_code.
        APPEND t_selparams.
      ENDIF.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'S_DATUM'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    t_selparams-high    = ''.
*    APPEND t_selparams.
    ENDIF.

    IF w_t0 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_GSTRP'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'BT'.
      t_selparams-low     = lv_mon.
      t_selparams-high    = lv_sun.
      APPEND t_selparams.

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'S_DATUM'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    t_selparams-high    = ''.
*    APPEND t_selparams.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.

    ENDIF.


    IF w_t1 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_GSTRP'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'BT'.
      t_selparams-low     = lv_mon1.
      t_selparams-high    = lv_sun1.
      APPEND t_selparams.

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'S_DATUM'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    t_selparams-high    = ''.
*    APPEND t_selparams.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.

    ENDIF.


    IF w_t2 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_GSTRP'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'BT'.
      t_selparams-low     = lv_mon2.
      t_selparams-high    = lv_sun2.
      APPEND t_selparams.

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'S_DATUM'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    t_selparams-high    = ''.
*    APPEND t_selparams.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_t3 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_GSTRP'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'BT'.
      t_selparams-low     = lv_mon3.
      t_selparams-high    = lv_sun6.
      APPEND t_selparams.

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'S_DATUM'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    t_selparams-high    = '12/31/9999'.
*    APPEND t_selparams.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.

    ENDIF.

    IF w_t6 = 'X'.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_GSTRP'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'GE'.
      t_selparams-low     = lv_mon7.
      APPEND t_selparams.

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'S_DATUM'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    t_selparams-high    = ''.
*    APPEND t_selparams.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_tall = 'X'.

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'S_DATUM'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
*    t_selparams-high    = ''.
*    APPEND t_selparams.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.

    ENDIF.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'S_DATUM'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
    t_selparams-high    = '99991231'.
    APPEND t_selparams.

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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_SCH_OPS

*&---------------------------------------------------------------------*
*&      Form  SUB_OPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_ops .
  DATA :    l_function TYPE zzfunction,
             l_ret_code TYPE sy-subrc,
             lv_date TYPE sy-datum,
             l_program  TYPE sy-cprog.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'S_IWERK'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'P107'.
    APPEND t_selparams.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'S_DATUM'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'BT'.
*  t_selparams-low     = ''.
    t_selparams-high    = '99991231'.
    APPEND t_selparams.

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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_OPS

*&---------------------------------------------------------------------*
*&      Form  SUB_EQP_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_eqp_bom .
  DATA : l_function TYPE zzfunction,
           l_ret_code TYPE sy-subrc,
           lv_date TYPE sy-datum,
           l_program  TYPE sy-cprog.
  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_EQP_BOM
*&---------------------------------------------------------------------*
*&      Form  SUB_UNIT_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_unit_report .
  DATA :    l_function TYPE zzfunction,
              l_ret_code TYPE sy-subrc,
              lv_date TYPE sy-datum,
              l_program  TYPE sy-cprog.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.
    IF ( w_avail = 'X' AND w_s = 'X' )  OR
        ( w_s = 'X' AND w_uns = 'X' )  OR
        ( w_uns = 'X' AND w_avail = 'X' ) .

    ELSE.
      CLEAR t_selparams.
      t_selparams-kind    = 'P'.
      t_selparams-selname = 'CODGR'.
      t_selparams-low     = 'UNT-AVAL'."'UNT-STAT'.
      APPEND t_selparams.
      IF w_avail = 'X'.
        CLEAR t_selparams.
        t_selparams-kind    = 'P'.
        t_selparams-selname = 'VLCOD'.
        t_selparams-low     = 'A'.

        APPEND t_selparams.
      ELSEIF   w_s = 'X'.
        CLEAR t_selparams.
        t_selparams-kind    = 'P'.
        t_selparams-selname = 'VLCOD'.
        t_selparams-low     = 'B'.
        APPEND t_selparams.

      ELSEIF w_uns = 'X'.
        CLEAR t_selparams.
        t_selparams-kind    = 'P'.
        t_selparams-selname = 'VLCOD'.
        t_selparams-low     = 'C'.

        APPEND t_selparams.
      ENDIF.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_UNIT_REPORT
*&---------------------------------------------------------------------*
*&      Form  SUB_STATUS_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_status_report .
  DATA :    l_function TYPE zzfunction,
               l_ret_code TYPE sy-subrc,
               lv_date TYPE sy-datum,
               l_program  TYPE sy-cprog.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.
    IF ( w_online = 'X' AND w_nd = 'X' )  OR
      ( w_d = 'X' AND w_nd = 'X' )  OR
      ( w_online = 'X' AND w_d = 'X' ) .

    ELSE.
      CLEAR t_selparams.
      t_selparams-kind    = 'P'.
      t_selparams-selname = 'CODGR'.
      t_selparams-low     = 'UNT-STAT'.
      APPEND t_selparams.

      IF w_online = 'X'.
        CLEAR t_selparams.
        t_selparams-kind    = 'P'.
        t_selparams-selname = 'VLCOD'.
        t_selparams-low     = 'C'.

        APPEND t_selparams.
      ELSEIF   w_nd = 'X'.
        CLEAR t_selparams.
        t_selparams-kind    = 'P'.
        t_selparams-selname = 'VLCOD'.
        t_selparams-low     = 'B'.

        APPEND t_selparams.
      ELSEIF w_d = 'X'.
        CLEAR t_selparams.
        t_selparams-kind    = 'P'.
        t_selparams-selname = 'VLCOD'.
        t_selparams-low     = 'A'.

        APPEND t_selparams.
      ENDIF.

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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_STATUS_REPORT

*&---------------------------------------------------------------------*
*&      Form  SUB_PARTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_parts .
  DATA : l_function  TYPE zzfunction,
         l_ret_code  TYPE sy-subrc,
         l_program TYPE sy-cprog.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_PARTS
*&      Form  Call_Tcode_Varaint
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_tcode_varaint.

  DATA : l_function TYPE zzfunction,
         l_program  TYPE sy-cprog,
         l_ret_code TYPE sy-subrc.
  REFRESH : t_selparams.
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code
                               function = l_function.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode  " IW38
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003. " You are not authorized to use this function
      RETURN.
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
ENDFORM.                    "Call_Tcode_Varaint
