*&---------------------------------------------------------------------*
*&  Include           ZCPMAINTCOOF01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SUB_RUN_TRENDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM sub_run_trends  USING p_fcode.
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
    IF w_year = 'X'.

      CLEAR lv_date.
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL_SG'
        EXPORTING
          date      = sy-datum
          days      = '0000'
          months    = '0000'
          signum    = '-'
          years     = '0001' "1 year
        IMPORTING
          calc_date = lv_date.

      t_selparams-selname = 'ERDAT'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'BT'.
      t_selparams-low     = lv_date.
      t_selparams-high    = sy-datum.
      APPEND t_selparams.

    ELSEIF   w_month = 'X'.

      CLEAR lv_date.
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL_SG'
        EXPORTING
          date      = sy-datum
          days      = '0000'
          months    = '0001' " 1 month
          signum    = '-'
          years     = '0000'
        IMPORTING
          calc_date = lv_date.

      t_selparams-selname = 'ERDAT'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'BT'.
      t_selparams-low     = lv_date.
      t_selparams-high    = sy-datum.
      APPEND t_selparams.

    ELSEIF  w_week = 'X'.
      CLEAR lv_date.
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL_SG'
        EXPORTING
          date      = sy-datum
          days      = '0007'  " 1 week
          months    = '0000'
          signum    = '-'
          years     = '0000'
        IMPORTING
          calc_date = lv_date.

      t_selparams-selname = 'ERDAT'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'BT'.
      t_selparams-low     = lv_date.
      t_selparams-high    = sy-datum.
      APPEND t_selparams.

    ELSEIF sy-ucomm = 'F70' AND w_all = 'X'.

    ENDIF.

    IF w_r_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.

*  IF p_fcode = 'F69'." for activity
*    PERFORM sub_get_prog_name USING t_config-tcode
*                                    CHANGING l_program.
*
*    PERFORM sub_submit_report TABLES t_selparams
*                              USING  l_program
*                                     t_config-repvar
*                                     t_config-autoexec.
*  ELSE.
*    PERFORM sub_get_prog_name USING t_config-tcode
*                                  CHANGING l_program.
*
*    PERFORM sub_submit_report TABLES t_selparams
*                              USING  l_program
*                                     t_config-repvar
*                                     t_config-autoexec.
*  ENDIF.

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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_RUN_TRENDS

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
      RETURN.
    ENDIF.

    IF w_r_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_RES_ALL

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
      RETURN.
    ENDIF.

    IF w_r_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.
*-- remove the hardcoding. The order type should be picked up from the variant
*    t_selparams-selname = 'AUART'.
*    t_selparams-kind    = 'S'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'PM60'.
*    t_selparams-high    = ''.
*    APPEND t_selparams.

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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_RES_ENGR

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

    IF w_r_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_RES_OPS

*&---------------------------------------------------------------------*
*&      Form  sub_equip_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sub_equip_bom .
  IF  w_create = 'X'.
    CALL TRANSACTION 'IB01'.
  ELSEIF  w_change = 'X'.
    CALL TRANSACTION 'IB02'.
  ELSEIF  w_display = 'X'.
    CALL TRANSACTION 'IB03'.
  ENDIF.
ENDFORM.                    "sub_equip_bom

*&---------------------------------------------------------------------*
*&      Form  SUB_FL_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_fl_bom .
*  IF  w_create = 'X'.
*    CALL TRANSACTION 'IB11'.
*  ELSEIF  w_change = 'X'.
*    CALL TRANSACTION 'IB12'.
*  ELSEIF  w_display = 'X'.
*    CALL TRANSACTION 'IB13'.
*  ENDIF.
  IF w_r_sto = 'X'.
    SET PARAMETER ID 'WRK' FIELD 'P107'.
  ELSEIF w_r_smc = 'X'.
    SET PARAMETER ID 'WRK' FIELD 'P103'.
  ENDIF.
  IF  w_create = 'X'.
** new code added by sghosh
    DATA :   l_program  TYPE sy-cprog,
             l_space    TYPE char1,
             l_function TYPE zzfunction,
             l_ret_code TYPE sy-subrc.


    CLEAR t_config.
    READ TABLE t_config WITH KEY fcode    = ok_code
                                 function = 'CRET'.

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
      MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
    ENDIF.


  ELSEIF  w_change = 'X'.
    CLEAR t_config.
    READ TABLE t_config WITH KEY fcode    = ok_code
                                 function = 'CHNG'.

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
      MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
    ENDIF.


  ELSEIF  w_display = 'X'.
    CLEAR t_config.
    READ TABLE t_config WITH KEY fcode    = ok_code
                                 function = 'DISP'.

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
      MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
    ENDIF.
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
  IF w_create = 'X'.
    CALL TRANSACTION 'CS01'.
  ELSEIF w_change = 'X'.
    CALL TRANSACTION 'CS02'.
  ELSEIF   w_display = 'X'.
    CALL TRANSACTION 'CS03'.
  ENDIF.
ENDFORM.                    "sub_mat_bom

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
    PERFORM sub_read_default_values.

*fill default from ZCPMAIN based on user
*  IF x_defaults-zzunit1 IS NOT INITIAL .

*--  Populate parameter PLANNING PLANT
    IF w_r_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'MS_WERKS'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'MS_WERKS'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.
*
*    PERFORM sub_get_prog_name USING t_config-tcode
*                                CHANGING l_program.
**  PERFORM sub_get_prog_name USING 'MMBE'
**                              CHANGING l_program.
*
*    PERFORM sub_submit_report TABLES t_selparams
*                              USING  l_program
*                                     t_config-repvar
*                                     t_config-autoexec.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_MAT_AVAI

*&---------------------------------------------------------------------*
*&      Form  SUB_NEW_NOTIFICATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_new_notification .
  DATA : l_ret_code  TYPE sy-subrc,
          l_program TYPE sy-cprog,
          lv_date TYPE sy-datum,
          l_space     TYPE char1.
  CLEAR lv_date.

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
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL_SG'
      EXPORTING
        date      = sy-datum
        days      = '0003' " 3days
        months    = '0000'
        signum    = '-'
        years     = '0000'
      IMPORTING
        calc_date = lv_date.

    CLEAR : t_selparams.
    t_selparams-selname = 'ERDAT'.
    t_selparams-kind    = 'S'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'BT'.
    t_selparams-low     = lv_date.
    t_selparams-high    = sy-datum.
    APPEND t_selparams.

    IF w_r_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_NEW_NOTIFICATION

*&---------------------------------------------------------------------*
*&      Form  SUB_ORDER_CNF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_order_cnf .
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

    IF ok_code = 'F58'.
**Orders CNF not TECO is IW38 with a status inclusive of CNF and status exclusive of TECO
      CLEAR t_selparams.
      t_selparams-selname = 'STAI1'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'CNF'.
      APPEND t_selparams.

      CLEAR t_selparams.
      t_selparams-selname = 'STAE1'. " exclusive status
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'TECO'.
      APPEND t_selparams.

    ELSEIF ok_code = 'F57'. " only hold

*    CLEAR t_selparams.
*    t_selparams-selname = 'STAI1'.
*    t_selparams-kind    = 'S'.
*    t_selparams-sign    = 'I'.
*    t_selparams-low = 'HOLD'.
*    APPEND t_selparams.

    ENDIF.

    IF w_r_sto = 'X' .
*--  Populate parameter PLANNING PLANT
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_ORDER_CNF

*&---------------------------------------------------------------------*
*&      Form  SUB_MAINT_BKLG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_maint_bklg .
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
*Corrective Maint BKLG is IW38 with PM10 order type and outstanding and in process checked
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

    IF w_r_sto = 'X' .

*--  Populate parameter PLANNING PLANT
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_MAINT_BKLG

*&---------------------------------------------------------------------*
*&      Form  SUB_ORDERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_orders .
*** new code added by sghosh
*  DATA :   l_program  TYPE sy-cprog,
*           l_space    TYPE char1,
*           l_function TYPE zzfunction,
*           l_ret_code TYPE sy-subrc.
*
**- checks whether user is authorised to use called transaction *-
*  PERFORM sub_check_tcode_authority USING    c_tcode_iw3d   " IW3D
*                                    CHANGING l_ret_code.
*  IF l_ret_code <> c_authority_ok.
*    MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
*    RETURN.
*  ELSE.
*    CALL TRANSACTION c_tcode_iw3d.
*
*  ENDIF.
  DATA : l_function TYPE zzfunction,
         l_program  TYPE sy-cprog,
         l_ret_code TYPE sy-subrc.
  REFRESH : t_selparams.
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

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


ENDFORM.                    "sub_orders

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

    IF w_r_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_NOTIF_BATCH

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
    IF w_r_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_ORDERS_BATCH

*&---------------------------------------------------------------------*
*&      Form  sub_tasklist
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sub_tasklist .
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
    IF w_r_sto = 'X'.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'PN_WERKS'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'PN_WERKS'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_ORDERS_BATCH

*&---------------------------------------------------------------------*
*&      Form  SUB_RUN_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_run_report .
  DATA : l_function TYPE zzfunction,
           l_ret_code TYPE sy-subrc,
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

    IF w_r_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'SWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'SWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.

    IF chk18 = 'X'.   "init-created
      CLEAR t_selparams.
      t_selparams-selname = 'STAI1'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'NEW'.
      APPEND t_selparams.
    ENDIF.

    IF chk19 = 'X'. "retur-add data req
      CLEAR t_selparams.
      t_selparams-selname = 'STAI1'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'RETU'.
      APPEND t_selparams.
    ENDIF.

    IF chk43 = 'X'. "appr-approved
      CLEAR t_selparams.
      t_selparams-selname = 'STAI1'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'APPR'.
      APPEND t_selparams.
    ENDIF.

    IF chk44 = 'X'. "dupl-duplicate
      CLEAR t_selparams.
      t_selparams-selname = 'STAI1'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'DUPL'.
      APPEND t_selparams.
    ENDIF.

    IF chk45 = 'X'. "canc-cancelled
      CLEAR t_selparams.
      t_selparams-selname = 'STAI1'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'CANC'.
      APPEND t_selparams.
    ENDIF.

*-- By default the current program is opening IW38 and need to use tcode IW39. So, passing the tcode.
      CLEAR t_selparams.
      t_selparams-kind    = 'P'.
      t_selparams-selname = 'DY_TCODE'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = t_config-tcode.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_RUN_REPORT

*&---------------------------------------------------------------------*
*&      Form  sub_op_run_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sub_op_run_report .
  DATA : l_function TYPE zzfunction,
           l_ret_code TYPE sy-subrc,
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

    IF w_chk11 = 'X'.   "created
      CLEAR t_selparams.
      t_selparams-selname = 'S_VSTAIN'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'CRTD'.
      APPEND t_selparams.
    ENDIF.

    IF w_chk12 = 'X'. "DISP
      CLEAR t_selparams.
      t_selparams-selname = 'S_VSTAIN'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'DISP'.
      APPEND t_selparams.
    ENDIF.

    IF w_chk13 = 'X'. "ope. started
      CLEAR t_selparams.
      t_selparams-selname = 'S_VSTAIN'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'STRT'.
      APPEND t_selparams.
    ENDIF.

    IF w_chk14 = 'X'. "hold
      CLEAR t_selparams.
      t_selparams-selname = 'S_VSTAIN'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'HOLD'.
      APPEND t_selparams.
    ENDIF.

    IF w_chk15 = 'X'. "RTRN
      CLEAR t_selparams.
      t_selparams-selname = 'S_VSTAIN'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'RTRN'.
      APPEND t_selparams.
    ENDIF.

    IF w_chk16 = 'X'. "RSTR
      CLEAR t_selparams.
      t_selparams-selname = 'S_VSTAIN'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'RSTR'.
      APPEND t_selparams.
    ENDIF.

    IF w_arbpl IS NOT INITIAL.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_VARBPL'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = w_arbpl.
      APPEND t_selparams.
    ENDIF.


    IF w_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    "sub_op_run_report

*&---------------------------------------------------------------------*
*&      Form  SUB_CREATE_GTL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_create_gtl .
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
    CALL TRANSACTION t_config-tcode.
  ELSE.
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_CREATE_GTL

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
*  DATA :   l_program  TYPE sy-cprog,
*           l_space    TYPE char1,
*           l_function TYPE zzfunction,
*           l_ret_code TYPE sy-subrc.

*- checks whether user is authorised to use called transaction *-
*  PERFORM sub_check_tcode_authority USING    c_tcode_iw22   " IW22
*                                    CHANGING l_ret_code.
*  IF l_ret_code <> c_authority_ok.
*    MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
*    RETURN.
*  ELSE.
*    CALL TRANSACTION c_tcode_iw22.
*  ENDIF.

  DATA : l_function TYPE zzfunction,
           l_program  TYPE sy-cprog,
           l_ret_code TYPE sy-subrc.
  REFRESH : t_selparams.
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

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

ENDFORM.                    " SUB_NOTIF_PRTSHP

*&---------------------------------------------------------------------*
*&      Form  SUB_PERMIT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_permit.
  DATA :   l_ret_code  TYPE sy-subrc,
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
    IF w_r_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.

*-- By default the current program is opening IW38 and need to use tcode IW39. So, passing the tcode.
      CLEAR t_selparams.
      t_selparams-kind    = 'P'.
      t_selparams-selname = 'DY_TCODE'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = t_config-tcode.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_PERMIT_ALL

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

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'DATUB'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = '99991231'.
*    APPEND t_selparams.
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

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'IWERK'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'P103'.
*    APPEND t_selparams.

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

*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'IWERK'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'P103'.
*    APPEND t_selparams.

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
*
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'IWERK'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'P103'.
*    APPEND t_selparams.

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
*
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'IWERK'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'P103'.
*    APPEND t_selparams.

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
*
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'IWERK'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'P103'.
*    APPEND t_selparams.
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
*
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'IWERK'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'P103'.
*    APPEND t_selparams.
    ENDIF.
    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'DATUB'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'BT'.
    t_selparams-low     = '99991231'.
    APPEND t_selparams.

    IF w_r_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.

*    PERFORM sub_get_prog_name USING t_config-tcode
*                                  CHANGING l_program.
**  PERFORM sub_get_prog_name USING c_tcode_iw38
**                                CHANGING l_program.
*    PERFORM sub_submit_report TABLES t_selparams
*                              USING  l_program
*                                     t_config-repvar
*                                     t_config-autoexec.

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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
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


    ENDIF.

    IF w_t6 = 'X'.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_GSTRP'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'GE'.
      t_selparams-low     = lv_mon7.
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
*
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'S_IWERK'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'P103'.
*    APPEND t_selparams.

    ENDIF.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'S_DATUM'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
    t_selparams-high    = '99991231'.
    APPEND t_selparams.

    IF w_r_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.

*-- By default the current program is opening IW38 and need to use tcode IW39. So, passing the tcode.
      CLEAR t_selparams.
      t_selparams-kind    = 'P'.
      t_selparams-selname = 'DY_TCODE'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = t_config-tcode.
      APPEND t_selparams.

*    PERFORM sub_get_prog_name USING t_config-tcode
*                                  CHANGING l_program.
*
**  PERFORM sub_get_prog_name USING c_tcode_iw37n
**                                CHANGING l_program.
*
*    PERFORM sub_submit_report TABLES t_selparams
*                              USING  l_program
*                                     t_config-repvar
*                                     t_config-autoexec.

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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_SCH_OPS
*&---------------------------------------------------------------------*
*&      Form  sub_dispatch
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sub_dispatch .
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
      RETURN.
    ENDIF.

    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'S_DATUM'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'BT'.
*    t_selparams-low     = ''.
    t_selparams-high    = '99991231'.
    APPEND t_selparams.

    IF w_r_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.

*-- By default the current program is opening IW38 and need to use tcode IW39. So, passing the tcode.
      CLEAR t_selparams.
      t_selparams-kind    = 'P'.
      t_selparams-selname = 'DY_TCODE'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = t_config-tcode.
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    "sub_dispatch

*&---------------------------------------------------------------------*
*&      Form  SUB_RUN_ORDERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_run_orders .
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
      RETURN.
    ENDIF.

    IF w_r_sto = 'X'.

      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_chk1 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'STAI1'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'RDPL'.
      APPEND t_selparams.
    ENDIF.
    IF w_chk2 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'STAI1'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'PIPR'.
      APPEND t_selparams.
    ENDIF.
    IF w_chk3 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'STAI1'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'RTSC'.
      APPEND t_selparams.
    ENDIF.
    IF w_chk4 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'STAI1'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'SCHD'.
      APPEND t_selparams.
    ENDIF.
    IF w_chk5 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'STAI1'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'ASGN'.
      APPEND t_selparams.
    ENDIF.
    IF w_chk6 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'STAI1'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'WRKG'.
      APPEND t_selparams.
    ENDIF.
    IF w_chk8 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'STAI1'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'SRVR'.
      APPEND t_selparams.
    ENDIF.
    IF w_chk9 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'STAI1'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'RWVS'.
      APPEND t_selparams.
    ENDIF.
    IF w_chk10 = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'STAI1'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'CANC'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_onmob = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'STAI1'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'MOBI'.
      APPEND t_selparams.
    ELSEIF w_r_ntmob = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'STAE1'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'MOBI'.
      APPEND t_selparams.
    ENDIF.

    IF w_r_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.

*  IF w_chk1 = 'X'.
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'STAI1'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'RDPL'.
*    APPEND t_selparams.
*  ENDIF.
*  IF w_chk2 = 'X'.
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'STAI1'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'PIPR'.
*    APPEND t_selparams.
*  ENDIF.
*  IF w_chk3 = 'X'.
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'STAI1'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'RTSC'.
*    APPEND t_selparams.
*  ENDIF.
*  IF w_chk4 = 'X'.
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'STAI1'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'SCHD'.
*    APPEND t_selparams.
*  ENDIF.
*  IF w_chk5 = 'X'.
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'STAI1'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'ASGN'.
*    APPEND t_selparams.
*  ENDIF.
*
*  IF w_chk6 = 'X'.
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'STAI1'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'WRKG'.
*    APPEND t_selparams.
*  ENDIF.
*
*  IF w_chk8 = 'X'.
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'STAI1'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'SRVR'.
*    APPEND t_selparams.
*  ENDIF.
*
*  IF w_chk9 = 'X'.
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'STAI1'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'RWVS'.
*    APPEND t_selparams.
*  ENDIF.
*
*  IF w_chk10 = 'X'.
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'STAI1'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'CANC'.
*    APPEND t_selparams.
*  ENDIF.
*
*
*  IF w_r_onmob = 'X'.
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'STAI1'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'MOBI'.
*    APPEND t_selparams.
*
*  ELSEIF w_r_ntmob = 'X'.
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'STAE1'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'MOBI'.
*    APPEND t_selparams.
*  ENDIF.

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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_RUN_ORDERS
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
