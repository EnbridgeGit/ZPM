*----------------------------------------------------------------------*
***INCLUDE ZCPENG_GLOBAL .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_REPORT_PROBLEM
*&---------------------------------------------------------------------*
*       Handle push button action "Report a Problem"
*----------------------------------------------------------------------*
FORM sub_handle_report_problem .
**new code added jsharma
  DATA x_batch_opt TYPE ctu_params.
  REFRESH t_bdcdata.
  DATA :   l_program  TYPE sy-cprog,
             l_space    TYPE char1,
             l_function TYPE zzfunction,
             l_ret_code TYPE sy-subrc.
*set PARAMETER ID 'QMR' FIELD 'M1'.
*CALL TRANSACTION 'IW21'.
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.

    IF t_config-tcode = 'IW21'.
      PERFORM sub_bdc_dynpro USING 'SAPLIQS0' '0100'.
      PERFORM sub_bdc_field  USING 'BDC_OKCODE' '/00'.
      PERFORM sub_bdc_field  USING 'RIWO00-QMART' 'M1'.

      x_batch_opt-updmode = c_sync.
      x_batch_opt-dismode = c_mode_error.
      x_batch_opt-nobinpt = 'X'.

      CALL TRANSACTION 'IW21'
           USING t_bdcdata
           OPTIONS FROM x_batch_opt.


*- checks whether user is authorised to use called transaction *-
      PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                        CHANGING l_ret_code.
      IF l_ret_code <> c_authority_ok.
        MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
        RETURN.
      ENDIF.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.

  ELSE.
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
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
  DATA x_batch_opt TYPE ctu_params.
  REFRESH t_bdcdata.
  DATA :   l_program  TYPE sy-cprog,
             l_space    TYPE char1,
             l_function TYPE zzfunction,
             l_ret_code TYPE sy-subrc.

  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.

    IF t_config-tcode = 'IW21'.
      PERFORM sub_bdc_dynpro USING 'SAPLIQS0' '0100'.
      PERFORM sub_bdc_field  USING 'BDC_OKCODE' '/00'.
      PERFORM sub_bdc_field  USING 'RIWO00-QMART' 'M4'.

      x_batch_opt-updmode = c_sync.
      x_batch_opt-dismode = c_mode_error.
      x_batch_opt-nobinpt = 'X'.

      CALL TRANSACTION 'IW21'
           USING t_bdcdata
           OPTIONS FROM x_batch_opt.


*- checks whether user is authorised to use called transaction *-
      PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                        CHANGING l_ret_code.
      IF l_ret_code <> c_authority_ok.
        MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
        RETURN.
      ENDIF.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.

  ELSE.
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_HANDLE_MASTER_DATA
*&---------------------------------------------------------------------*
*&      Form  sub_start_url
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_URL      text
*----------------------------------------------------------------------*
FORM sub_start_url USING p_url.

  CALL FUNCTION 'CALL_BROWSER'
    EXPORTING
      url                    = p_url
    EXCEPTIONS
      frontend_not_supported = 1
      frontend_error         = 2
      prog_not_found         = 3
      no_batch               = 4
      unspecified_error      = 5
      OTHERS                 = 6.

  IF sy-subrc <> 0.
    MESSAGE 'Can not start web browser' TYPE 'I'.
  ENDIF.

ENDFORM.                    "sub_start_url
*&---------------------------------------------------------------------*
*&      Form  SUB_BDC_DYNPRO
*&---------------------------------------------------------------------*
*       Populate Batch Data screen information
*----------------------------------------------------------------------*
*      -->P_PROG    Program
*      -->P_DYNNR   Screen Number
*----------------------------------------------------------------------*
FORM sub_bdc_dynpro USING p_prog p_dynnr.
  CLEAR t_bdcdata.
  t_bdcdata-program  = p_prog.
  t_bdcdata-dynpro   = p_dynnr.
  t_bdcdata-dynbegin = 'X'.
  APPEND t_bdcdata.
ENDFORM.                    " SUB_BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  SUB_BDC_FIELD
*&---------------------------------------------------------------------*
*       Populate batch data field information
*----------------------------------------------------------------------*
*      -->P_FNAM   Field Name
*      -->P_FVAL   Field Value
*----------------------------------------------------------------------*
FORM sub_bdc_field  USING p_fnam p_fval.
  CLEAR t_bdcdata.
  t_bdcdata-fnam = p_fnam.
  t_bdcdata-fval = p_fval.
  APPEND t_bdcdata.
ENDFORM.                    " SUB_BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  NEW_NOTIFICATION
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

    IF w_sto = 'X' .

*--  Populate parameter PLANNING PLANT
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_smc = 'X'.
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

ENDFORM                    "sub_new_notification
.                    " NEW_NOTIFICATION
*&---------------------------------------------------------------------*
*&      Form  SUB_GET_PROG_NAME
*&---------------------------------------------------------------------*
*       Get program name attached to a T-Code
*----------------------------------------------------------------------*
*      -->P_TCODE  Transaction Code
*      <--P_CPROG  Attached program name
*----------------------------------------------------------------------*
FORM sub_get_prog_name  USING    p_tcode TYPE sy-tcode
                        CHANGING p_cprog.

  SELECT SINGLE pgmna
         FROM tstc
         INTO p_cprog
         WHERE tcode = p_tcode.

ENDFORM.                    " SUB_GET_PROG_NAME
*&---------------------------------------------------------------------*
*&      Form  sub_submit_report
*&---------------------------------------------------------------------*
*       Submit Report Program with selection screen variant and
*       selection screen parameters
*----------------------------------------------------------------------*
*      -->P_T_SELPARAMS  Internal Table - Seltion Parameters
*      -->P_CPROG        Program Name
*      -->P_VARIANT      Selection Screen Variant
*----------------------------------------------------------------------*
FORM sub_submit_report TABLES p_t_selparams STRUCTURE rsparams
                       USING  p_cprog p_variant p_selscr.
  IF p_selscr IS INITIAL.
    SUBMIT (p_cprog) USING SELECTION-SET p_variant
                     WITH SELECTION-TABLE p_t_selparams
                     VIA SELECTION-SCREEN AND RETURN.
  ELSE.
    SUBMIT (p_cprog) USING SELECTION-SET p_variant
                     WITH SELECTION-TABLE p_t_selparams
                     VIA SELECTION-SCREEN AND RETURN.
  ENDIF.

ENDFORM.                    "sub_submit_report
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
    IF w_chk = 'X'.
      CLEAR t_selparams.
      t_selparams-selname = 'STAI1'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'NEW'.
      t_selparams-option = 'EQ'.
      APPEND t_selparams.
    ENDIF.

    IF w_x_retu = 'X'.
      CLEAR t_selparams.
      t_selparams-selname = 'STAI1'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'RETU'.
      t_selparams-option = 'EQ'.
      APPEND t_selparams.
    ENDIF.

    IF w_x_appr = 'X'.
      CLEAR t_selparams.
      t_selparams-selname = 'STAI1'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'APPR'.
      t_selparams-option = 'EQ'.
      APPEND t_selparams.
    ENDIF.

    IF w_x_dupl = 'X'.
      CLEAR t_selparams.
      t_selparams-selname = 'STAI1'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'DUPL'.
      t_selparams-option = 'EQ'.
      APPEND t_selparams.
    ENDIF.

    IF w_x_canc = 'X'.
      CLEAR t_selparams.
      t_selparams-selname = 'STAI1'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-low = 'CANC'.
      t_selparams-option = 'EQ'.
      APPEND t_selparams.
    ENDIF.



    CLEAR t_selparams.
    t_selparams-selname = 'QMART'.
    t_selparams-kind    = 'S'.
    t_selparams-sign    = 'I'.
    t_selparams-low = 'M4'.
    t_selparams-option = 'EQ'.
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
*&      Form  SUB_RUN_TRENDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0077   text
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
    ELSEIF sy-ucomm = 'RAD' AND w_all = 'X'.

*    CLEAR lv_date.
*    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL_SG'
*      EXPORTING
*        date      = sy-datum
*        days      = '0000'
*        months    = '0012' " 1year
*        signum    = '-'
*        years     = '0000'
*      IMPORTING
*        calc_date = lv_date.
*
*    t_selparams-selname = 'ERDAT'.
*    t_selparams-kind    = 'S'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'BT'.
*    t_selparams-low     = lv_date.
*    t_selparams-high    = sy-datum.
*    APPEND t_selparams.
    ENDIF.

    IF w_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.

*  IF p_fcode = 'F44'." for activity
*    PERFORM sub_get_prog_name USING 'IW65'
*                                    CHANGING l_program.
*
*    PERFORM sub_submit_report TABLES t_selparams
*                              USING  l_program
*                                     t_config-repvar
*                                     t_config-autoexec.
*  ELSE.
*    PERFORM sub_get_prog_name USING 'IW68'
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
*Orders CNF not TECO is IW38 with a status inclusive of CNF and status exclusive of TECO
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

    IF w_sto = 'X' .

*--  Populate parameter PLANNING PLANT
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_smc = 'X'.
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
    IF w_sto = 'X' .

*--  Populate parameter PLANNING PLANT
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_smc = 'X'.
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
*&      Form  SUB_FL_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_fl_bom .
* There should probably be a create, change, or display radio button for the FL BOM and Material BOM buttons.
*  FLBOM is IB11, 12, 13. Material BOM is CS01, 02, 03. We will need a variant for each.
*  REFRESH: t_selparams[].
*  IF w_create = 'X'.
*    CALL TRANSACTION 'IB11'.
*  ELSEIF  w_change = 'X'.
*    CALL TRANSACTION 'IB12'.
*  ELSEIF  w_display = 'X'.
*    CALL TRANSACTION 'IB13'.
*  ENDIF.
** new code added by sghosh
  IF  w_create = 'X'.
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
  DATA : l_function TYPE zzfunction,
            l_ret_code TYPE sy-subrc,
            lv_date TYPE sy-datum,
            l_program  TYPE sy-cprog.
  REFRESH: t_selparams[].
  IF  w_create = 'X'.
*    CLEAR t_config.
*    READ TABLE t_config WITH KEY fcode    = ok_code.
    CALL TRANSACTION 'CS01'.
  ELSEIF   w_change = 'X'.

    CALL TRANSACTION 'CS02'.
  ELSEIF  w_display = 'X'.

    CALL TRANSACTION 'CS03'.

  ENDIF.
ENDFORM.                    " SUB_MAT_BOM
*&---------------------------------------------------------------------*
*&      Form  SUB_equip_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_equip_bom .
  DATA : l_function TYPE zzfunction,
            l_ret_code TYPE sy-subrc,
            lv_date TYPE sy-datum,
            l_program  TYPE sy-cprog.
  REFRESH: t_selparams[].
  IF  w_create = 'X'.

    CALL TRANSACTION 'IB01'.
  ELSEIF   w_change = 'X'.

    CALL TRANSACTION 'IB02'.
  ELSEIF  w_display = 'X'.

    CALL TRANSACTION 'IB03'.

  ENDIF.
ENDFORM.                    " SUB_equip_BOM
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
*  PERFORM sub_read_default_values.

    IF w_sto = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'MS_WERKS'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'MS_WERKS'.
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
ENDFORM.                    " SUB_MAT_AVAI
*&---------------------------------------------------------------------*
*&      Form  SUB_READ_DEFAULT_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_read_default_values .
  DATA : l_key TYPE indx-srtfd.
  SELECT SINGLE *
         INTO CORRESPONDING FIELDS
         OF x_nt_defaults
         FROM zntdeflt
         WHERE uname = sy-uname.

  IF sy-subrc = 0.
    x_defaults = x_nt_defaults.
  ENDIF.


ENDFORM.                    " SUB_READ_DEFAULT_VALUES


*&---------------------------------------------------------------------*
*&      Form  SUB_PRINT_NOT_BATCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_print_not_batch .
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
    IF w_sto = 'X' .

*--  Populate parameter PLANNING PLANT
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_smc = 'X'.
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
ENDFORM.                    " SUB_PRINT_NOT_BATCH


*&---------------------------------------------------------------------*
*&      Form  SUB_PRINT_ORD_BATCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_print_ord_batch .
  DATA : l_function TYPE zzfunction,
          l_ret_code TYPE sy-subrc,
          lv_date TYPE sy-datum,
          l_program  TYPE sy-cprog.
  CLEAR t_config.

  REFRESH t_selparams[].
  IF w_sto = 'X' .

*--  Populate parameter PLANNING PLANT
    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'IWERK'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'P107'.
    APPEND t_selparams.
  ENDIF.

  IF w_smc = 'X'.
    CLEAR t_selparams.
    t_selparams-kind    = 'S'.
    t_selparams-selname = 'IWERK'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'P103'.
    APPEND t_selparams.
  ENDIF.
  READ TABLE t_config WITH KEY fcode    = ok_code.

*  PERFORM sub_get_prog_name USING 'IW38'
*                              CHANGING l_program.
*
*  PERFORM sub_submit_report TABLES t_selparams
*                            USING  l_program
*                                   t_config-repvar
*                                   t_config-autoexec.

  IF t_config-tcode IS NOT INITIAL.
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

ENDFORM.                    " SUB_PRINT_ORD_BATCH

*&---------------------------------------------------------------------*
*&      Form  Sub_operations
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sub_operations.
*   ** new code added by sghosh
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
    IF w_sto = 'X' .

*--  Populate parameter PLANNING PLANT
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.

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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    "Sub_operations
*&---------------------------------------------------------------------*
*&      Form  SUB_REST_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_rest_all .

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
    IF w_sto = 'X' .
*--  Populate parameter PLANNING PLANT
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.

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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_REST_ALL
*&---------------------------------------------------------------------*
*&      Form  SUB_ENG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_eng .

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
    IF w_sto = 'X' .

*--  Populate parameter PLANNING PLANT
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF w_smc = 'X'.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.

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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_ENG

*&---------------------------------------------------------------------*
*&      Form  SUB_CALL_TRANSC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_call_transc .
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
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_CALL_TRANSC
