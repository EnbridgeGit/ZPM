*&---------------------------------------------------------------------*
*&  Include           ZCPMAIN_MYWORK_FRAME
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SUB_NOTIFICATION_TASKS
*&---------------------------------------------------------------------*
*       Handle action of push button Notification Tasks
*----------------------------------------------------------------------*
FORM sub_notification_tasks.

  DATA : l_program  TYPE sy-cprog,
         l_space    TYPE char1,
         l_function TYPE zzfunction,
         l_ret_code TYPE sy-subrc.

*- check which radio button is selected *-
*- call transaction accordingly *-
  CASE 'X'.
    WHEN w_x_open.      "  Open
      l_function = c_func_open.

    WHEN w_x_complete.  "  Complete
      l_function = c_func_comp.

    WHEN w_x_all.  "  all
      l_function = c_func_all.

  ENDCASE.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code
                               function = l_function.
  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003. " You are not authorized to use this function
      RETURN.
    ENDIF.

    PERFORM sub_populate_seltab USING 'ARBPL' 'STRNO'
                                       space  'QMART'
                                       space.

*--  Populate parameter Created by
    CLEAR t_selparams.
    t_selparams-kind    = 'P'.
    t_selparams-selname = 'M_PARNR'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = sy-uname.
    APPEND t_selparams.

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

    PERFORM sub_get_prog_name USING    t_config-tcode
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


ENDFORM.                    " SUB_NOTIFICATION_TASKS
*&---------------------------------------------------------------------*
*&      Form  SUB_ORDER_OPERATIONS
*&---------------------------------------------------------------------*
*       Handle action of push button Order Operations
*----------------------------------------------------------------------*
FORM sub_order_operations .

  DATA : l_function TYPE zzfunction,
         l_ret_code TYPE sy-subrc,
         l_program  TYPE sy-cprog.

  REFRESH :t_selparams[].
  CLEAR t_config.

*- check which radio button is selected *-
*- call transaction accordingly *-
  CASE 'X'.
    WHEN w_x_open.      " Open
      l_function = c_func_open.

    WHEN w_x_complete.  " Complete
      l_function = c_func_comp.
    WHEN w_x_all.  "  all
      l_function = c_func_all.
  ENDCASE.

  READ TABLE t_config WITH KEY fcode    = ok_code
                               function = l_function.
  IF t_config-tcode IS NOT INITIAL.

**- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING   t_config-tcode  " IW37N
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003. " You are not authorized to use this function
      RETURN.
    ENDIF.

*- check successful,submit program with values from main screen *-
*- populate employee *-
    PERFORM sub_populate_seltab USING 'S_VARBPL' 'S_STRNO'
                                      'S_AUART'   space
                                      'S_VPERNR'.

    IF x_defaults-zzunit1 IS NOT INITIAL .

*--  Populate parameter PLANNING PLANT
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF x_defaults-zzunit2 IS NOT INITIAL.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'S_IWERK'.
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
        t_selparams-selname = 'S_ERDAT'.
        t_selparams-kind    = 'S'.
        APPEND t_selparams.
      ENDLOOP.

    ENDIF.

    PERFORM sub_get_prog_name USING  t_config-tcode
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
ENDFORM.                    " SUB_ORDER_OPERATIONS
*&---------------------------------------------------------------------*
*&      Form  SUB_NOTIFICATION_I_CREATED
*&---------------------------------------------------------------------*
*       Handle action of push button Notifications I Created
*----------------------------------------------------------------------*
FORM sub_notification_i_created .

  DATA : l_function TYPE zzfunction,
         l_ret_code TYPE sy-subrc,
         l_program  TYPE sy-cprog.
*- check which radio button is selected *-
*- call transaction accordingly *-
  CASE 'X'.
    WHEN w_x_open.      "open
      l_function = c_func_open.

    WHEN w_x_complete.  "complete
      l_function = c_func_comp.

    WHEN w_x_all.  "  all
      l_function = c_func_all.
  ENDCASE.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code
                               function = l_function.

  IF t_config-tcode IS NOT INITIAL.
**- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " IW28
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003. " You are not authorized to use this function
      RETURN.
    ENDIF.

**- check successful,submit program with values from main screen *-
**- populate Functional Location *-
    PERFORM sub_populate_seltab USING 'ARBPL' 'STRNO'
                                       space  'QMART'
                                       space.

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
ENDFORM.                    " SUB_NOTIFICATION_I_CREATED
*&---------------------------------------------------------------------*
*&      Form  SUB_ENTER_TIMESHEET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_enter_timesheet .

  DATA : l_ret_code TYPE sy-subrc.


  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.
*                               function = l_function.
  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " CAT2
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003. " You are not authorized to use this function
      RETURN.
    ENDIF.

    PERFORM sub_call_tcode USING t_config-tcode                 " CAT2
                                 space.
  ELSE.
    MESSAGE 'Tcode is not exist in the table ZNTCONFIG' TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_ENTER_TIMESHEET
*&---------------------------------------------------------------------*
*&      Form  SUB_EXPENSE_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_expense_report .
  DATA :      l_ret_code TYPE sy-subrc,
              l_pernr TYPE pa0105-pernr.

  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " PR04
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003. " You are not authorized to use this function
      RETURN.
    ENDIF.
*- call program for tcode PR04 *-
    PERFORM sub_get_employee USING    sy-uname
                             CHANGING l_pernr.
    SET PARAMETER ID 'PER' FIELD l_pernr.
    CALL TRANSACTION t_config-tcode.
  ELSE.
    MESSAGE 'Tcode is not exist in the table ZNTCONFIG' TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_EXPENSE_REPORT
*&---------------------------------------------------------------------*
*&      Form  SUB_GET_EMPLOYEE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      <--P_L_PERNR  text
*----------------------------------------------------------------------*
FORM sub_get_employee  USING    p_uname
                       CHANGING p_pernr.

  CONSTANTS:c_0001(4) TYPE c VALUE '0001'.

  DATA:BEGIN OF t_pa0105 OCCURS 0,
        pernr TYPE pa0105-pernr,
        begda TYPE pa0105-begda,
        endda TYPE pa0105-endda,
      END OF t_pa0105.

  CLEAR p_pernr.

*- get all employees with the same user ID *-
*- Ideally there should be one employee for each ID *-
*- If multiple exists,take the latest record *-
  SELECT  pernr begda endda
    FROM pa0105
    INTO TABLE t_pa0105
    WHERE subty = c_0001
    AND   usrid = p_uname.

  IF sy-subrc <> 0.
    MESSAGE 'Can not determine Employee No. for current user' TYPE 'E'."sy-uname.
  ENDIF.

*- to find out latest record *-
  SORT t_pa0105 BY begda DESCENDING.
  READ TABLE t_pa0105 INDEX 1.
  IF sy-subrc = 0.
    p_pernr = t_pa0105-pernr.
  ENDIF.

ENDFORM.                    " SUB_GET_EMPLOYEE

*&---------------------------------------------------------------------*
*&      Form  SUB_DISPLAY_ZACPPEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_display_zacppem .
  DATA : l_function TYPE zzfunction,
           l_ret_code TYPE sy-subrc,
           l_program  TYPE sy-cprog.
*
*
*  IF s_arbpl[] IS NOT INITIAL.
*    LOOP AT s_arbpl.
*      CLEAR t_selparams.
*      MOVE-CORRESPONDING s_arbpl TO t_selparams.
*      t_selparams-selname = 'S_ARBPL'.
*      t_selparams-kind    = 'S'.
*      APPEND t_selparams.
*    ENDLOOP.
*  ENDIF.
*
*  IF s_date[] IS NOT INITIAL.
*    LOOP AT s_date.
*      CLEAR t_selparams.
*      MOVE-CORRESPONDING s_date TO t_selparams.
*      t_selparams-selname = 'S_DATE'.
*      t_selparams-kind    = 'S'.
*      APPEND t_selparams.
*    ENDLOOP.
*  ENDIF.
*- checks whether user is authorised to use called transaction *-
  PERFORM sub_check_tcode_authority USING    c_tcode_zacppem
                                    CHANGING l_ret_code.
  IF l_ret_code <> c_authority_ok.
    MESSAGE s000 WITH text-003. " You are not authorized to use this function
    RETURN.
  ENDIF.

*  PERFORM sub_populate_seltab USING 'ARBPL' 'STRNO'
*                                     space  'QMART'
*                                     space.

*--  Populate parameter Created by
*  CLEAR t_selparams.
*  t_selparams-kind    = 'P'.
*  t_selparams-selname = 'M_PARNR'.
*  t_selparams-sign    = 'I'.
*  t_selparams-option  = 'EQ'.
*  t_selparams-low     = sy-uname.
*  APPEND t_selparams.

*  IF x_defaults-zzunit1 IS NOT INITIAL .
*
**--  Populate parameter PLANNING PLANT
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'IWERK'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'P103'.
*    APPEND t_selparams.
*  ENDIF.
*
*  IF x_defaults-zzunit2 IS NOT INITIAL.
*    CLEAR t_selparams.
*    t_selparams-kind    = 'S'.
*    t_selparams-selname = 'IWERK'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*    t_selparams-low     = 'P103'.
*    APPEND t_selparams.
*  ENDIF.

***populate date
  IF s_date[] IS NOT INITIAL.
    LOOP AT s_date.
      CLEAR t_selparams.
      MOVE-CORRESPONDING s_date TO t_selparams.
      t_selparams-selname = 'S_ERDAT'.
      t_selparams-kind    = 'S'.
      APPEND t_selparams.
    ENDLOOP.

  ENDIF.

  IF s_strno[] IS NOT INITIAL.
    LOOP AT s_strno.
      CLEAR t_selparams.
      MOVE-CORRESPONDING s_strno TO t_selparams.
      t_selparams-selname = 'S_STRNO'.
      t_selparams-kind    = 'S'.
      APPEND t_selparams.
    ENDLOOP.
  ENDIF.

*  **new code added jsharma
  DATA x_batch_opt TYPE ctu_params.

  REFRESH t_bdcdata.

  PERFORM sub_bdc_dynpro USING 'SAPLIQS0' '0100'.
  PERFORM sub_bdc_field  USING 'BDC_OKCODE' '/00'.
  PERFORM sub_bdc_field  USING 'RIWO00-QMART' 'M4'.


  x_batch_opt-updmode = c_sync.
  x_batch_opt-dismode = c_mode_error.
  x_batch_opt-nobinpt = 'X'.

  CALL TRANSACTION c_tcode_iw21
       USING t_bdcdata
       OPTIONS FROM x_batch_opt.



  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code
                               function = l_function.

*  CHECK sy-subrc = 0.

  PERFORM sub_get_prog_name USING    c_tcode_zacppem
                            CHANGING l_program.

  PERFORM sub_submit_report TABLES t_selparams
                            USING  l_program
                                   t_config-repvar
                                   t_config-autoexec.



ENDFORM." SUB_DISPLAY_ZACPPEM
