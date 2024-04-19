*----------------------------------------------------------------------*
***INCLUDE ZCPMAIN_MANAGE_WORK .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SUB_PRINT_ORDER
*&---------------------------------------------------------------------*
*       Handle action of push button Print an Order
*----------------------------------------------------------------------*
FORM sub_print_order .

  DATA : l_function TYPE zzfunction,
         l_program  TYPE sy-cprog,
         l_ret_code TYPE sy-subrc.
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.
  IF t_config-tcode IS NOT INITIAL.

    PERFORM sub_check_tcode_authority USING  t_config-tcode  "c_tcode_iw3d
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003.
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
ENDFORM.                    " SUB_PRINT_ORDER
*&---------------------------------------------------------------------*
*&      Form  SUB_CONFIRM_OPERATION
*&---------------------------------------------------------------------*
*       Handle action of push button Confirm an Operation
*----------------------------------------------------------------------*
FORM sub_confirm_operation .


  DATA : l_function TYPE zzfunction,
         l_program  TYPE sy-cprog,
         l_ret_code TYPE sy-subrc.
  REFRESH : t_selparams.
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.
  IF t_config-tcode IS NOT INITIAL.
    PERFORM sub_check_tcode_authority USING   t_config-tcode
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003.
      RETURN.
    ENDIF.
*- call tcode IW42
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

ENDFORM.                    " SUB_CONFIRM_OPERATION
*&---------------------------------------------------------------------*
*&      Form  SUB_NEW_NOTIFICATIONS
*&---------------------------------------------------------------------*
*       Handle action of Push Button New Notifs - Last 3 days
*----------------------------------------------------------------------*
FORM sub_new_notifications .

  DATA : l_ret_code  TYPE sy-subrc,
         l_prog_name TYPE sy-cprog,
         lv_date TYPE sy-datum,
         l_space     TYPE char1.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode"c_tcode_iw28   " IW28
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

*clear lv_date.
*CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL_SG'
*  EXPORTING
*    date            = sy-datum
*    days            = '0003' " 3days
*    months          = '0000'
*   SIGNUM          = '-'
*    years           = '0000'
* IMPORTING
*   CALC_DATE       = lv_date
    .
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
    PERFORM sub_get_prog_name USING    t_config-tcode"c_tcode_iw28
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
ENDFORM.                    " SUB_NEW_NOTIFICATIONS
*&---------------------------------------------------------------------*
*&      Form  SUB_NOTIFICATIONS
*&---------------------------------------------------------------------*
*       Handle action of push button Notifications
*----------------------------------------------------------------------*
FORM sub_notifications .

  DATA : l_function  TYPE zzfunction,
         l_ret_code  TYPE sy-subrc,
         l_prog_name TYPE sy-cprog.


*- check which radio button is selected *-
*- call transaction accordingly *-
  CASE 'X'.
    WHEN w_x_mgwr_open.      "  Open
      l_function = c_func_open.

    WHEN w_x_mgwr_complete.  "  Complete
      l_function = c_func_comp.

    WHEN w_x_mgwr_all.  "  all
      l_function = c_func_all.

  ENDCASE.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code
                               function = l_function.
  IF t_config-tcode IS NOT INITIAL.

*- checks whether user is authorised to use called transaction *-
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

ENDFORM.                    " SUB_NOTIFICATIONS
*&---------------------------------------------------------------------*
*&      Form  SUB_ORDERS
*&---------------------------------------------------------------------*
*       Handle action of push button Orders
*----------------------------------------------------------------------*
FORM sub_orders.

  DATA : l_function  TYPE zzfunction,
         l_ret_code  TYPE sy-subrc,
         l_prog_name TYPE sy-cprog.


*- check which radio button is selected *-
*- call transaction accordingly *-
  CASE 'X'.
    WHEN w_x_mgwr_open.      "  Open
      l_function = c_func_open.

    WHEN w_x_mgwr_complete.  "  Complete
      l_function = c_func_comp.
    WHEN w_x_mgwr_all.  "  all
      l_function = c_func_all.
  ENDCASE.

  REFRESH :t_selparams[].
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

*{   REPLACE        D30K925363                                        1
*\    PERFORM sub_populate_seltab USING 'GEWRK' 'STRNO'
*\                                      'AUART'  space
*\                                       space.
    PERFORM sub_populate_seltab USING 'GEWRK' 'STRNO'
                                      'AUART'  space
                                       space.
*}   REPLACE

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
ENDFORM.                    " SUB_ORDERS
*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_NOTIFICATION_TASKS
*&---------------------------------------------------------------------*
*       Handle action of push button Notification Tasks
*----------------------------------------------------------------------*
FORM sub_handle_notification_tasks.

  DATA : l_function TYPE zzfunction,
         l_program  TYPE sy-cprog,
         l_ret_code TYPE sy-subrc.


*- check which radio button is selected *-
*- call transaction accordingly *-
  CASE 'X'.
    WHEN w_x_mgwr_open.      "  Open
      l_function = c_func_open.

    WHEN w_x_mgwr_complete.  "  Complete
      l_function = c_func_comp.
    WHEN w_x_mgwr_all.  "  all
      l_function = c_func_all.
  ENDCASE.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code
                               function = l_function.
  IF t_config-tcode IS NOT INITIAL.

*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode  "c_tcode_iw66
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003. " You are not authorized to use this function
      RETURN.
    ENDIF.

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

    PERFORM sub_get_prog_name USING    t_config-tcode  "c_tcode_iw66
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
ENDFORM.                    " SUB_HANDLE_NOTIFICATION_TASKS

*&---------------------------------------------------------------------*
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
ENDFORM.                    "Call_Tcode_Varaint
