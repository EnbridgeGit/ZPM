*&---------------------------------------------------------------------*
*&  Include           ZCPENGI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE  ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'MAIN'.
      LEAVE TO TRANSACTION 'SESSION_MANAGER'.

    WHEN 'USER'.
      CALL TRANSACTION 'ZCPUSER'.

    WHEN 'MCP'.
      CALL TRANSACTION 'ZCPMAIN'.

    WHEN 'F11'.
*      CALL TRANSACTION 'IL01'.
      PERFORM sub_call_transc.

    WHEN 'F2'.
*      CALL TRANSACTION 'IL03'.
      PERFORM sub_call_transc.

    WHEN 'F3'.
*      CALL TRANSACTION 'IL02'.
      PERFORM sub_call_transc.

    WHEN 'F4'.
*      CALL TRANSACTION 'IH11'.
      PERFORM sub_call_transc.

**Equipment master
    WHEN 'F5'.
*      CALL TRANSACTION 'IE01'.
      PERFORM sub_call_transc.

    WHEN 'F6'.
*      CALL TRANSACTION 'IE03'.
      PERFORM sub_call_transc.

    WHEN 'F7'.
*      CALL TRANSACTION 'IE02'.
      PERFORM sub_call_transc.

    WHEN 'F8'.
*      CALL TRANSACTION 'IH08'.
      PERFORM sub_call_transc.

*Part search
    WHEN 'F9'.
*      CALL TRANSACTION 'MM03'.
      PERFORM sub_call_transc.

    WHEN 'F13'." FL bom
      PERFORM sub_fl_bom. "Ib11/IB12/IB13

    WHEN 'F14'." Material Boms
*      PERFORM sub_mat_bom.
      PERFORM sub_fl_bom.

    WHEN 'F15'." Material Avaiablity
      PERFORM sub_mat_avai.

    WHEN 'F16'." Equipment
*      PERFORM sub_equip_bom.
      PERFORM sub_fl_bom.

*status
    WHEN 'F21'.
      PERFORM sub_run_report. "IW28

**trends reports
    WHEN 'F40'. "part
      PERFORM sub_run_trends USING 'F40'. "IW65
    WHEN 'F42'. " damage
      PERFORM sub_run_trends USING 'F42'. "IW68
    WHEN 'F43'." cause
      PERFORM sub_run_trends USING 'F43'.
    WHEN 'F44'."activity
      PERFORM sub_run_trends USING 'F44'.

* Report List
*   *New notification last 3 days 45
    WHEN 'F45'.
      PERFORM sub_new_notification. "IW28
    WHEN 'F46'." cause
      PERFORM sub_order_cnf. "IW38
    WHEN 'F47'." cause
      PERFORM sub_maint_bklg. "IW38


*Maintainence Plan
    WHEN 'F24'.
*      CALL TRANSACTION 'IP03'.
      PERFORM sub_call_transc.

    WHEN 'F02'.
*      CALL TRANSACTION 'IP16'.
      PERFORM sub_call_transc.

**print
    WHEN 'F25'.
*      PERFORM sub_print_not.
*        CALL TRANSACTION 'IW22'.
      PERFORM sub_call_transc.

    WHEN 'F26'.
      PERFORM sub_print_not_batch. "IW28

    WHEN 'F27'.
*      PERFORM sub_print_order.
*        CALL TRANSACTION 'IW3D'.
      PERFORM sub_call_transc.

    WHEN 'F28'.
      PERFORM sub_print_not_batch.
*      PERFORM sub_print_ord_batch.

* measuring pont
    WHEN 'F10'.
      PERFORM sub_call_transc.
*      CALL TRANSACTION 'IK01'.

    WHEN 'F29'.
      PERFORM sub_call_transc.
*      CALL TRANSACTION 'IK03'.

    WHEN 'F30'.
      PERFORM sub_call_transc.
*      CALL TRANSACTION 'IK02'.

    WHEN 'F31'.
      PERFORM sub_call_transc.
*      CALL TRANSACTION 'IK08'.


*meas Document

    WHEN 'F32'.
      PERFORM sub_call_transc.
*      CALL TRANSACTION 'IK11'.

    WHEN 'F33'.
      PERFORM sub_call_transc.
*      CALL TRANSACTION 'IK13'.

    WHEN 'F34'.
      PERFORM sub_call_transc.
*      CALL TRANSACTION 'IK12'.

    WHEN 'F35'.
      PERFORM sub_call_transc.
*      CALL TRANSACTION 'IK17'."list edit

    WHEN 'F03'.
      PERFORM sub_call_transc.
*      CALL TRANSACTION 'IK21'."collective entry FLOc

    WHEN 'F04'.
      PERFORM sub_call_transc.
*      CALL TRANSACTION 'IK22'."collective entry Equipment


*Meas Entry
    WHEN 'F36'.
      PERFORM sub_call_transc.
*      CALL TRANSACTION 'IK31'.

    WHEN 'F37'.
      PERFORM sub_call_transc.
*      CALL TRANSACTION 'IK33'.

    WHEN 'F38'.
      PERFORM sub_call_transc.
*      CALL TRANSACTION 'IK32'.

    WHEN 'F39'.
      PERFORM sub_call_transc.
*      CALL TRANSACTION 'IK34'.

*Identify
    WHEN 'F53'.
      PERFORM sub_handle_report_problem.
    WHEN 'F54'.
      PERFORM sub_handle_master_data.

*     *restraint
    WHEN 'F01'. "Operations
      PERFORM sub_operations.
    WHEN 'F22'. " ALL
      PERFORM sub_rest_all.
    WHEN 'F23'. "Engineering
      PERFORM sub_eng.

*  Links
    WHEN 'F49'.
      ok_code = 'F49'.
      READ TABLE t_config WITH KEY fcode    = ok_code
      function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.

    WHEN 'F50'.
      ok_code = 'F50'.
      READ TABLE t_config WITH KEY fcode    = ok_code
      function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.

    WHEN 'F51'.
      ok_code = 'F51'.
      READ TABLE t_config WITH KEY fcode    = ok_code
      function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.

  ENDCASE.

  CLEAR ok_code.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_9000 INPUT.

  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " EXIT_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&      Form  SUB_CHECK_TCODE_AUTHORITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_CONFIG_TCODE  text
*      <--P_L_RET_CODE  text
*----------------------------------------------------------------------*
FORM sub_check_tcode_authority USING    p_tcode  TYPE sy-tcode
                               CHANGING p_return TYPE sy-subrc.

  CLEAR p_return.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = p_tcode
    EXCEPTIONS
      ok     = 1
      not_ok = 2
      OTHERS = 3.

  p_return = sy-subrc.

ENDFORM. "_TCODE_AUTHORITY                    " SUB_CHECK_TCODE_AUTHORITY
