*----------------------------------------------------------------------*
***INCLUDE ZCPOE_USER_COMMAND_9000 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
*- store user command in a local variable *-

  ok_code = sy-ucomm.
  IF ok_code = 'RAD1' AND w_tr NE 'X'.
    CLEAR w_rev_code.
  ENDIF.

*- check user command and navigate accordingly *-
  CASE ok_code.
    WHEN 'BACK'.
*- leave to previous screen *-
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
*- leave program *-
      LEAVE PROGRAM.
    WHEN 'SMM'.
*- go to SAP Main Menu *-
      LEAVE TO TRANSACTION 'SESSION_MANAGER'.
    WHEN 'MCP'.
*- go to main control panel *-
      CALL TRANSACTION 'ZCPMAIN'.
    WHEN 'DEF'.
*- defaults popup *-
      CALL FUNCTION 'Z_NT_GET_DEFAULTS_DIALOG'.
    WHEN 'USER'.
      CALL TRANSACTION 'ZCPUSER'.

*- Restraints *-
    WHEN 'F24'.
      PERFORM sub_res_ops. "OPERATIONS"
    WHEN 'F23'.
      PERFORM sub_res_engr. "Engineering
    WHEN 'F22'.
      PERFORM sub_res_all.  "All
    WHEN 'F26'.
      PERFORM sub_fl_bom. ".....


*- Notifications *-
    WHEN 'F11'.
      PERFORM sub_notif_7days. "new in 7 days
    WHEN 'F12'.
      PERFORM sub_notif_24hrs. "new in last 24 hours

*- Print shop *-
    WHEN 'F42'.
      PERFORM sub_notif_prtshp. "Notifications in Print shop
    WHEN 'F40'.
      PERFORM sub_notif_batch. "Notifications in Batch
    WHEN 'F51'.
      PERFORM sub_orders.      "Orders
    WHEN 'F41'.
      PERFORM sub_orders_batch. "Orders in Batch


*-  Work Assigned to Ops *-
    WHEN 'F20'.
      PERFORM sub_tasks.     "Tasks
    WHEN 'F21'.
      PERFORM sub_orders_ops. " Orders
    WHEN 'F19'.
      PERFORM sub_ops.        "Operations
    WHEN 'F25'.
      PERFORM sub_meas_dsply. "Measurement document Display

*-  Functional Location *-
    WHEN 'F44'.
      PERFORM sub_floc_dsply. "functional location single display
    WHEN 'F45'.
      PERFORM sub_floc_rept. "list edit report
    WHEN 'F46'.
      PERFORM sub_eqn_dsply. "Equipment master single display
    WHEN 'F47'.
      PERFORM sub_eqn_rept. "Equipment Master List Edit
*- Links *-
    WHEN 'F81'.
      CLEAR t_config.
      ok_code = 'F81'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.
    WHEN 'F52'.
      CLEAR t_config.
      ok_code = 'F52'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.
    WHEN 'F53'.
      CLEAR t_config.
      ok_code = 'F53'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.
    WHEN 'F65'.
      CLEAR t_config.
      ok_code = 'F65'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.
**- Control Room Log Future *-
    WHEN 'F31'.
      CLEAR t_config.
      ok_code = 'F31'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.
    WHEN 'F57'.
      CLEAR t_config.
      ok_code = 'F57'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.
    WHEN 'F58'.
      CLEAR t_config.
      ok_code = 'F58'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.
    WHEN 'F59'.
      CLEAR t_config.
      ok_code = 'F59'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.
**- Shift Report Future *-
    WHEN 'F34'.
      CLEAR t_config.
      ok_code = 'F34'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.
    WHEN 'F35'.
      CLEAR t_config.
      ok_code = 'F35'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.
    WHEN 'F37'.
      CLEAR t_config.
      ok_code = 'F37'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.
    WHEN 'F39'.
      CLEAR t_config.
      ok_code = 'F39'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.
**- Reports *-
    WHEN 'F3'.
      PERFORM sub_maint_bklg.
    WHEN 'F4'.
      PERFORM sub_order_cnf.
    WHEN 'F5'.
      PERFORM call_tcode_varaint.
    WHEN 'F6'.
      PERFORM call_tcode_varaint.

*- Parts Search
    WHEN 'F71'.
      PERFORM sub_parts.
*      CALL TRANSACTION 'MM03'..
    WHEN 'F48'. "fl Bom
      PERFORM sub_fl_bom.
*      CALL TRANSACTION 'IB13'.
    WHEN 'F54'. "Eqpmnt Bom
      PERFORM sub_eqp_bom.
*      CALL TRANSACTION 'IB03'.
    WHEN 'F49'. "Mat Bom
      PERFORM sub_mat_bom.
*      CALL TRANSACTION 'CS03'.

    WHEN 'F50'. "Mat avl
      PERFORM sub_mat_avai.

*-status *-
    WHEN 'F32'."unit report
      PERFORM sub_unit_report.
    WHEN 'F33'." status report
      PERFORM sub_status_report.

*-scheduling *-
    WHEN 'F1'.
      PERFORM sub_sch_ord. " Order list
    WHEN 'F2'.
      PERFORM sub_sch_ops. " Operations List
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " EXIT  INPUT
