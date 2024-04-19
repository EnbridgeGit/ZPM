*&---------------------------------------------------------------------*
*&  Include           ZCPMAINTCOOI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  CASE  ok_code.
*- store user command in a local variable *-
      ok_code = sy-ucomm.
      IF ok_code = 'RAD1' AND w_tr NE 'X'.
*    REFRESH lt_vrm_values.
        CLEAR w_rev_code.
      ENDIF.

    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'MCP'.
      CALL TRANSACTION 'ZCPMAIN'.
    WHEN 'USER'.
      CALL TRANSACTION 'ZCPUSER'.
*Tasks list
    WHEN 'F1'.
      PERFORM sub_create_gtl. "create GTL
*      LEAVE TO TRANSACTION 'IA05'.
    WHEN 'F2'.
      PERFORM sub_create_gtl. "Update GTL
*      CALL TRANSACTION 'IA06'.
    WHEN 'F0'.
      PERFORM sub_create_gtl.  "display GTL
*      CALL TRANSACTION 'IA07'.
    WHEN 'F3'.
      PERFORM sub_create_gtl. "create ETL
*      CALL TRANSACTION 'IA01'.
    WHEN 'F4'.
      PERFORM sub_create_gtl. "update ETL
*      CALL TRANSACTION 'IA02'.
    WHEN 'F5'.
      PERFORM sub_create_gtl. "List Change
*      CALL TRANSACTION 'IA08'.
    WHEN 'F6'.
      PERFORM sub_create_gtl. "display ETL
*      CALL TRANSACTION 'IA03'.
*Reliability*
    WHEN 'F66'. "part
      PERFORM sub_run_trends USING 'F66'.
    WHEN 'F67'. "damage
      PERFORM sub_run_trends USING 'F67'.
    WHEN 'F68'. "Cause
      PERFORM sub_run_trends USING 'F68'.
    WHEN 'F69'. "Activity
      PERFORM sub_run_trends USING 'F69'.
*Order Restraints*
    WHEN 'F62'.
      PERFORM sub_res_all.  "all,IW38
    WHEN 'F63'.
      PERFORM sub_res_engr. "engineering,IW38
    WHEN 'F65'.
      PERFORM sub_res_ops.  "Operations, IW38
    WHEN 'F72'.
     Perform Call_Tcode_Varaint . "Operations, IW38
*Parts Search
    WHEN 'F7'.
      CLEAR t_config.
      ok_code = 'F7'.
      READ TABLE t_config WITH KEY fcode    = ok_code.
      IF t_config-tcode IS NOT INITIAL.
        CALL TRANSACTION t_config-tcode."'MM03'."Part number, manufacturer, Description, Material number
      ELSE.
        MESSAGE 'Tcode is not exist in the table ZNTCONFIG' TYPE 'I'.
      ENDIF.

    WHEN 'F10'.
*      PERFORM sub_equip_bom.
      PERFORM sub_fl_bom.
    WHEN 'F11'. "FL BOM
      PERFORM sub_fl_bom.
    WHEN 'F12'. "MAT BOM
*      PERFORM sub_mat_bom.
      PERFORM sub_fl_bom.
    WHEN 'F13'.
      PERFORM sub_mat_avai. "MMBE

*Permits
    WHEN 'F48'. "All , IPM3
      PERFORM sub_permit.
    WHEN 'F49'. "Hot work
      PERFORM sub_permit.
    WHEN 'F50'. "logout/tagout
      PERFORM sub_permit.
    WHEN 'F51'. "confined space
      PERFORM sub_permit.
    WHEN 'F52'. "dig
      PERFORM sub_permit.

*Maintenance Plan
    WHEN 'F54'.
      PERFORM sub_create_gtl.
*      CALL TRANSACTION 'IP03'.
    WHEN 'F55'.
      PERFORM sub_create_gtl.
*       CALL TRANSACTION 'IP16'.

*Report list
    WHEN 'F56'.
      PERFORM sub_new_notification. "new notifications in last 3 days, IW28
    WHEN 'F58'.
      PERFORM sub_order_cnf. "Order CNF not TECO, IW38
    WHEN 'F57'.
      PERFORM sub_order_cnf. "only hold, IW38
    WHEN 'F59'.
      PERFORM sub_maint_bklg. "maintenance backlog, IW38

*Print Shop
    WHEN 'F34'.
      PERFORM sub_create_gtl. "Notifications in Print shop, IW22
*      PERFORM sub_notif_prtshp.
    WHEN 'F35'.
      PERFORM sub_notif_batch. "Notifications in Batch, IW28
    WHEN 'F38'.
      PERFORM sub_create_gtl.      "Orders
*      PERFORM sub_orders.     "IW3D
    WHEN 'F39'.
      PERFORM sub_orders_batch. "Orders in Batch , IW38
    WHEN 'F37'." task list
      PERFORM sub_tasklist. "IA17
    WHEN 'F40'.
      PERFORM sub_create_gtl.
*      LEAVE TO TRANSACTION 'ME23N'.
*status
    WHEN 'F46'.
      PERFORM sub_run_report. "RUN REPORT,IW28

* opeartionS (Tied to scheduling Panel)
    WHEN 'F33'.
      PERFORM sub_op_run_report. "RUN REPORT,IW37N

*Process & Departments
    WHEN 'F14'.
      CALL TRANSACTION 'ZACPPEM'.  "Performance Metrics
    WHEN 'F15'.
      CALL TRANSACTION 'ZCPOPS'.   "Operations
    WHEN 'F16'.
      CALL TRANSACTION 'ZCPENGR'.  "Engineering panel

*-scheduling *-
    WHEN 'F70'.
      PERFORM sub_sch_ord. " Order list,IW38
    WHEN 'F71'.
      PERFORM sub_sch_ops. " Operations List,IW37N
*-Orders(Tied to Scheduling Panel)
    WHEN 'F27'.
      PERFORM sub_run_orders. " Run report Iw38
*      schedule dispatch
    WHEN 'F60'.
      PERFORM sub_dispatch. " Operations List IW37N

  ENDCASE.
  CLEAR ok_code.
ENDMODULE.                 " USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Module  HANDLE_EXIT_COMMAN_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE handle_exit_comman_9001 INPUT.

  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " HANDLE_EXIT_COMMAN_9000  INPUT
" SUB_RES_ENGR
*&---------------------------------------------------------------------*
