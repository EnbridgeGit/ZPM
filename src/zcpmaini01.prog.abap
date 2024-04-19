*&--------------------------------------------------------------------*
*&  Include           ZCPMAINI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  HANDLE_USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       Handle user interaction with screen 9000
*----------------------------------------------------------------------*
MODULE handle_user_command_9000 INPUT.

  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'MAIN'.
      LEAVE TO TRANSACTION 'SESSION_MANAGER'.

    WHEN 'USER'.

      LEAVE TO TRANSACTION 'ZCPUSER'.

*- handle user command for the block My Work *-
*- The block is designated as '1' and buttons are numbered serially *-
*- thus function code for button "Notification Tasks" is 'F11' *-
    WHEN 'F10'.   " Info Button
      PERFORM sub_display_info USING c_doc_mywork.

    WHEN 'F11'.   " Notification Tasks
      PERFORM sub_notification_tasks.

    WHEN 'F12'.   " Order Operations
      PERFORM sub_order_operations.

    WHEN 'F13'.   " Notifications I created
      PERFORM sub_notification_i_created.

    WHEN 'F14'.   " Enter Timesheet
      PERFORM sub_enter_timesheet.

    WHEN 'F15'.   " SAP Inbox
      PERFORM sub_navigate_sap_inbox.

    WHEN 'F16'.   " Expense Report *-
      PERFORM sub_expense_report.

    WHEN 'F20'.   " Info Button
      PERFORM sub_display_info USING c_doc_defaults.

*- handle user command for the block Process and Departments *-
*- The block is designated as '3' and buttons are numbered serially *-
*- thus function code for button "Corrective Action Program is" 'F31' *-
    WHEN 'F31'.   " Corrective Action Program
*      CALL TRANSACTION 'ZCPCAP'.
      LEAVE TO TRANSACTION 'ZACPPEM'.
    WHEN 'F33'.   " Operations
*      CALL TRANSACTION 'ZCPOPS'.
      LEAVE TO TRANSACTION 'ZCPOPS'.
    WHEN 'F35'.   " Maintenance
*      CALL TRANSACTION 'ZCPMTNC'.
      LEAVE TO TRANSACTION 'ZCMCOR'.
    WHEN 'F38'.   " Engineering
*      CALL TRANSACTION 'ZCPENGR'.
      LEAVE TO TRANSACTION 'ZCPENGR'.
*    **process and department
    WHEN 'F37'.   " SAP Website   http://www.sap.com/usa/index.epx
      PERFORM call_tcode_varaint.

    WHEN 'F73'.   " SAP Website   http://www.sap.com/usa/index.epx
      PERFORM call_tcode_varaint.

    WHEN 'F74'.   " SAP Website   http://www.sap.com/usa/index.epx
      PERFORM call_tcode_varaint.
**end jsharma
      .

****jsharma added
*- handle user command for the block Other Information *-
*- The block is designated as '5' and buttons are numbered serially *-
*- thus function code for button "SAP Website" is 'F51' *-
    WHEN 'F51'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F51'.
      READ TABLE t_config WITH KEY fcode    = ok_code
      function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.

    WHEN 'F52'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F52'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.

    WHEN 'F53'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F53'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.

    WHEN 'F54'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F54'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.

    WHEN 'F55'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F55'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.



*- handle user command for the block Manage Work *-
*- The block is designated as '4' and buttons are numbered serially *-
*- thus function code for button "Print an Order is" 'F41' *-
    WHEN 'F40'.   " Info Button
      PERFORM sub_display_info USING c_doc_manage_work.

    WHEN 'F41'.   " Print an Order
      PERFORM sub_print_order.

    WHEN 'F42'.   " Confirm an Operation
      PERFORM sub_confirm_operation.

    WHEN 'F43'.   " New Notifications - last 3 days
      PERFORM sub_new_notifications.

    WHEN 'F44'.   " Notifications
      PERFORM sub_notifications.

    WHEN 'F45'.   " Orders
      PERFORM sub_orders.

    WHEN 'F46'.   " Notification Tasks
      PERFORM sub_handle_notification_tasks.

**** Start of Block Base Transactions ****
*- handle user command for the block "Base Transaction" *-
*- The block is designated as '6' and buttons are numbered serially *-
*- thus function code for button "Create" is 'F61' *-

    WHEN 'F60'.
      PERFORM sub_display_info USING c_doc_base_transc.

    WHEN 'F61'.   "  Create
      PERFORM sub_handle_create_base_trans.

    WHEN 'F62'.   "  Change
      PERFORM sub_handle_change_base_trans.

    WHEN 'F63'.   "  Display
      PERFORM sub_handle_display_base_trans.

    WHEN 'F64'.   " List Edit
      PERFORM sub_handle_listedit_base_trans.

    WHEN 'F65'.   " Order Operations
      PERFORM sub_handle_order_operations.
**** End of Base Transactions Work ****


****handle user command for the Identify Tab*****************************
*- The block is designated as '7' and buttons are numbered serially *-
    WHEN 'F71'.   " Maintenance request
      PERFORM sub_handle_report_problem.
    WHEN 'F72'.   " Master data
      PERFORM sub_handle_master_data.
  ENDCASE.

  CLEAR ok_code.

ENDMODULE.                 " HANDLE_USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  HANDLE_EXIT_COMMAN_9000  INPUT
*&---------------------------------------------------------------------*
*       Handle exit commands of screen 9000
*----------------------------------------------------------------------*
MODULE handle_exit_comman_9000 INPUT.

  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " HANDLE_EXIT_COMMAN_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  SAVE_DEFAULTS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE save_defaults INPUT.
  PERFORM sub_save_defaults.
ENDMODULE.                 " SAVE_DEFAULTS  INPUT
