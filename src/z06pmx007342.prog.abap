*&---------------------------------------------------------------------*
*&  Include           Z06PMX007342
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CLEAR gv_save_code.
  gv_save_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE gv_save_code.
    WHEN 'F1'.
*      LEAVE TO TRANSACTION 'ZCPMAIN'.
      PERFORM call_tcode_varaint.
    WHEN 'F2' OR 'BACK'.
      LEAVE TO TRANSACTION 'ZCPMAIN'.

    WHEN 'F3'.
      CALL TRANSACTION 'ZCPUSER'.

    WHEN 'F4'.
      CALL FUNCTION 'Z_06PM_DEFAULTS_DIALOG'.

      "  Processes and Departments

    WHEN 'F11'.   " Operations
      LEAVE TO TRANSACTION 'ZCPOPS'.

    WHEN 'F18'.   "Engineering
      LEAVE TO TRANSACTION 'ZCPENGR'.

    WHEN 'F19'.   "Maintenance Coordination
      LEAVE TO TRANSACTION 'ZCMCOR'.
    WHEN 'F21'.   " Operations
      PERFORM call_tcode_varaint.
    WHEN 'F22'.   "Engineering
      PERFORM call_tcode_varaint.
    WHEN 'F20'.   "Maintenance Coordination
      PERFORM call_tcode_varaint.

* LIst edit
    WHEN 'F41'.
      PERFORM sub_handle_corr. " corrective maintenance backlog

    WHEN 'F42'.
      PERFORM sub_handle_pmdue. "PM's due in current month

    WHEN 'F43'.
      PERFORM sub_handle_mtbf.  "MTBF = brkdwn analys.

    WHEN 'F45'.
      PERFORM sub_handle_eqn_pmis. "MTBR/MTRR = rel by eqipment

    WHEN 'F44'.
      PERFORM sub_handle_floc_pmis. "MTBR

    WHEN 'F46'.   " SAP Website   http://www.sap.com/usa/index.epx
      PERFORM call_tcode_varaint.
    WHEN 'F47'.   " SAP Website   http://www.sap.com/usa/index.epx
      PERFORM call_tcode_varaint.
    WHEN 'F48'.   " SAP Website   http://www.sap.com/usa/index.epx
      PERFORM call_tcode_varaint.

      "  Metrics
*    WHEN 'F61'.
*      REFRESH gt_notif.
*      SELECT qmart qmnum INTO TABLE gt_notif
*             FROM viqmel WHERE ( qmart = 'NC' OR qmart = 'NG' )
*                         AND   qmdab NE ''
*                         AND   qmzab NE ''.
*      IF go_chart_container IS NOT INITIAL.
*        CLEAR: go_inst, go_manager, go_chart_inst.
*        CALL METHOD go_chart_container->free.
*      ENDIF.
*    WHEN 'F64'. " 62
*      REFRESH: gt_auart, gt_order.
*      SELECT auart INTO TABLE gt_auart FROM t003o
*             WHERE auart = 'PM20'. "LIKE 'N%'
**             AND   autyp = '30'.
*
*      CHECK gt_auart[] IS NOT INITIAL.
*      SELECT auart aufnr INTO TABLE gt_order
*             FROM aufk FOR ALL ENTRIES IN gt_auart
*                         WHERE auart = gt_auart-auart.
**                         AND   phas0 = 'X'.
*
*      IF go_chart_container IS NOT INITIAL.
*        CLEAR: go_inst, go_manager, go_chart_inst.
*        CALL METHOD go_chart_container->free.
*      ENDIF.

    WHEN 'F64'.
      PERFORM sub_handle_ord_auart.
*
    WHEN 'F65'.
      PERFORM sub_handle_ord_floc.

    WHEN 'F67'.
      PERFORM sub_handle_ord_fail."by type

    WHEN 'F68'.
      PERFORM sub_handle_ord_fail_loc."by loc
    WHEN 'F69'.   " SAP Website   http://www.sap.com/usa/index.epx
      PERFORM call_tcode_varaint.
    WHEN 'F70'.   " SAP Website   http://www.sap.com/usa/index.epx
      PERFORM call_tcode_varaint.

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

    WHEN 'F32'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F32'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.

    WHEN 'F56'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F56'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.

    WHEN 'F57'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F57'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.

    WHEN 'F58'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F58'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'LINK'.
      PERFORM sub_start_url USING t_config-linkpath.
*{   INSERT         D30K924358                                        1


*}   INSERT


*****BW Reports
    WHEN 'F01'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F01'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'BURL'.
      DATA :p_link TYPE string.
      CLEAR p_link.
      p_link = t_config-linkpath.
      PERFORM sub_start_url_sso USING p_link.

    WHEN 'F02'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F02'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'BURL'.
      CLEAR p_link.
      p_link = t_config-linkpath.
      PERFORM sub_start_url_sso USING p_link.

    WHEN 'F03'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F03'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'BURL'.
      CLEAR p_link.
      p_link = t_config-linkpath.
      PERFORM sub_start_url_sso USING p_link.

    WHEN 'F04'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F04'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'BURL'.
      CLEAR p_link.
      p_link = t_config-linkpath.
      PERFORM sub_start_url_sso USING p_link.

    WHEN 'F05'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F05'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'BURL'.
      CLEAR p_link.
      p_link = t_config-linkpath.
      PERFORM sub_start_url_sso USING p_link..

    WHEN 'F06'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F06'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'BURL'.
      CLEAR p_link.
      p_link = t_config-linkpath.
      PERFORM sub_start_url_sso USING p_link.

    WHEN 'F07'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F07'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'BURL'.
      CLEAR p_link.
      p_link = t_config-linkpath.
      PERFORM sub_start_url_sso USING p_link.

    WHEN 'F08'.   " SAP Website   http://www.sap.com/usa/index.epx
      ok_code = 'F08'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'BURL'.
      CLEAR p_link.
      p_link = t_config-linkpath.
      PERFORM sub_start_url_sso USING p_link.
*{   INSERT         D30K924358                                        2
 WHEN 'F15'.
      ok_code = 'F15'.
      READ TABLE t_config WITH KEY fcode    = ok_code  function = 'BURL'.
      CLEAR p_link.
      p_link = t_config-linkpath.
      PERFORM sub_start_url_sso USING p_link.
*
*}   INSERT


    WHEN OTHERS.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  SAVE_DEFAULT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE save_default INPUT.

  PERFORM save_default.

ENDMODULE.                " SAVE_DEFAULT  INPUT
