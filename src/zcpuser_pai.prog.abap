*----------------------------------------------------------------------*
***INCLUDE ZCPUSER_PAI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  save_code = ok_code.
  CLEAR ok_code.

  CASE save_code.
    WHEN 'F01'. " SAP Easy Access
      PERFORM update_table.
      LEAVE PROGRAM.
    WHEN 'F02'.
      PERFORM update_table. " Main Control Panel
      CALL TRANSACTION 'ZCPMAIN'.
    WHEN 'F03'.             " Define User's Buttons
      REFRESH gt_user_button.
      CLEAR: gt_user_button, gv_modified.
      CALL SCREEN 9001 STARTING AT 40 20.
      SET SCREEN 9000.
    WHEN 'F04'.             " Call Another's User Panel
      CALL SCREEN 9002 STARTING AT 40 5.
    WHEN 'BACK'.
      PERFORM update_table.
      LEAVE TO SCREEN 0.
    WHEN 'COPY_BUT'.
      PERFORM copy_buttons.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      IF save_code(1) = 'B'.
        CLEAR gt_buttons.
        READ TABLE gt_buttons WITH KEY zbutton_index = save_code+1(2).
        IF sy-subrc = 0.
          PERFORM submit_report USING gt_buttons-ztcode
                                      gt_buttons-zvariant.
        ENDIF.
      ENDIF.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_9001 INPUT.
  REFRESH gt_user_button.
  CLEAR: gt_user_button, gv_modified.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " EXIT_9001  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_TEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_text INPUT.

  IF gt_user_button-zbutton_text IS INITIAL.
    SET CURSOR FIELD 'GT_USER_BUTTON-ZBUTTON_TEXT' LINE sy-stepl.
    MESSAGE e000(zntemplate) WITH text-t01.
    EXIT.
  ENDIF.

ENDMODULE.                 " CHECK_TEXT  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_TCODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_tcode INPUT.

  IF gt_user_button-ztcode IS INITIAL.
    SET CURSOR FIELD 'GT_USER_BUTTON-ZTCODE' LINE sy-stepl.
    MESSAGE e000(zntemplate) WITH text-t02.
    EXIT.
  ELSE.
    SELECT SINGLE * INTO lw_tstc FROM tstc WHERE tcode = gt_user_button-ztcode.
    IF sy-subrc NE 0.
      SET CURSOR FIELD 'GT_USER_BUTTON-ZTCODE' LINE sy-stepl.
      MESSAGE e000(zntemplate) WITH text-t03.
      EXIT.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_TCODE  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_RECORD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_record INPUT.

  MODIFY gt_user_button INDEX tc_buttons-current_line.
  IF sy-subrc NE 0.
    DESCRIBE TABLE gt_user_button LINES gv_line.
    gt_user_button-zbutton_index = gv_line + 1.
*    gt_user_button-zbutton_index = sy-stepl.
    APPEND gt_user_button.
  ENDIF.

  gv_modified = 'X'.

ENDMODULE.                 " MODIFY_RECORD  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  save_code = ok_code.
  CLEAR ok_code.

  CASE save_code.
    WHEN 'F05'.
      LEAVE TO SCREEN 0.
    WHEN 'F06'.
    WHEN 'DELE'.
      CLEAR gt_user_button.
      READ TABLE gt_user_button WITH KEY zsel = 'X'.
      IF sy-subrc NE 0.
        MESSAGE s000(zntemplate) WITH text-t08.
      ELSE.
        gv_modified = 'X'.
        DELETE gt_user_button WHERE zsel = 'X'.
        IF gt_user_button[] IS NOT INITIAL.
          CLEAR: gv_line, gt_user_button.
          gv_line = 1.
          LOOP AT gt_user_button.
            gt_user_button-zbutton_index = gv_line.
            gv_line = gv_line + 1.
            MODIFY gt_user_button INDEX sy-tabix TRANSPORTING zbutton_index.
          ENDLOOP.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_VARIANT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_variant INPUT.

  DATA: lv_rc LIKE sy-subrc.

  SELECT SINGLE * INTO lw_tstc FROM tstc
         WHERE tcode = gt_user_button-ztcode.
  IF sy-subrc = 0 AND gt_user_button-zvariant IS NOT INITIAL.

    CALL FUNCTION 'RS_VARIANT_EXISTS'
      EXPORTING
        report              = lw_tstc-pgmna
        variant             = gt_user_button-zvariant
      IMPORTING
        r_c                 = lv_rc
      EXCEPTIONS
        not_authorized      = 1
        no_report           = 2
        report_not_existent = 3
        report_not_supplied = 4
        OTHERS              = 5.
    IF sy-subrc <> 0 OR lv_rc NE 0.
      MESSAGE e000(zntemplate) WITH text-t06.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_VARIANT  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_9002 INPUT.
  CLEAR gv_call.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " EXIT_9002  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002 INPUT.

  DATA: lw_usr01 TYPE usr01.

  CLEAR save_code.
  save_code = ok_code.
  CLEAR ok_code.

  IF save_code = 'F5'.

    CHECK gv_call IS NOT INITIAL.

    IF gv_call = sy-uname.
      LEAVE TO SCREEN 0.
    ELSE.

      SELECT SINGLE * INTO lw_usr01 FROM usr01
             WHERE bname = gv_call.

      IF sy-subrc NE 0.
        SET CURSOR FIELD 'GV_CALL'.
        MESSAGE s000(zntemplate) WITH text-t07.
      ELSE.
        EXPORT lv_user FROM gv_call TO MEMORY ID 'CALL_USER'.
        CALL TRANSACTION 'ZCPUSER'.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDIF.

  ENDIF.

ENDMODULE.                 " USER_COMMAND_9002  INPUT
