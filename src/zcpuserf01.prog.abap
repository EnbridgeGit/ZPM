*&---------------------------------------------------------------------*
*&  Include           ZCPUSERF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table .

  FIELD-SYMBOLS: <fs_button> TYPE zcpbuttons.

  CHECK gv_modified IS NOT INITIAL.

  LOOP AT gt_user_button ASSIGNING <fs_button>.
    <fs_button>-zuser = sy-uname.
    <fs_button>-mandt = sy-mandt.
    IF gv_check IS NOT INITIAL.
      <fs_button>-zcopy = 'X'.
    ENDIF.
  ENDLOOP.

  DELETE FROM zcpbuttons WHERE zuser = sy-uname.
  MODIFY zcpbuttons FROM TABLE gt_user_button.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.                    " UPDATE_TABLE
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_BUTTONS_ZTCODE  text
*      -->P_GT_BUTTONS_ZVARIANT  text
*----------------------------------------------------------------------*
FORM submit_report  USING    p_ztcode
                             p_zvariant.

  DATA: lw_tstc TYPE tstc.

  DATA: lv_cprog TYPE program_id,
        lv_variant TYPE syslset.

  SELECT SINGLE * INTO lw_tstc FROM tstc WHERE tcode = p_ztcode.
  CHECK sy-subrc = 0.

  lv_cprog   = lw_tstc-pgmna.
  lv_variant = p_zvariant.

  IF p_zvariant IS NOT INITIAL.
    SUBMIT (lv_cprog) USING SELECTION-SET lv_variant
            AND RETURN.
  ELSE.
    CALL TRANSACTION p_ztcode.
  ENDIF.


ENDFORM.                    " SUBMIT_REPORT

*&---------------------------------------------------------------------*
*&      Form  copy_buttons
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM copy_buttons.
  DATA: lt_buttons TYPE STANDARD TABLE OF zcpbuttons,
        lv_index(2) type n.

  FIELD-SYMBOLS: <fs_button> TYPE zcpbuttons.

  SELECT MAX( zbutton_index ) into lv_index FROM zcpbuttons
    WHERE zuser = sy-uname.
  IF sy-subrc = 0.

    LOOP AT gt_buttons ASSIGNING <fs_button>.
      <fs_button>-zuser = sy-uname.
      <fs_button>-mandt = sy-mandt.
      lv_index = lv_index + 1.
      <fs_button>-zbutton_index = lv_index.
    ENDLOOP.
    MODIFY zcpbuttons FROM TABLE gt_buttons.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.                    "copy_buttons
