*----------------------------------------------------------------------*
***INCLUDE ZCPUSER_PBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  DATA: lv_user LIKE sy-uname.

  DATA: lt_event TYPE TABLE OF sy-ucomm WITH HEADER LINE.

  IMPORT lv_user TO lv_user FROM MEMORY ID 'CALL_USER'.
  IF sy-subrc NE 0.
    gv_uname = sy-uname.
  ELSE.
    gv_uname = lv_user.
    gv_copy  = 'X'.
    lt_event = 'F03'.
    APPEND lt_event.

    CLEAR lt_event.
    lt_event = 'F04'.
    APPEND lt_event.
  ENDIF.

  SELECT SINGLE *
         FROM zntlogo
         WHERE tcode = sy-tcode.

  SET PF-STATUS 'ZSTATUS_9000' EXCLUDING lt_event.
  SET TITLEBAR 'ZTITLE_9000' WITH zntlogo-title gv_uname.

  FREE MEMORY ID 'CALL_USER'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'ZSTATUS_9001'.
  SET TITLEBAR 'ZTITLE_9001' WITH gv_uname.

  tc_buttons-lines = 24.

ENDMODULE.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.

  DATA: lv_index    TYPE i,
        lv_number   TYPE i,
        lv_desc_number TYPE i,
        lv_char(2)  TYPE c,
        lv_name(20) TYPE c,
        lv_desc(30) TYPE c,
        lv_string(480) TYPE c,
        lv_desc_string(720) TYPE c.

  CLEAR: lv_index,
         lv_number,
         lv_desc_number,
         lv_string,
         lv_desc_string.

  CLEAR gs_buttons.
  IF gv_uname = sy-uname.
    SELECT * INTO TABLE gt_buttons FROM zcpbuttons
             UP TO 24 ROWS WHERE zuser = gv_uname.
  ELSE.
    SELECT * INTO TABLE gt_buttons FROM zcpbuttons
             UP TO 24 ROWS WHERE zuser = gv_uname
                           AND   zcopy = gv_copy.
  ENDIF.


  SORT gt_buttons BY zbutton_index.

  CLEAR gt_buttons.
  LOOP AT gt_buttons.
    " Button Text field
    lv_string+lv_number = gt_buttons-zbutton_text.
    lv_number = ( lv_index + 1 ) * 20.

    " Button Description field
    lv_desc_string+lv_desc_number = gt_buttons-zbutton_desc.
    lv_desc_number = ( lv_index + 1 ) * 30.

    lv_index = lv_index + 1.
    CLEAR gt_buttons.
  ENDLOOP.

  IF lv_string IS NOT INITIAL.
    gs_buttons = lv_string.
  ENDIF.

  IF lv_desc_string IS NOT INITIAL.
    gs_desc = lv_desc_string.
  ENDIF.

  DESCRIBE TABLE gt_buttons LINES lv_index.

  CHECK lv_index > 0.

  DO lv_index TIMES.

    lv_char = sy-index + 9.
    CONCATENATE 'GS_BUTTONS-B' lv_char
           INTO lv_name.
    CONDENSE lv_name.

    CONCATENATE 'GS_DESC-B' lv_char
           INTO lv_desc.

    LOOP AT SCREEN.
      IF screen-name = lv_name
      OR screen-name = lv_desc.
        screen-invisible = '0'.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = 'GV_COPY_BUT'.
        IF gv_copy = 'X'.
          screen-invisible = '0'.
        ELSE.
          screen-invisible = '1'.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ENDDO.

ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  RET_RECORDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ret_records OUTPUT.

  IF gt_user_button[] IS INITIAL.
    SELECT * INTO TABLE gt_user_button FROM zcpbuttons
             WHERE zuser = sy-uname.

    SORT gt_user_button BY zbutton_index.
  ENDIF.
ENDMODULE.                 " RET_RECORDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9002 OUTPUT.
  SET PF-STATUS 'ZSTATUS_9002'.
  SET TITLEBAR 'ZTITLE_9002'.
ENDMODULE.                 " STATUS_9002  OUTPUT
