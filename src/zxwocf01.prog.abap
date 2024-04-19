*----------------------------------------------------------------------*
***INCLUDE ZXWOCF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DISPLAY
*&---------------------------------------------------------------------*
*       This sub routine will be used to change the editable attribute of custom fields.
*----------------------------------------------------------------------*
FORM change_display .

  LOOP AT SCREEN.
    IF screen-group1 = 'BAN'. "Banner additional fields
      IF gv_display = abap_true.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CHANGE_DISPLAY
