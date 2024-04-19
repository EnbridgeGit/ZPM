*&---------------------------------------------------------------------*
*& Report  Z_PM_EQUIPMENT_UPDATE_BAN_WO
*&
*&---------------------------------------------------------------------*
*& This program updates Banner work order number in AUFK table.
*& This program is expected to be used only to correct a specific scenario
*&  and is not intended to be used in production
*&---------------------------------------------------------------------*

REPORT  z_pm_equipment_update_ban_wo.

DATA: lt_aufk TYPE STANDARD TABLE OF aufk,
      lv_lines TYPE i.

FIELD-SYMBOLS: <fs_aufk> TYPE aufk.

IF sy-uname EQ 'NREDDY' OR
   sy-uname EQ 'EGARRISO' OR
   sy-uname EQ 'RIJAZ' .
*-- Allow the above users to continue.
ELSE.
*-- Do not allow these users to continue.
  MESSAGE 'Please do not use this program. This is written to handle specific issue' TYPE 'E'.
ENDIF.

START-OF-SELECTION.
  SELECT * FROM aufk INTO TABLE lt_aufk.
  IF sy-subrc EQ 0.
    DELETE lt_aufk WHERE zzpmbworkord IS INITIAL OR
                         zzpmbworkord = '9999999999999999'.
    LOOP AT lt_aufk ASSIGNING <fs_aufk>.
      <fs_aufk>-zzpmbworkord = '9999999999999999'.
    ENDLOOP.
    IF lt_aufk IS INITIAL.
      WRITE: / 'No records are available to be updated.'.
      EXIT.
    ENDIF.
  ELSE.
    WRITE: / 'No records are available to be updated.'.
    EXIT.
  ENDIF.

END-OF-SELECTION.
  WRITE: / 'Starting to update Equipment with Banner Work order number - 9999999999999999'.

  UPDATE aufk FROM TABLE lt_aufk.
  WRITE: / sy-dbcnt, ' record/s have been updated successfully'.
