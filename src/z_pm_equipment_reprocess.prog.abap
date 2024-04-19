*&---------------------------------------------------------------------*
*& Report  Z_PM_EQUIPMENT_TEST
*&
*&---------------------------------------------------------------------*
*&Custom Program created to test the Equipment Interface
*& Data is picked from Custom table ZPMT_EQUIP_LOG (BODS data)
*Created by : Jsharma
*date       : 04.12.2014
*&---------------------------------------------------------------------*

REPORT  z_pm_equipment_reprocess.

TABLES: zpmt_equip_log.

DATA : ta_equip  TYPE STANDARD TABLE OF zpms_equip,
       wa_equip  TYPE zpms_equip,
       ta_return TYPE STANDARD TABLE OF zpms_equip_ret.
FIELD-SYMBOLS: <fs_return> TYPE zpms_equip_ret.

SELECT-OPTIONS : s_equnr FOR zpmt_equip_log-equnr,
                 s_stat  FOR zpmt_equip_log-proc_status.

START-OF-SELECTION.

  SELECT * FROM zpmt_equip_log
    INTO CORRESPONDING FIELDS OF TABLE ta_equip
    WHERE equnr       IN s_equnr AND
          proc_status IN s_stat.
  IF sy-subrc NE 0.
    MESSAGE 'No records were selected for processing'(002) TYPE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  CALL FUNCTION 'ZPM_CREATE_EQUIP'
    EXPORTING
      imp_equip      = ta_equip
    TABLES
      tbl_return_tab = ta_return.

  MESSAGE 'Check Background jobs for execution status'(001) TYPE 'I'.
