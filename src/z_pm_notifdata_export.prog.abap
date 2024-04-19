*&---------------------------------------------------------------------*
*&  Include           Z_PM_NOTIFDATA_EXPORT
*&---------------------------------------------------------------------*
*
** Triger Notification data export when custom shop paper for
* for work order header is selected

TYPES: BEGIN OF lty_work_paper,
          workpaper TYPE t390-workpaper,
       END OF   lty_work_paper.

DATA: lit_work_paper TYPE STANDARD TABLE OF lty_work_paper,
      lwa_work_paper TYPE lty_work_paper.

IF NOT caufvd-qmnum IS INITIAL.
  SELECT workpaper
         FROM t390
         INTO TABLE lit_work_paper
         WHERE pdf_form = 'ZPM_WO_HEADER'.

  IF sy-subrc EQ 0.
    LOOP AT lit_work_paper INTO lwa_work_paper.
      READ TABLE lv_iworkpaper WITH KEY workpaper = lwa_work_paper-workpaper
                                        selected  = abap_true
                                        TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        PERFORM export_notif_data_from_order USING caufvd-qmnum.
        EXIT. "exit loop.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDIF.
