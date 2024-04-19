**&---------------------------------------------------------------------*
**&  Include           ZVALIDATE_PSORT
*
*This routine is used to validate the PSORT field , check with the Ztable
*Object : EAM-PM-E-076-N_Load_Inspection_Points
*Author : Jyoti Sharma
*Date   : 17-06-2014
**&---------------------------------------------------------------------*
DATA : lv_code TYPE imrc_psort.

IF sy-tcode = 'IK02' OR sy-tcode = 'IK01'.

  IF impt-psort IS NOT INITIAL.
    SELECT SINGLE code INTO lv_code FROM zpmt_meas_pos
          WHERE code = impt-psort.
    IF lv_code IS INITIAL.
      MESSAGE E002(ZPM) WITH impt-psort.
    ENDIF.
  ENDIF.
ENDIF.
