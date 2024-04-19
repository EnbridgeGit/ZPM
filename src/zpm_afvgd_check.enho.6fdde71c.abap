"Name: \PR:SAPLCOIH\FO:CONTRACT_CHK_TK\SE:END\EI
ENHANCEMENT 0 ZPM_AFVGD_CHECK.
*
  DATA: lv_bname TYPE usr01-bname.
  IF afvgd-afnam IS NOT INITIAL.
    IF afvgd-afnam NE afvgd_sav-afnam.
      TRANSLATE afvgd-afnam TO UPPER CASE.
      SELECT SINGLE bname FROM usr01 INTO lv_bname WHERE bname = afvgd-afnam.
      IF sy-subrc NE 0.
        MESSAGE e004(zpm) WITH afvgd-afnam.
      ENDIF.
    ENDIF.
  ENDIF.

ENDENHANCEMENT.
