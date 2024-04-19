"Name: \FU:CO_MK_CHECK_SOURCE_OF_SUPPLY\SE:END\EI
ENHANCEMENT 0 ZINFNRCLEAR.
*

  DATA: lv_loekz LIKE eina-loekz.

  "If input was initial
  IF resbd_imp-infnr IS INITIAL.
    SELECT SINGLE loekz
      FROM eina
      INTO lv_loekz
      WHERE infnr = resbd_exp-infnr
    .

    "If loekz is populated, clear the inforecord.
    IF lv_loekz IS NOT INITIAL.
      CLEAR resbd_exp-infnr.
    ENDIF.
  ENDIF.

ENDENHANCEMENT.
