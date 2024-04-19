"Name: \PR:SAPLCOMD\FO:KONNR_CHK_PM\SE:BEGIN\EI
ENHANCEMENT 0 ZLCOMDF30_KONNR_CHK_PM.
*
DATA: lt_ekko TYPE TABLE OF ekko,
        ls_ekko LIKE LINE OF lt_ekko.

  "Make sure Vendor has changed
  "Make sure material number is supplied
  IF resbd-lifnr IS NOT INITIAL AND resbd-lifnr NE *resbd-lifnr AND resbd-matnr IS NOT INITIAL.
    CLEAR: resbd-konnr, resbd-ktpnr.
    "Get all PO's for this vendor
    "Contracts in number range 81*
    "Validity date within requirement date from the Work Order
    SELECT ebeln bstyp kdatb kdate lifnr
      FROM ekko
      into CORRESPONDING FIELDS OF TABLE lt_ekko
      WHERE ebeln >= '8100000000'
        AND ebeln <  '8200000000'
        AND bstyp =  'K'
        AND kdatb <= resbd-bdter
        AND kdate >= resbd-bdter
        AND lifnr =  resbd-lifnr
    .

    "Loop through all posible PO's
    LOOP AT lt_ekko INTO ls_ekko.
      "Check if the material is used on one of them
      SELECT SINGLE ebeln ebelp
        INTO (resbd-konnr, resbd-ktpnr)
        FROM ekpo
        WHERE ebeln = ls_ekko-ebeln
          AND loekz =  ''
          AND matnr =  resbd-matnr
      .
      "Once one is found it is set as the agreement
      "Exit the loop
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDENHANCEMENT.
