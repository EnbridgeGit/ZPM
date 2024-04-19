*&---------------------------------------------------------------------*
*&  Include           ZXCOZU02
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(CAUFVD_IMP) LIKE  CAUFVD STRUCTURE  CAUFVD
*"             VALUE(RESBD_IMP) LIKE  RESBD STRUCTURE  RESBD
*"             VALUE(EBAN_IMP) LIKE  EBAN STRUCTURE  EBAN
*"             VALUE(EBKN_IMP) LIKE  EBKN STRUCTURE  EBKN
*"       CHANGING
*"             VALUE(EBAN_ORD_CUST_CHG) LIKE  EBAN_ORD_CUST
*"                             STRUCTURE  EBAN_ORD_CUST
*"----------------------------------------------------------------------
TYPES:
  BEGIN OF ts_ekpo,
            ebeln TYPE ebeln,   "Agrement No.
            ebelp TYPE ebelp,   "Item No.
            txz01 TYPE txz01,   "Short Text
            matnr TYPE matnr,   "Material
            matkl TYPE matkl,   "Material Group
            netpr TYPE bprei,   "Price
            werks TYPE ewerk,   "Plant
            plifz TYPE eplif,   "Planned Delivery Time in Days 'Lead Time'
            lifnr TYPE elifn,   "Vendor No.
            ekorg TYPE ekorg,   "Purchasing Organization
  END OF ts_ekpo,
  tt_ekpo TYPE TABLE OF ts_ekpo,

  BEGIN OF ts_eina,
          matnr TYPE matnr,   "Material Number
          matkl TYPE matkl,   "Material Group
          lifnr TYPE elifn,   "Vendor Account No.
          infnr TYPE infnr,   "No. of Purchasing Info records
          ekorg TYPE ekorg,   "Purchasing Org
          ekgrp TYPE ekgrp,   "Purchasing Group
          esokz TYPE esokz,   "Purchasing info record category
          werks TYPE ewerk,   "Plant
  END OF ts_eina,
  tt_eina TYPE TABLE OF ts_eina .



* Define Structures
DATA: ls_plantspc   TYPE ts_ekpo,
      ls_orgspc     TYPE ts_ekpo,
      ls_inforec    TYPE ts_eina,
      ls_ven_agre   TYPE ts_ekpo.

* Define Internal Tables
DATA: lt_plantspc TYPE tt_ekpo, "Plant specific
      lt_orgspc   TYPE tt_ekpo, "Organization specific
      lt_inforec  TYPE tt_eina,
      lt_ven_agre TYPE tt_ekpo.


* Vendor Determination
DATA: lv_minprice   TYPE bprei,   "Min Price
      lv_minledtime TYPE eplif,   "Min Lead Time
      lv_line       TYPE i.

DATA: lv_mtart      TYPE mara-mtart.

* Define Constants
CONSTANTS : co_1       TYPE char1  VALUE '1',
            co_9       TYPE char1  VALUE '9',
            co_k       TYPE bstyp  VALUE 'K',
            co_x       TYPE char1  VALUE 'X',
            co_del     TYPE char1  VALUE ''.

*BOI by PANUSURI Ticket 47426
DATA: ls_afvgd TYPE afvgd.

*Read operation data
IF resbd_imp-aplzl IS NOT INITIAL AND resbd_imp-aufpl IS NOT INITIAL.

  CALL FUNCTION 'CO_BT_AFVG_READ_WITH_KEY'
    EXPORTING
      aplzl     = resbd_imp-aplzl
      aufpl     = resbd_imp-aufpl
    IMPORTING
      afvgd_exp = ls_afvgd
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  IF ls_afvgd-slwid = 'P2P_PM'.
    MOVE ls_afvgd-usr00 TO eban_ord_cust_chg-konnr.
    MOVE ls_afvgd-usr02 TO eban_ord_cust_chg-ktpnr.
    TRANSLATE eban_ord_cust_chg-zzariba_approver TO UPPER CASE.
    MOVE ls_afvgd-usr03 TO eban_ord_cust_chg-zzorigreq.
    MOVE ls_afvgd-usr06 TO eban_ord_cust_chg-preis.
  ENDIF.

* Begin of code removal as part of defect 51
*  "Move the operation dates.
*  "START
*  IF ls_afvgd-ntanf IS NOT INITIAL.
*    eban_ord_cust_chg-frgdt = ls_afvgd-ntanf.
*  ELSE.
*    "Put system date
*    eban_ord_cust_chg-frgdt = sy-datum.
*  ENDIF.
*  "END
*
*  IF ls_afvgd-ntend IS NOT INITIAL.
*    eban_ord_cust_chg-lfdat = ls_afvgd-ntend.
*  ELSE.
*    "Make end date equal to start date
*    eban_ord_cust_chg-lfdat = eban_ord_cust_chg-frgdt.
*  ENDIF.
* End of code removal as part of defect 51

ENDIF.
*EOI by PANUSURI Ticket 47426

IF eban_imp-flief IS NOT INITIAL.
  "Already has a vendor assigned.
  EXIT.
ENDIF.

IF eban_imp-konnr IS NOT INITIAL AND eban_imp-ktpnr IS NOT INITIAL.
  "Assign lifnr from Agreement.
  SELECT SINGLE ekko~lifnr
    INTO eban_ord_cust_chg-flief
    FROM ekpo
    INNER JOIN ekko
      ON ekpo~ebeln = ekko~ebeln
    WHERE ekpo~ebeln = eban_imp-konnr
      AND ekpo~ebelp = eban_imp-ktpnr
  .

  "Get PGROUP FROM Info Rec
  CLEAR lt_inforec.
  SELECT a~matnr
         a~matkl
         a~lifnr
         b~infnr
         b~ekorg
         b~ekgrp
         b~esokz
         b~werks
    INTO TABLE lt_inforec
    FROM eina AS a
      INNER JOIN eine AS b
        ON b~infnr = a~infnr
    WHERE a~matnr = eban_imp-matnr
      AND b~werks = eban_imp-werks
      AND a~loekz = ''
  .

  IF sy-subrc = 0. "InfoRec Found
    SORT lt_inforec BY infnr DESCENDING.  "Sort by Info record to get the last used

    "Get the most recent info record associated with the vendor
    CLEAR ls_inforec.
    READ TABLE lt_inforec INDEX 1 INTO ls_inforec.

    IF sy-subrc = 0. "Recent Info record  found
      eban_ord_cust_chg-ekgrp = ls_inforec-ekgrp.
    ENDIF.

  ELSE. "No Plant Specific InfoRec
    CLEAR lt_inforec.
    SELECT a~matnr
       a~matkl
       a~lifnr
       b~infnr
       b~ekorg
       b~ekgrp
       b~esokz
       b~werks
      INTO TABLE lt_inforec
      FROM eina AS a
        INNER JOIN eine AS b
          ON b~infnr = a~infnr
      WHERE a~matnr = eban_imp-matnr
        AND a~loekz = ''
    .

    IF sy-subrc = 0.
      SORT lt_inforec BY infnr DESCENDING.  "Sort by Info record to get the last used

      "Get the most recent info record associated with the vendor
      CLEAR ls_inforec.
      READ TABLE lt_inforec INDEX 1 INTO ls_inforec.

      IF sy-subrc = 0. "Recent Info record  found
        eban_ord_cust_chg-ekgrp = ls_inforec-ekgrp.
      ENDIF.
    ENDIF.
  ENDIF.
  "Exit
  EXIT.
ENDIF.

IF eban_imp-matnr IS INITIAL.
  "No material, can't do auto sourcing.
  EXIT.
ENDIF.

SELECT SINGLE mtart
  INTO lv_mtart
  FROM mara
  WHERE matnr = eban_imp-matnr.
.

IF lv_mtart = 'DIEN'.
  "Do not source DIEN
  EXIT.
ENDIF.
****************************************
**** Source of supply determination ****
****************************************


"Fetch Agreement(EBELN) Vendor(LIFNR) using Material No.(MATNR)
SELECT a~ebeln
     a~ebelp
     a~txz01
     a~matnr
     a~matkl
     a~netpr
     a~werks
     a~plifz
     b~lifnr
     b~ekorg
  INTO TABLE lt_ven_agre
  FROM ekpo AS a
  INNER JOIN ekko AS b
    ON a~ebeln = b~ebeln
  INNER JOIN lfa1 AS c
    ON b~lifnr = c~lifnr
  WHERE a~matnr = eban_imp-matnr
    AND   a~ebeln >= '8100000000'
    AND   a~ebeln <  '8200000000'
    AND   a~pstyp = eban_imp-pstyp
    AND   b~bstyp = co_k
    AND   a~loekz = ''
    AND   b~kdatb <= eban_imp-lfdat
    AND   b~kdate >= eban_imp-lfdat
    AND   c~sperm <> 'X'
    AND ( a~werks = eban_imp-werks
        OR a~werks = space )
.

IF sy-subrc = 0.  " Agreement found
  CLEAR : ls_ven_agre, lt_plantspc, lt_orgspc.

  LOOP AT lt_ven_agre INTO ls_ven_agre.
    IF ls_ven_agre-werks IS NOT INITIAL.
      IF ls_ven_agre-werks EQ eban_imp-werks.
        APPEND ls_ven_agre TO lt_plantspc.
      ENDIF.
    ELSE.
      APPEND ls_ven_agre TO lt_orgspc.
    ENDIF.
  ENDLOOP.

  CLEAR : lt_ven_agre, ls_ven_agre.
*     If multiple OLA then get the agreement with Lowest price
*     If same price then get the shortest lead time
*     if lowest price and lead time same then exit, source of supply will not be assigned.

  IF NOT lt_plantspc IS INITIAL. "Plant specific
    lt_ven_agre[] = lt_plantspc[].
  ELSEIF NOT lt_orgspc IS INITIAL. "Organization specific
    lt_ven_agre[] = lt_orgspc[].
  ENDIF.


  SORT lt_ven_agre BY netpr ASCENDING.   "Sort by Price

**Read 1st records for getting the lowest price
  CLEAR ls_ven_agre.
  READ TABLE lt_ven_agre INDEX  1
                             INTO ls_ven_agre.

  lv_minprice = ls_ven_agre-netpr.

** Delete all other records not having lowest price
  DELETE lt_ven_agre WHERE netpr NE lv_minprice.

  DESCRIBE TABLE  lt_ven_agre LINES lv_line. "Count the number of records

**If more then one record found with same price then filter based on shortest lead time
  IF lv_line > 1.

    SORT lt_ven_agre BY plifz ASCENDING.   "Sort by Leadtime

**Read 1st records for getting the lowest price
    CLEAR ls_ven_agre.
    READ TABLE lt_ven_agre INDEX  1
                               INTO ls_ven_agre.

    lv_minledtime = ls_ven_agre-plifz.

** Delete all other records not having lowest price
    DELETE lt_ven_agre WHERE plifz NE lv_minledtime.
    CLEAR lv_line.
    DESCRIBE TABLE  lt_ven_agre LINES lv_line. "Count the number of records

**If more then one record found with same price and same shortest lead time
    IF lv_line > 1.
      CLEAR ls_ven_agre.
    ENDIF.
  ENDIF.
ENDIF.

IF ls_ven_agre IS NOT INITIAL. "Agreement found

  "Set the new values
  eban_ord_cust_chg-konnr = ls_ven_agre-ebeln.
  eban_ord_cust_chg-ktpnr = ls_ven_agre-ebelp.
  eban_ord_cust_chg-flief = ls_ven_agre-lifnr.

  "Get PGROUP FROM Info Rec
  CLEAR lt_inforec.
  SELECT a~matnr
         a~matkl
         a~lifnr
         b~infnr
         b~ekorg
         b~ekgrp
         b~esokz
         b~werks
    INTO TABLE lt_inforec
    FROM eina AS a
      INNER JOIN eine AS b
        ON b~infnr = a~infnr
    WHERE a~matnr = eban_imp-matnr
      AND a~lifnr = ls_ven_agre-lifnr
      AND b~werks = eban_imp-werks
      AND a~loekz = ''
  .

  IF sy-subrc = 0. "InfoRec Found
    SORT lt_inforec BY infnr DESCENDING.  "Sort by Info record to get the last used

    "Get the most recent info record associated with the vendor
    CLEAR ls_inforec.
    READ TABLE lt_inforec INDEX 1 INTO ls_inforec.

    IF sy-subrc = 0. "Recent Info record  found
      eban_ord_cust_chg-ekgrp = ls_inforec-ekgrp.
    ENDIF.

  ELSE. "No Plant Specific InfoRec
    CLEAR lt_inforec.
    SELECT a~matnr
       a~matkl
       a~lifnr
       b~infnr
       b~ekorg
       b~ekgrp
       b~esokz
       b~werks
      INTO TABLE lt_inforec
      FROM eina AS a
        INNER JOIN eine AS b
          ON b~infnr = a~infnr
      WHERE a~matnr = eban_imp-matnr
        AND a~lifnr = ls_ven_agre-lifnr
        AND a~loekz = ''
    .

    IF sy-subrc = 0.
      SORT lt_inforec BY infnr DESCENDING.  "Sort by Info record to get the last used

      "Get the most recent info record associated with the vendor
      CLEAR ls_inforec.
      READ TABLE lt_inforec INDEX 1 INTO ls_inforec.

      IF sy-subrc = 0. "Recent Info record  found
        eban_ord_cust_chg-ekgrp = ls_inforec-ekgrp.
      ENDIF.
    ENDIF.
  ENDIF.

  EXIT.
ENDIF.

"No Agreement found look for Source List

"Get Source List
SELECT SINGLE lifnr
  INTO (eban_ord_cust_chg-flief)
  FROM eord
  WHERE matnr = eban_imp-matnr
    AND werks = eban_imp-werks
    AND vdatu <= eban_imp-lfdat
    AND bdatu >= eban_imp-lfdat
    AND notkz = ''
    AND autet <> 1
.

IF eban_ord_cust_chg-flief IS NOT INITIAL. "Agreement found
  EXIT.
ENDIF.


"If no source list, look for Info Rec
"Fetch Info Rec(INFNR) details
CLEAR lt_inforec.
SELECT a~matnr
       a~matkl
       a~lifnr
       b~infnr
       b~ekorg
       b~ekgrp
       b~esokz
       b~werks
  INTO TABLE lt_inforec
  FROM eina AS a
    INNER JOIN eine AS b
      ON b~infnr = a~infnr
  WHERE a~matnr = eban_imp-matnr
    AND b~werks = eban_imp-werks
    AND a~loekz = ''
.

IF sy-subrc = 0. "InfoRec Found
  SORT lt_inforec BY infnr DESCENDING.  "Sort by Info record to get the last used

  "Get the most recent info record associated with the vendor
  CLEAR ls_inforec.
  READ TABLE lt_inforec INDEX 1 INTO ls_inforec.

  IF sy-subrc = 0. "Recent Info record  found
    eban_ord_cust_chg-ekgrp = ls_inforec-ekgrp.
    eban_ord_cust_chg-flief = ls_inforec-lifnr.
    eban_ord_cust_chg-infnr = ls_inforec-infnr.
  ENDIF.

ELSE. "No Plant Specific InfoRec
  CLEAR lt_inforec.
  SELECT a~matnr
     a~matkl
     a~lifnr
     b~infnr
     b~ekorg
     b~ekgrp
     b~esokz
     b~werks
    INTO TABLE lt_inforec
    FROM eina AS a
      INNER JOIN eine AS b
        ON b~infnr = a~infnr
    WHERE a~matnr = eban_imp-matnr
      AND a~loekz = ''
  .
  IF sy-subrc = 0.
    SORT lt_inforec BY infnr DESCENDING.  "Sort by Info record to get the last used

    "Get the most recent info record associated with the vendor
    CLEAR ls_inforec.
    READ TABLE lt_inforec INDEX 1 INTO ls_inforec.

    IF sy-subrc = 0. "Recent Info record  found
      eban_ord_cust_chg-ekgrp = ls_inforec-ekgrp.
      eban_ord_cust_chg-flief = ls_inforec-lifnr.
      eban_ord_cust_chg-infnr = ls_inforec-infnr.
    ENDIF.
  ENDIF.
ENDIF.
