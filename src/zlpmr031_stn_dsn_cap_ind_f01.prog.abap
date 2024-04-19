*&---------------------------------------------------------------------*
*&  Include           ZLPMR031_STN_DSN_CAP_IND_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZLPMR031_STN_DSN_CAP_IND_F01                   *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 17-Aug-2017                                    *
*& Object ID          : ACR-4673                                       *
*& Application Area   : PM                                             *
*& Description        : Station design capacity indicator.             *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_EQUI_CHANGE_DATA
*&---------------------------------------------------------------------*
FORM get_equi_change_data .
  TYPES: BEGIN OF lty_tplnr,
           tplnr TYPE cuobn,
         END OF lty_tplnr,
         BEGIN OF lty_cuobj,
           cuobj TYPE objnum,
         END OF lty_cuobj.
  DATA: ltp_field         TYPE atnam,
        lta_tplnr         TYPE STANDARD TABLE OF lty_tplnr,
        lwa_tplnr         TYPE lty_tplnr,
        lta_cuobj         TYPE STANDARD TABLE OF lty_cuobj,
        lwa_cuobj         TYPE lty_cuobj.
  CONSTANTS: lco_cap_enh(23) TYPE c VALUE 'ZPM_DSGN_CAP_ENH_ACTIVE'.

* Check if design capacity enhancement is active
  SELECT SINGLE field
         FROM zpmt_stncapind
         INTO ltp_field
         WHERE object = lco_cap_enh.
  IF sy-subrc = 0 AND ltp_field = 'Y'.
  ELSE.
    RETURN.
  ENDIF.

* Get FNAME and Object type from custom table ZPMT_STNCAPIND
  SELECT object
         field
         FROM zpmt_stncapind
         INTO TABLE ta_zpmt_stncapind.
  IF sy-subrc = 0.
    SORT ta_zpmt_stncapind BY object field.
  ENDIF.

* Get Equipment change data
  PERFORM get_equi_data.

* Get Classification change data
  PERFORM get_classify_data.

  SORT ta_objectid BY objectclas objectid.
  DELETE ADJACENT DUPLICATES FROM ta_objectid COMPARING objectclas objectid.
* Get equipment details
  SELECT equnr
         eqart
         tplnr
         FROM v_equi
         INTO TABLE ta_v_equi
         FOR ALL ENTRIES IN ta_objectid
         WHERE equnr = ta_objectid-objectid
         AND   datbi GE sy-datum.
  IF sy-subrc = 0.
    SORT ta_v_equi BY tplnr.
    DELETE ADJACENT DUPLICATES FROM ta_v_equi COMPARING tplnr.
    DELETE ta_v_equi WHERE tplnr = space.
  ENDIF.
  IF ta_v_equi IS NOT INITIAL.
*   Get the structure indicator for the functional location
    SELECT tplnr
           tplkz
           FROM iflot
           INTO TABLE ta_iflot
           FOR ALL ENTRIES IN ta_v_equi
           WHERE tplnr = ta_v_equi-tplnr.
    IF sy-subrc = 0.
      SORT ta_iflot BY tplnr tplkz.
    ENDIF.
*   Validate that the equipment is engineering relevant
    LOOP AT ta_v_equi INTO wa_v_equi.
      lwa_tplnr-tplnr = wa_v_equi-tplnr.
      APPEND lwa_tplnr TO lta_tplnr.
      CLEAR: lwa_tplnr,
             wa_v_equi.
    ENDLOOP.
    SELECT cuobj
           objek
           FROM inob
           INTO TABLE ta_inob_floc
           FOR ALL ENTRIES IN lta_tplnr
           WHERE klart = co_klart_003
           AND   objek = lta_tplnr-tplnr.
    IF sy-subrc EQ 0.
      SORT ta_inob_floc BY objek.
    ENDIF.
    IF ta_inob_floc IS NOT INITIAL.
      LOOP AT ta_inob_floc INTO wa_inob_floc.
        lwa_cuobj-cuobj = wa_inob_floc-cuobj.
        APPEND lwa_cuobj TO lta_cuobj.
        CLEAR: lwa_cuobj,
               wa_inob_floc.
      ENDLOOP.
      SELECT objek
             FROM kssk
             INTO TABLE ta_kssk
             FOR ALL ENTRIES IN lta_cuobj
             WHERE objek = lta_cuobj-cuobj
             AND   klart = co_klart_003
             AND   clint IN (co_2590, co_2591).
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_EQUI_CHANGE_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_INDICATOR
*&---------------------------------------------------------------------*
FORM set_indicator .
  DATA: ltp_set_ind     TYPE c,
        ltp_tabix       TYPE sy-tabix,
        ltp_tabix_char  TYPE sy-tabix.

  LOOP AT ta_v_equi INTO wa_v_equi.
    CLEAR ltp_set_ind.
*   Check if the equipment is engineering relevant
    CLEAR wa_inob_floc.
    READ TABLE ta_inob_floc INTO wa_inob_floc WITH KEY objek = wa_v_equi-tplnr.
    IF sy-subrc = 0.
*     Check if the functional location has engineering class assigned
      CLEAR wa_kssk.
      READ TABLE ta_kssk INTO wa_kssk WITH KEY objek = wa_inob_floc-cuobj.
      IF sy-subrc = 0.
*       Get the structure indicator for the functional location
        CLEAR wa_iflot.
        READ TABLE ta_iflot INTO wa_iflot WITH KEY tplnr = wa_v_equi-tplnr.
        IF sy-subrc = 0 AND wa_iflot-tplkz IS NOT INITIAL.
*         Equipment change
          CLEAR wa_objectid.
          READ TABLE ta_objectid INTO wa_objectid WITH KEY objectclas = co_equi
                                                           objectid = wa_v_equi-equnr.
          IF sy-subrc = 0.
            ltp_set_ind = 'X'.
          ELSE.
*           Classification change
            CLEAR wa_objectid.
            READ TABLE ta_objectid INTO wa_objectid WITH KEY objectclas = co_classify
                                                             objectid = wa_v_equi-equnr.
            IF sy-subrc = 0.
              CLEAR wa_inob.
              READ TABLE ta_inob INTO wa_inob WITH KEY objek = wa_v_equi-equnr.
              IF sy-subrc = 0.
                CLEAR wa_char.
                READ TABLE ta_char INTO wa_char WITH KEY cuobj = wa_inob-cuobj.
                IF sy-subrc = 0.
                  ltp_tabix_char = sy-tabix.
                  LOOP AT ta_char INTO wa_char FROM ltp_tabix_char.
                    IF wa_char-cuobj NE wa_inob-cuobj.
                      EXIT.
                    ENDIF.
                    CLEAR wa_cabn.
                    READ TABLE ta_cabn INTO wa_cabn WITH KEY atinn = wa_char-atinn.
                    IF sy-subrc = 0.
                      CLEAR wa_zpmt_stncapind.
                      READ TABLE ta_zpmt_stncapind INTO wa_zpmt_stncapind WITH KEY object = wa_v_equi-eqart
                                                                                   field = wa_cabn-atnam.
                      IF sy-subrc = 0.
                        ltp_set_ind = 'X'.
                        EXIT.
                      ENDIF.
                    ENDIF.
                  ENDLOOP.
                  CLEAR: ltp_tabix_char.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
          IF ltp_set_ind = 'X'.
*           Set the indicator
            zpm_cl_funcloc_util=>handle_equi_loc_change(
                   iv_equnr = wa_v_equi-equnr
                   iv_tplnr = wa_v_equi-tplnr
                   iv_tplkz = wa_iflot-tplkz ).

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: wa_v_equi,
           ltp_set_ind.
  ENDLOOP.

ENDFORM.                    " SET_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  GET_EQUI_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_equi_data .
  DATA: lwa_cond(72)      TYPE c,
        lta_cond          LIKE TABLE OF lwa_cond.
  CONSTANTS: lco_fname(5)    TYPE c VALUE 'FNAME'.

* Build dynamic WHERE clause
  CONCATENATE 'OBJECTCLAS' 'EQ' 'CO_EQUI' INTO lwa_cond SEPARATED BY space.
  APPEND lwa_cond TO lta_cond.
  CLEAR lwa_cond.
  IF pa_fdate IS NOT INITIAL.
    CONCATENATE 'AND' 'UDATE' 'GE' 'PA_FDATE' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  IF pa_tdate IS NOT INITIAL.
    CONCATENATE 'AND' 'UDATE' 'LE' 'PA_TDATE' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  IF pa_ftime IS NOT INITIAL.
    CONCATENATE 'AND' 'UTIME' 'GE' 'PA_FTIME' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  IF pa_ttime IS NOT INITIAL.
    CONCATENATE 'AND' 'UTIME' 'LE' 'PA_TTIME' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.

* Get the change history header details for equipment
  SELECT objectclas
         objectid
         changenr
         udate
         utime
         FROM cdhdr
         INTO TABLE ta_cdhdr
         WHERE (lta_cond).

  IF sy-subrc = 0.
    SORT ta_cdhdr BY objectclas objectid changenr.
  ENDIF.
  IF ta_cdhdr IS NOT INITIAL.
*   Get the change history item details for equipment
    SELECT objectclas
           objectid
           changenr
           fname
           FROM cdpos
           INTO TABLE ta_cdpos
           FOR ALL ENTRIES IN ta_cdhdr
           WHERE objectclas EQ ta_cdhdr-objectclas
           AND   objectid EQ ta_cdhdr-objectid
           AND   changenr EQ ta_cdhdr-changenr
           AND   chngind EQ 'U'.
    IF sy-subrc = 0.
      SORT ta_cdpos BY objectclas objectid changenr.
    ENDIF.

    LOOP AT ta_cdpos INTO wa_cdpos.
      READ TABLE ta_zpmt_stncapind INTO wa_zpmt_stncapind WITH KEY object = co_fname
                                                                   field = wa_cdpos-fname.
      IF sy-subrc = 0.
        wa_objectid-objectclas = wa_cdpos-objectclas.
        wa_objectid-objectid = wa_cdpos-objectid.
        APPEND wa_objectid TO ta_objectid.
        CLEAR: wa_objectid,
               wa_zpmt_stncapind.
      ENDIF.
      CLEAR wa_cdpos.
    ENDLOOP.

    SORT ta_objectid BY objectclas objectid.
    DELETE ADJACENT DUPLICATES FROM ta_objectid COMPARING objectclas objectid.

  ENDIF.

ENDFORM.                    " GET_EQUI_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_CLASSIFY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_classify_data .
  TYPES: BEGIN OF lty_cuobj,
           cuobj TYPE cuobj,
         END OF lty_cuobj,
         BEGIN OF lty_atinn,
           atinn TYPE atinn,
         END OF lty_atinn.

  DATA: lwa_cond(72)     TYPE c,
        lwa_cond_tmp(72) TYPE c,
        lta_cond         LIKE TABLE OF lwa_cond,
        lta_cuobj        TYPE STANDARD TABLE OF lty_cuobj,
        lwa_cuobj        TYPE lty_cuobj,
        lta_atinn        TYPE STANDARD TABLE OF lty_atinn,
        lwa_atinn        TYPE lty_atinn.

* Build dynamic WHERE clause
  CONCATENATE 'OBJECTCLAS' 'EQ' 'CO_CLASSIFY' INTO lwa_cond SEPARATED BY space.
  APPEND lwa_cond TO lta_cond.
  CLEAR lwa_cond.
  IF pa_fdate IS NOT INITIAL.
    CONCATENATE 'AND' 'UDATE' 'GE' 'PA_FDATE' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  IF pa_tdate IS NOT INITIAL.
    CONCATENATE 'AND' 'UDATE' 'LE' 'PA_TDATE' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  IF pa_ftime IS NOT INITIAL.
    CONCATENATE 'AND' 'UTIME' 'GE' 'PA_FTIME' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  IF pa_ttime IS NOT INITIAL.
    CONCATENATE 'AND' 'UTIME' 'LE' 'PA_TTIME' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  CONCATENATE 'AND' 'TCODE' 'IN' INTO lwa_cond_tmp SEPARATED BY space.
  CONCATENATE '(' 'CO_IE02' ',' INTO lwa_cond.
  CONCATENATE lwa_cond_tmp lwa_cond INTO lwa_cond SEPARATED BY space.
  CLEAR lwa_cond_tmp.
  CONCATENATE 'SPACE' ')' INTO lwa_cond_tmp.
  CONCATENATE lwa_cond lwa_cond_tmp INTO lwa_cond SEPARATED BY space.

  APPEND lwa_cond TO lta_cond.
  CLEAR  lwa_cond.

* Get the change history header details for characteristics
  SELECT objectclas
         objectid
         changenr
         udate
         utime
         FROM cdhdr
         INTO TABLE ta_cdhdr_cl
         WHERE (lta_cond).

  IF sy-subrc = 0.
    SORT ta_cdhdr_cl BY objectclas objectid changenr.
  ENDIF.
  IF ta_cdhdr_cl IS NOT INITIAL.
*   Get the change history item details for characteristics
    SELECT objectclas
           objectid
           changenr
           tabkey
           FROM cdpos
           INTO TABLE ta_cdpos_cl
           FOR ALL ENTRIES IN ta_cdhdr_cl
           WHERE objectclas EQ ta_cdhdr_cl-objectclas
           AND   objectid EQ ta_cdhdr_cl-objectid
           AND   changenr EQ ta_cdhdr_cl-changenr
           AND   fname IN (co_atwrt, co_atflv)
           AND   chngind EQ 'U'.
    IF sy-subrc = 0.
      SORT ta_cdpos_cl BY objectclas objectid changenr.
    ENDIF.
  ENDIF.
  IF ta_cdpos_cl IS NOT INITIAL.
    LOOP AT ta_cdpos_cl INTO wa_cdpos_cl.
      lwa_cuobj-cuobj = wa_cdpos_cl-tabkey+0(18).
      lwa_atinn-atinn = wa_cdpos_cl-tabkey+18(10).
      wa_char-cuobj = wa_cdpos_cl-tabkey+0(18).
      wa_char-atinn = wa_cdpos_cl-tabkey+18(10).
      APPEND lwa_cuobj TO lta_cuobj.
      APPEND lwa_atinn TO lta_atinn.
      APPEND wa_char TO ta_char.
      CLEAR: lwa_cuobj,
             lwa_atinn,
             wa_char,
             wa_cdpos_cl.
    ENDLOOP.
    SORT ta_char BY cuobj atinn.
    DELETE ADJACENT DUPLICATES FROM ta_char COMPARING cuobj atinn.

    SORT lta_cuobj BY cuobj.
    DELETE ADJACENT DUPLICATES FROM lta_cuobj COMPARING cuobj.
*   Get the equipment number
    IF lta_cuobj IS NOT INITIAL.
      SELECT cuobj
             objek
             FROM inob
             INTO TABLE ta_inob
             FOR ALL ENTRIES IN lta_cuobj
             WHERE cuobj = lta_cuobj-cuobj
             AND   klart = co_klart_002. "'002'.
      IF sy-subrc = 0.
        SORT ta_inob BY objek.
      ENDIF.
    ENDIF.
    SORT lta_atinn BY atinn.
    DELETE ADJACENT DUPLICATES FROM lta_atinn COMPARING atinn.
*   Get the characteristic name
    IF lta_atinn IS NOT INITIAL.
      SELECT atinn
             atnam
             FROM cabn
             INTO TABLE ta_cabn
             FOR ALL ENTRIES IN lta_atinn
             WHERE atinn = lta_atinn-atinn.
      IF sy-subrc = 0.
        SORT ta_cabn BY atinn.
      ENDIF.
    ENDIF.
  ENDIF.
  IF ta_inob IS NOT INITIAL.
    LOOP AT ta_inob INTO wa_inob.
      wa_objectid-objectclas = co_classify.
      wa_objectid-objectid = wa_inob-objek.
      APPEND wa_objectid TO ta_objectid.
      CLEAR: wa_objectid,
             wa_inob.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GET_CLASSIFY_DATA
