*&---------------------------------------------------------------------*
*&  Include           zlpmr029_equi_analysis_loc_f01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& include name       : zlpmr029_equi_analysis_loc_f01                 *
*& author             : praveena anusuri                               *
*& creation date      : 10-jul-2017                                    *
*& object id          : acr-4555                                       *
*& application area   : pm                                             *
*& description        : equipment analysis by location.                *
*&---------------------------------------------------------------------*
* Version No    : 2.0                                                  *
* Date          : 01-Oct-2018                                          *
* Modified By   : KBANERJEE                                            *
* Correction No : D30K929166                                           *
* Description   : Changes for performance improvement  and correction  *
*                 of timeout error.                                    *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_equi_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_equi_data .
  TYPES: BEGIN OF lty_objid,
           objid TYPE cr_objid,
         END OF lty_objid,
         BEGIN OF lty_eqart,
           eqart TYPE char18,
         END OF lty_eqart,
         BEGIN OF lty_equnr,
           equnr TYPE char50,
         END OF lty_equnr,
         BEGIN OF lty_clint,
           clint TYPE objnum,
         END OF lty_clint,
         BEGIN OF lty_clint_final,
           clint TYPE clint,
         END OF lty_clint_final,
**--START OF CHANGES FOR CHG0125142 BY KBANERJEE
         BEGIN OF lty_iflot,
           tplnr TYPE tplnr,
           iwerk TYPE iwerk,
         END OF lty_iflot.
**--END OF CHANGES FOR CHG0125142 BY KBANERJEE
  DATA: lta_objid       TYPE STANDARD TABLE OF lty_objid,
        lwa_objid       TYPE lty_objid,
        lta_eqart       TYPE STANDARD TABLE OF lty_eqart,
        lwa_eqart       TYPE lty_eqart,
        lta_equnr       TYPE STANDARD TABLE OF lty_equnr,
        lwa_equnr       TYPE lty_equnr,
        lta_clint       TYPE STANDARD TABLE OF lty_clint,
        lwa_clint       TYPE lty_clint,
        lta_clint_final TYPE STANDARD TABLE OF lty_clint_final,
        lwa_clint_final TYPE lty_clint_final,
        lta_equi_tmp    TYPE STANDARD TABLE OF ty_v_equi,
        ltp_equnr_cnt   TYPE i,
**--START OF CHANGES FOR CHG0125142 BY KBANERJEE
        lt_iflot        TYPE STANDARD TABLE OF lty_iflot INITIAL SIZE 0,
        lt_inob         TYPE STANDARD TABLE OF  ty_inob  INITIAL SIZE 0,
        lt_iflot_tmp    TYPE STANDARD TABLE OF lty_iflot INITIAL SIZE 0,
        lwa_iflot       TYPE lty_iflot.
**--END OF CHANGES FOR CHG0125142 BY KBANERJEE
  CONSTANTS: lco_klart  TYPE klassenart VALUE '002'.
  RANGES: lra_gewrk     FOR crhd-objid.

  IF so_gewrk IS NOT INITIAL.
    SELECT objid
           FROM crhd
           INTO TABLE lta_objid
           WHERE arbpl IN so_gewrk.
    IF sy-subrc = 0.
      SORT lta_objid BY objid.
    ENDIF.
    lra_gewrk-sign = 'I'.
    lra_gewrk-option = 'EQ'.
    LOOP AT lta_objid INTO lwa_objid.
      lra_gewrk-low = lwa_objid-objid.
      APPEND lra_gewrk.
    ENDLOOP.
  ENDIF.
**--START OF CHANGES FOR CHG0125142 BY KBANERJEE
*Get Planning plant for functional location entered
  SELECT tplnr iwerk
    FROM iflot
    INTO TABLE lt_iflot
    WHERE tplnr IN so_tplnr.
  IF sy-subrc IS INITIAL.
    SORT lt_iflot BY iwerk.
    lt_iflot_tmp = lt_iflot.
    DELETE ADJACENT DUPLICATES FROM lt_iflot_tmp COMPARING iwerk.
  ENDIF.
**--END OF CHANGES FOR CHG0125142 BY KBANERJEE
**--START OF CHANGES FOR CHG0125142 BY KBANERJEE
** Get Equipment data
*  SELECT equnr
*         eqart
*         herst
*         serge
*         typbz
*         objnr
*         mapar
*         gewrk
*         tplnr
*         FROM v_equi
*         INTO TABLE ta_v_equi
*         WHERE eqart IN so_eqart
*         AND   datbi = '99991231'
*         AND   gewrk IN lra_gewrk
*         AND   tplnr IN so_tplnr.
* Get Equipment data
  IF lt_iflot_tmp IS NOT INITIAL.
    IF lra_gewrk IS NOT INITIAL.
      SELECT equnr
             eqart
             herst
             serge
             typbz
             objnr
             mapar
             gewrk
             tplnr
             FROM v_equi
             INTO TABLE ta_v_equi
             FOR ALL ENTRIES IN lt_iflot_tmp
             WHERE eqart IN so_eqart
               AND datbi = '99991231'
               AND iwerk  = lt_iflot_tmp-iwerk
               AND gewrk IN lra_gewrk
               AND tplnr IN so_tplnr.
    ELSE.
      SELECT equnr
                eqart
                herst
                serge
                typbz
                objnr
                mapar
                gewrk
                tplnr
                FROM v_equi
                INTO TABLE ta_v_equi
                FOR ALL ENTRIES IN lt_iflot_tmp
                WHERE eqart IN so_eqart
                  AND datbi = '99991231'
                  AND iwerk  = lt_iflot_tmp-iwerk
                  AND tplnr IN so_tplnr.
    ENDIF."end of check lr_gwerk is initial
**--END OF CHANGES FOR CHG0125142 BY KBANERJEE
    IF sy-subrc = 0.
      SORT ta_v_equi BY equnr eqart.
    ENDIF.
**--START OF CHANGES FOR CHG0125142 BY KBANERJEE
  ENDIF. "end of check on initiality of lt_iflot_tmp
**--END OF CHANGES FOR CHG0125142 BY KBANERJEE
  IF ta_v_equi IS NOT INITIAL.
*   Get system status
    lta_equi_tmp[] = ta_v_equi[].
    SORT lta_equi_tmp BY equnr.
    DELETE ADJACENT DUPLICATES FROM lta_equi_tmp COMPARING equnr.
    SELECT objnr
           stat
           FROM jest
           INTO TABLE ta_jest
           FOR ALL ENTRIES IN lta_equi_tmp
           WHERE objnr = lta_equi_tmp-objnr
           AND   stat IN (co_i0076, co_i0320)
           AND   inact = space.
    IF sy-subrc = 0.
      SORT ta_jest BY objnr stat.
    ENDIF.
    REFRESH lta_equi_tmp.

*   Delete Equipments with status DLFL and INAC
    LOOP AT ta_v_equi INTO wa_v_equi.
      CLEAR wa_jest.
      READ TABLE ta_jest INTO wa_jest WITH KEY objnr = wa_v_equi-objnr.
      IF sy-subrc = 0.
      ELSE.
        lwa_equnr-equnr = wa_v_equi-equnr.
        APPEND lwa_equnr TO lta_equnr.
        lwa_eqart-eqart = wa_v_equi-eqart.
        APPEND lwa_eqart TO lta_eqart.
        wa_v_equi_final-tplnr = wa_v_equi-tplnr.
        wa_v_equi_final-gewrk = wa_v_equi-gewrk.
        wa_v_equi_final-eqart = wa_v_equi-eqart.
        wa_v_equi_final-equnr = wa_v_equi-equnr.
        wa_v_equi_final-herst = wa_v_equi-herst.
        wa_v_equi_final-typbz = wa_v_equi-typbz.
        wa_v_equi_final-mapar = wa_v_equi-mapar.
        wa_v_equi_final-serge = wa_v_equi-serge.
        APPEND wa_v_equi_final TO ta_v_equi_final.
        CLEAR: lwa_equnr,
               lwa_eqart,
               wa_v_equi_final.
      ENDIF.
      CLEAR wa_v_equi.
    ENDLOOP.
  ENDIF.
  IF ta_v_equi_final IS NOT INITIAL.
*   Get Object type descriptions
    lta_equi_tmp[] = ta_v_equi_final[].
    SORT lta_equi_tmp BY eqart.
    DELETE ADJACENT DUPLICATES FROM lta_equi_tmp COMPARING eqart.
    SELECT eqart
           eartx
           FROM t370k_t
           INTO TABLE ta_t370k_t
           FOR ALL ENTRIES IN lta_equi_tmp
           WHERE spras = 'EN'
           AND   eqart = lta_equi_tmp-eqart.
    IF sy-subrc = 0.
      SORT ta_t370k_t BY eqart eartx.
    ENDIF.
    REFRESH lta_equi_tmp.

*   Get total number of possible characteristics based on object type
    SORT lta_eqart BY eqart.
    DELETE ADJACENT DUPLICATES FROM lta_eqart COMPARING eqart.
*   Get internal class number based on object types
    SELECT clint
           class
           FROM klah
           INTO TABLE ta_klah
           FOR ALL ENTRIES IN lta_eqart
           WHERE klart = lco_klart  "'002'
           AND   class = lta_eqart-eqart.
    IF sy-subrc = 0.
      SORT ta_klah BY clint.
    ENDIF.
    REFRESH lta_eqart.

    IF ta_klah IS NOT INITIAL.
      LOOP AT ta_klah INTO wa_klah.
        lwa_clint-clint = wa_klah-clint.
        APPEND lwa_clint TO lta_clint.
        CLEAR: lwa_clint,
               wa_klah.
      ENDLOOP.
*     Get parent class assigned to the internal class
      SELECT objek
             clint
             FROM kssk
             INTO TABLE ta_kssk
             FOR ALL ENTRIES IN lta_clint
             WHERE objek = lta_clint-clint
             AND   mafid = 'K'
             AND   klart = lco_klart.
      IF sy-subrc = 0.
        SORT ta_kssk BY objek clint.
      ENDIF.

      lta_clint_final[] = lta_clint[].
      LOOP AT ta_kssk INTO wa_kssk.
        lwa_clint_final-clint = wa_kssk-clint.
        APPEND lwa_clint_final TO lta_clint_final.
        CLEAR: lwa_clint_final,
               wa_kssk.
      ENDLOOP.
      SORT lta_clint_final BY clint.
      DELETE ADJACENT DUPLICATES FROM lta_clint_final COMPARING clint.

      IF lta_clint_final IS NOT INITIAL.
*       Get number of characteristics assigned to the class
        SELECT clint
               posnr
               imerk
               FROM ksml
               INTO TABLE ta_ksml
               FOR ALL ENTRIES IN lta_clint_final
               WHERE clint = lta_clint_final-clint
               AND   klart = lco_klart. "'002'.
        IF sy-subrc = 0.
          SORT ta_ksml BY clint.
        ENDIF.

*       Remove duplicate characteristics
        LOOP AT ta_klah INTO wa_klah.
          READ TABLE ta_kssk INTO wa_kssk WITH KEY objek = wa_klah-clint.
          IF sy-subrc = 0.
            LOOP AT ta_ksml INTO wa_ksml.
              IF wa_ksml-clint = wa_kssk-clint.
                wa_ksml_final-clint = wa_kssk-objek.
                wa_ksml_final-posnr = wa_ksml-posnr.
                wa_ksml_final-imerk = wa_ksml-imerk.
                APPEND wa_ksml_final TO ta_ksml_final.
                CLEAR wa_ksml_final.
              ELSEIF wa_ksml-clint = wa_klah-clint.
                wa_ksml_final-clint = wa_ksml-clint.
                wa_ksml_final-posnr = wa_ksml-posnr.
                wa_ksml_final-imerk = wa_ksml-imerk.
                APPEND wa_ksml_final TO ta_ksml_final.
                CLEAR wa_ksml_final.
              ENDIF.
              CLEAR wa_ksml.
            ENDLOOP.
          ELSE.
            LOOP AT ta_ksml INTO wa_ksml WHERE clint = wa_klah-clint.
              wa_ksml_final-clint = wa_ksml-clint.
              wa_ksml_final-posnr = wa_ksml-posnr.
              wa_ksml_final-imerk = wa_ksml-imerk.
              APPEND wa_ksml_final TO ta_ksml_final.
              CLEAR: wa_ksml_final,
                     wa_ksml.
            ENDLOOP.
          ENDIF.
          CLEAR: wa_kssk,
                 wa_klah.
        ENDLOOP.

        SORT ta_ksml_final BY clint imerk.
        DELETE ADJACENT DUPLICATES FROM ta_ksml_final COMPARING clint imerk.
      ENDIF.
    ENDIF.

*   Get the number of characteristics that are populated
    SORT lta_equnr BY equnr.
    DELETE ADJACENT DUPLICATES FROM lta_equnr COMPARING equnr.
**--START OF CHANGES FOR CHG0125142 BY KBANERJEE
**   Get the internal classification number
*    SELECT cuobj
*           objek
*           FROM inob
*           INTO TABLE ta_inob
*           FOR ALL ENTRIES IN lta_equnr
*           WHERE klart = lco_klart "'002'
*           AND   objek = lta_equnr-equnr.
*    IF sy-subrc = 0.
*      SORT ta_inob BY cuobj.
*    ENDIF.
*    REFRESH lta_equnr.
*
**   Get the count of charecteristics populated for the list of equipment
*    IF ta_inob IS NOT INITIAL.
*      SELECT objek
*             atinn
*             FROM ausp
*             INTO TABLE ta_ausp
*             FOR ALL ENTRIES IN ta_inob
*             WHERE objek = ta_inob-cuobj
*             AND   klart = lco_klart. "'002'
*      IF sy-subrc = 0.
*        SORT ta_ausp BY objek.
*      ENDIF.
*    ENDIF.
** Get the count of charecteristics,Get the internal classification number
** by using a join on tbale INOB and AUSP
    SELECT b~objek  b~cuobj a~objek a~atinn
          INTO TABLE ta_inob
          FROM ausp AS a
          INNER JOIN inob AS b
          ON a~objek = b~cuobj
        WHERE a~klart = lco_klart.
    IF sy-subrc = 0.
*Filter characteristics data based on valid equipments
*as per functional location and planning plant
      SORT lt_inob BY objeki.
      LOOP AT lt_inob INTO wa_inob.
        READ TABLE lta_equnr INTO lwa_equnr
                            WITH KEY equnr = wa_inob-objeki
                            BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          APPEND wa_inob TO ta_inob.
        ENDIF.
        CLEAR:wa_inob,lwa_equnr,ta_inob.
      ENDLOOP.
    ENDIF.
**--ENDOF CHANGES FOR CHG0125142 BY KBANERJEE
*   Get work center descriptions
    lta_equi_tmp[] = ta_v_equi_final[].
    SORT lta_equi_tmp BY gewrk.
    DELETE ADJACENT DUPLICATES FROM lta_equi_tmp COMPARING gewrk.
    SELECT objid
           arbpl
           FROM crhd
           INTO TABLE ta_crhd
           FOR ALL ENTRIES IN lta_equi_tmp
           WHERE objty = 'A'
           AND   objid = lta_equi_tmp-gewrk.
    IF sy-subrc = 0.
      SORT ta_crhd BY objid.
    ENDIF.
    REFRESH lta_equi_tmp.
  ENDIF.

ENDFORM.                    " Get_equi_data
*&---------------------------------------------------------------------*
*&      Form  GET_OUTPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_output_data .
  TYPES: BEGIN OF lty_class_tmp,
           class       TYPE klasse_d,
           char_cnt    TYPE i,
         END OF lty_class_tmp,
         BEGIN OF lty_equnr_tmp,
           objek       TYPE cuobn,
           char_cnt    TYPE i,
         END OF lty_equnr_tmp,
         BEGIN OF lty_total_cnt,
           tplnr       TYPE string,
           eqart       TYPE eqart,
           gewrk       TYPE lgwid,
           arbpl       TYPE arbpl,
           eartx       TYPE eartx,
           equnr_cnt   TYPE i,
           herst_cnt   TYPE i,
           typbz_cnt   TYPE i,
           mapar_cnt   TYPE i,
           serge_cnt   TYPE i,
           tchar_cnt   TYPE i,
           pchar_cnt   TYPE i,
       END OF lty_total_cnt.

  DATA: lta_class_tmp      TYPE STANDARD TABLE OF lty_class_tmp,
        lwa_class_tmp      TYPE lty_class_tmp,
        lta_equnr_tmp      TYPE STANDARD TABLE OF lty_equnr_tmp,
        lwa_equnr_tmp      TYPE lty_equnr_tmp,
        lta_total_cnt      TYPE STANDARD TABLE OF lty_total_cnt,
        lwa_total_cnt      TYPE lty_total_cnt,
        lta_total_tmp      TYPE STANDARD TABLE OF lty_total_cnt,
        lwa_total_tmp      TYPE lty_total_cnt,
        lwa_tot_tmp        TYPE lty_total_cnt,
        ltp_string         TYPE string,
        ltp_equnr_cnt      TYPE i,
        ltp_herst_cnt      TYPE i,
        ltp_typbz_cnt      TYPE i,
        ltp_mapar_cnt      TYPE i,
        ltp_serge_cnt      TYPE i,
        ltp_tchar_cnt      TYPE i,
        ltp_pchar_cnt      TYPE i,
        ltp_equnr_loc_cnt  TYPE i,
        ltp_herst_loc_cnt  TYPE i,
        ltp_typbz_loc_cnt  TYPE i,
        ltp_mapar_loc_cnt  TYPE i,
        ltp_serge_loc_cnt  TYPE i,
        ltp_tchar_loc_cnt  TYPE i,
        ltp_pchar_loc_cnt  TYPE i.

* Get number of characteristics for the associated Class
  SORT ta_ksml_final BY clint.
  LOOP AT ta_ksml_final INTO wa_ksml_final.
    ltp_tchar_cnt = ltp_tchar_cnt + 1.
    AT END OF clint.
      READ TABLE ta_klah INTO wa_klah WITH KEY clint = wa_ksml_final-clint.
      IF sy-subrc = 0.
        lwa_class_tmp-class = wa_klah-class.
        lwa_class_tmp-char_cnt = ltp_tchar_cnt.
      ENDIF.
      APPEND lwa_class_tmp TO lta_class_tmp.
      CLEAR: lwa_class_tmp,
             wa_klah,
             ltp_tchar_cnt.
    ENDAT.
    CLEAR wa_ksml_final.
  ENDLOOP.

* Get the number of characteristics populated for the list of equipment
**--START OF CHANGES FOR CHG0125142 BY KBANERJEE
*  SORT ta_ausp BY objek.
*  LOOP AT ta_ausp INTO wa_ausp.
*    ltp_pchar_cnt = ltp_pchar_cnt + 1.
*    AT END OF objek.
*      READ TABLE ta_inob INTO wa_inob WITH KEY cuobj = wa_ausp-objek.
*      IF sy-subrc = 0.
*        lwa_equnr_tmp-objek = wa_inob-objek.
*        lwa_equnr_tmp-char_cnt = ltp_pchar_cnt.
*      ENDIF.
*      APPEND lwa_equnr_tmp TO lta_equnr_tmp.
*      CLEAR: lwa_equnr_tmp,
*             ltp_pchar_cnt.
*    ENDAT.
*    CLEAR wa_ausp.
*  ENDLOOP.
  SORT ta_inob BY objeki.
  LOOP AT ta_inob INTO wa_inob.
    ltp_pchar_cnt = ltp_pchar_cnt + 1.
    AT END OF objeki.
      lwa_equnr_tmp-objek    = wa_inob-objeki.
      lwa_equnr_tmp-char_cnt = ltp_pchar_cnt.
      APPEND lwa_equnr_tmp TO lta_equnr_tmp.
      CLEAR: lwa_equnr_tmp,
             ltp_pchar_cnt.
    ENDAT.
  ENDLOOP.
**--END OF CHANGES FOR CHG0125142 BY KBANERJEE
* Get the count of equipment and characteristics
  SORT ta_v_equi_final BY tplnr gewrk eqart equnr herst serge typbz mapar.
  DELETE ADJACENT DUPLICATES FROM ta_v_equi_final COMPARING tplnr gewrk eqart
                                            equnr herst serge typbz mapar.
  LOOP AT ta_v_equi_final INTO wa_v_equi_final.
    READ TABLE ta_t370k_t INTO wa_t370k_t WITH KEY eqart = wa_v_equi_final-eqart.
    IF sy-subrc = 0.
      lwa_total_cnt-eartx = wa_t370k_t-eartx.
    ENDIF.
    READ TABLE ta_crhd INTO wa_crhd WITH KEY objid = wa_v_equi_final-gewrk.
    IF sy-subrc = 0.
      lwa_total_cnt-arbpl = wa_crhd-arbpl.
    ENDIF.
    READ TABLE lta_equnr_tmp INTO lwa_equnr_tmp WITH KEY objek = wa_v_equi_final-equnr.
    IF sy-subrc = 0.
      lwa_total_cnt-pchar_cnt = lwa_equnr_tmp-char_cnt.
    ENDIF.
    SPLIT wa_v_equi_final-tplnr AT '-' INTO lwa_total_cnt-tplnr ltp_string.
    lwa_total_cnt-eqart = wa_v_equi_final-eqart.
    lwa_total_cnt-gewrk = wa_v_equi_final-gewrk.
    IF wa_v_equi_final-equnr IS NOT INITIAL.
      lwa_total_cnt-equnr_cnt =  1.
    ENDIF.
    IF wa_v_equi_final-herst IS NOT INITIAL.
      lwa_total_cnt-herst_cnt =  1.
    ENDIF.
    IF wa_v_equi_final-typbz IS NOT INITIAL.
      lwa_total_cnt-typbz_cnt =  1.
    ENDIF.
    IF wa_v_equi_final-mapar IS NOT INITIAL.
      lwa_total_cnt-mapar_cnt =  1.
    ENDIF.
    IF wa_v_equi_final-serge IS NOT INITIAL.
      lwa_total_cnt-serge_cnt =  1.
    ENDIF.

    APPEND lwa_total_cnt TO lta_total_cnt.
    CLEAR: wa_v_equi_final,
           lwa_total_cnt,
           wa_t370k_t,
           wa_crhd,
           lwa_equnr_tmp.
  ENDLOOP.

  SORT lta_total_cnt BY tplnr eqart gewrk.
  LOOP AT lta_total_cnt INTO lwa_total_cnt.
    COLLECT lwa_total_cnt INTO lta_total_tmp.
  ENDLOOP.

  CLEAR ltp_pchar_cnt.
* Populate final output table
  LOOP AT lta_total_tmp INTO lwa_total_tmp.
    lwa_tot_tmp = lwa_total_tmp.
    IF lwa_tot_tmp-equnr_cnt IS NOT INITIAL.
      ltp_equnr_cnt = lwa_tot_tmp-equnr_cnt + ltp_equnr_cnt.
    ENDIF.
    IF lwa_tot_tmp-herst_cnt IS NOT INITIAL.
      ltp_herst_cnt = lwa_tot_tmp-herst_cnt + ltp_herst_cnt.
    ENDIF.
    IF lwa_tot_tmp-typbz_cnt IS NOT INITIAL.
      ltp_typbz_cnt = lwa_tot_tmp-typbz_cnt + ltp_typbz_cnt.
    ENDIF.
    IF lwa_tot_tmp-mapar_cnt IS NOT INITIAL.
      ltp_mapar_cnt = lwa_tot_tmp-mapar_cnt + ltp_mapar_cnt.
    ENDIF.
    IF lwa_tot_tmp-serge_cnt IS NOT INITIAL.
      ltp_serge_cnt = lwa_tot_tmp-serge_cnt + ltp_serge_cnt.
    ENDIF.
    IF lwa_tot_tmp-pchar_cnt IS NOT INITIAL.
      ltp_pchar_cnt = lwa_tot_tmp-pchar_cnt + ltp_pchar_cnt.
    ENDIF.
    wa_final-tplnr = lwa_tot_tmp-tplnr.
    wa_final-gewrk = lwa_tot_tmp-gewrk.
    wa_final-arbpl = lwa_tot_tmp-arbpl.
    wa_final-eqart = lwa_tot_tmp-eqart.
    wa_final-eartx = lwa_tot_tmp-eartx.
    wa_final-equnr_cnt = lwa_tot_tmp-equnr_cnt.
    IF lwa_tot_tmp-equnr_cnt IS NOT INITIAL.
      wa_final-herst_cnt = ( lwa_tot_tmp-herst_cnt * 100 ) / lwa_tot_tmp-equnr_cnt.
      wa_final-typbz_cnt = ( lwa_tot_tmp-typbz_cnt * 100 ) / lwa_tot_tmp-equnr_cnt.
      wa_final-mapar_cnt = ( lwa_tot_tmp-mapar_cnt * 100 ) / lwa_tot_tmp-equnr_cnt.
      wa_final-serge_cnt = ( lwa_tot_tmp-serge_cnt * 100 ) / lwa_tot_tmp-equnr_cnt.
    ENDIF.
    CLEAR lwa_class_tmp.
    READ TABLE lta_class_tmp INTO lwa_class_tmp WITH KEY class = lwa_tot_tmp-eqart.
    IF sy-subrc = 0.
      wa_final-tchar_cnt = lwa_tot_tmp-equnr_cnt * lwa_class_tmp-char_cnt.
    ENDIF.
    IF wa_final-tchar_cnt IS NOT INITIAL.
      wa_final-pchar_cnt = ( lwa_tot_tmp-pchar_cnt * 100 ) / wa_final-tchar_cnt.
    ENDIF.
    APPEND wa_final TO ta_final.
    CLEAR wa_final.
    AT END OF eqart.
      wa_final-tplnr = lwa_tot_tmp-tplnr.
      wa_final-arbpl = 'ALL'.
      wa_final-eqart = lwa_tot_tmp-eqart.
      wa_final-eartx = lwa_tot_tmp-eartx.
      wa_final-equnr_cnt = ltp_equnr_cnt.
      IF ltp_equnr_cnt IS NOT INITIAL.
        wa_final-herst_cnt = ( ltp_herst_cnt * 100 ) / ltp_equnr_cnt.
        wa_final-typbz_cnt = ( ltp_typbz_cnt * 100 ) / ltp_equnr_cnt.
        wa_final-mapar_cnt = ( ltp_mapar_cnt * 100 ) / ltp_equnr_cnt.
        wa_final-serge_cnt = ( ltp_serge_cnt * 100 ) / ltp_equnr_cnt.
      ENDIF.
      IF lwa_class_tmp-char_cnt IS NOT INITIAL.
        wa_final-tchar_cnt = ltp_equnr_cnt * lwa_class_tmp-char_cnt.
        ltp_tchar_loc_cnt = wa_final-tchar_cnt + ltp_tchar_loc_cnt.
      ENDIF.
      IF wa_final-tchar_cnt IS NOT INITIAL.
        wa_final-pchar_cnt = ( ltp_pchar_cnt * 100 ) / wa_final-tchar_cnt.
      ENDIF.
      ltp_pchar_loc_cnt = ltp_pchar_cnt + ltp_pchar_loc_cnt.
      ltp_equnr_loc_cnt = ltp_equnr_cnt + ltp_equnr_loc_cnt.
      ltp_herst_loc_cnt = ltp_herst_cnt + ltp_herst_loc_cnt.
      ltp_typbz_loc_cnt = ltp_typbz_cnt + ltp_typbz_loc_cnt.
      ltp_mapar_loc_cnt = ltp_mapar_cnt + ltp_mapar_loc_cnt.
      ltp_serge_loc_cnt = ltp_serge_cnt + ltp_serge_loc_cnt.
      APPEND wa_final TO ta_final.
      CLEAR: wa_final,
             lwa_class_tmp,
             ltp_equnr_cnt,
             ltp_herst_cnt,
             ltp_typbz_cnt,
             ltp_mapar_cnt,
             ltp_serge_cnt,
             ltp_pchar_cnt.
    ENDAT.
    AT END OF tplnr.
      wa_final-tplnr = lwa_tot_tmp-tplnr.
      wa_final-arbpl = 'ALL'.
      wa_final-eqart = 'ALL'.
      wa_final-eartx = 'ALL'.
      wa_final-equnr_cnt = ltp_equnr_loc_cnt.
      IF ltp_equnr_loc_cnt IS NOT INITIAL .
        wa_final-herst_cnt = ( ltp_herst_loc_cnt * 100 ) / ltp_equnr_loc_cnt.
        wa_final-typbz_cnt = ( ltp_typbz_loc_cnt * 100 ) / ltp_equnr_loc_cnt.
        wa_final-mapar_cnt = ( ltp_mapar_loc_cnt * 100 ) / ltp_equnr_loc_cnt.
        wa_final-serge_cnt = ( ltp_serge_loc_cnt * 100 ) / ltp_equnr_loc_cnt.
      ENDIF.
      wa_final-tchar_cnt = ltp_tchar_loc_cnt.
      IF ltp_tchar_loc_cnt IS NOT INITIAL.
        wa_final-pchar_cnt = ( ltp_pchar_loc_cnt * 100 ) / ltp_tchar_loc_cnt.
      ENDIF.
      APPEND wa_final TO ta_final.
      CLEAR: wa_final,
             ltp_equnr_loc_cnt,
             ltp_herst_loc_cnt,
             ltp_typbz_loc_cnt,
             ltp_mapar_loc_cnt,
             ltp_serge_loc_cnt,
             ltp_tchar_loc_cnt,
             ltp_pchar_loc_cnt.
    ENDAT.
    CLEAR: lwa_total_tmp,
           lwa_tot_tmp.
  ENDLOOP.
  SORT ta_final BY tplnr gewrk eqart.

ENDFORM.                    " GET_OUTPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_data .
  DATA: lr_salv_table TYPE REF TO cl_salv_table,
        lr_columns    TYPE REF TO cl_salv_columns_list,
        lr_column     TYPE REF TO cl_salv_column,
        lr_aggreg     TYPE REF TO cl_salv_aggregations,
        lta_fcat      TYPE lvc_t_fcat,
        lr_display    TYPE REF TO cl_salv_display_settings,
        lr_func       TYPE REF TO cl_salv_functions_list.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lr_salv_table
        CHANGING
          t_table      = ta_final[].
    CATCH cx_salv_msg .
  ENDTRY.

  lr_columns = lr_salv_table->get_columns( ).

  TRY.
      lr_columns->set_optimize( if_salv_c_bool_sap=>true ).

      lr_column = lr_columns->get_column( columnname = 'TPLNR' ).
      lr_column->set_medium_text( 'Location' ).
      lr_column->set_long_text( 'Location' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'GEWRK' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'ARBPL' ).
      lr_column->set_short_text( 'Work Ctr.' ).
      lr_column->set_medium_text( 'Work Center' ).
      lr_column->set_long_text( 'Work Center' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'EQART' ).
      lr_column->set_short_text( 'Tech Obj.' ).
      lr_column->set_medium_text( 'Tech Obj type' ).
      lr_column->set_long_text( 'Tech Obj type' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'EARTX' ).
      lr_column->set_short_text( 'Tech Desc.' ).
      lr_column->set_medium_text( 'Tech Obj Desc' ).
      lr_column->set_long_text( 'Tech Obj Description' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'EQUNR_CNT' ).
      lr_column->set_medium_text( 'Equi Count' ).
      lr_column->set_long_text( 'Equipment Count' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'HERST_CNT' ).
      lr_column->set_medium_text( 'Details% (Manu)' ).
      lr_column->set_long_text( 'Details% (Manufacturer)' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'TYPBZ_CNT' ).
      lr_column->set_medium_text( 'Details% (Model)' ).
      lr_column->set_long_text( 'Details% (Model)' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'MAPAR_CNT' ).
      lr_column->set_medium_text( 'Details% (Part No.)' ).
      lr_column->set_long_text( 'Details% (Part Number)' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'SERGE_CNT' ).
      lr_column->set_medium_text( 'Details% (Ser No.)' ).
      lr_column->set_long_text( 'Details% (Serial Number)' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'TCHAR_CNT' ).
      lr_column->set_medium_text( 'Characteristic Count' ).
      lr_column->set_long_text( 'Characteristic Count' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'PCHAR_CNT' ).
      lr_column->set_medium_text( 'Char % Populated' ).
      lr_column->set_long_text( 'Char % Populated' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  lr_aggreg   = lr_salv_table->get_aggregations( ).
  lta_fcat    =  cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                r_columns      = lr_columns
                                r_aggregations = lr_aggreg ).

  "Functions
  lr_func = lr_salv_table->get_functions( ).
  lr_func->set_all( ).

  lr_display = lr_salv_table->get_display_settings( ).

  lr_salv_table->display( ).

ENDFORM.                    " DISPLAY_DATA
