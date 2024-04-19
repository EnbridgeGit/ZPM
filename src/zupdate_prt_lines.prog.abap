*{   INSERT         D30K924373                                        2
*&---------------------------------------------------------------------*
*&  Include           ZUPDATE_PRT_LINES
*This routine is used to add PRT lines per operation
*Object : EAM-PM-E-076-N_Load_Inspection_Points
*Author : Jyoti Sharma
*Date   : 17-06-2014
************************************************************************
*02/Sep/2014 New changes :- keep existing PRT lines Jsharma
*04/Feb/2015 Update terminate issue
*            The PRT Item counter and Status records were getting duplicated
*            Handled the issue to check and use the right PRT Item counter
*11/Feb/2015 Duplicate measurement point PRTS issue
*&---------------------------------------------------------------------*
  FIELD-SYMBOLS :   <fs_op> TYPE afvgb.
  TYPES: BEGIN OF ty_iflot,
    tplnr TYPE tplnr,
    objnr TYPE j_objnr,
    END OF   ty_iflot,

    BEGIN OF ty_equi,
    equnr TYPE equnr,
    objnr TYPE j_objnr,
   END OF ty_equi.

  TYPES : BEGIN OF ty_imptt,
    point TYPE imrc_point,
    mpobj TYPE imrc_mpobj,
    psort TYPE imrc_psort,
    END OF ty_imptt.

*Added on 2/4/2015 by sghosh
  TYPES : BEGIN OF ty_affh,
           aufpl TYPE co_aufpl,
           pzlfh TYPE pzlfh,
           objty TYPE objty,
           objid TYPE objid,
           aplzl TYPE cim_count,
           psnfh TYPE psnfh,
          END OF ty_affh.
*added on 2/4/2015 by sghosh

  DATA : wa_list_meas TYPE zpmt_list_meas,
         wa_affh TYPE affhb,
         ls_iflot TYPE ty_iflot,
         ls_equi  TYPE ty_equi,
         lv_obty TYPE cr_objty,
         lv_obid TYPE cr_objid,
         lv_objnr TYPE j_objnr,
         lv_objnr_temp TYPE j_objnr,
         lv_jsto_flg TYPE c,
         lv_cnt(4) TYPE n,
         lv_no(8) TYPE n,
         lv_index TYPE i,
         ta_affh TYPE STANDARD TABLE OF ty_affh,
         ta_affh_temp TYPE STANDARD TABLE OF ty_affh,
         wa_affh_temp TYPE ty_affh,
         wa_affh_bt_temp TYPE affhb,
         gt_affh_bt1  TYPE STANDARD TABLE OF affhb,
         ta_imptt TYPE STANDARD TABLE OF ty_imptt,
         ta_imptt_f TYPE STANDARD TABLE OF ty_imptt,
         ta_imptt_e TYPE STANDARD TABLE OF ty_imptt,
         ta_jest_i TYPE STANDARD TABLE OF jest_upd,
         ta_jest_u TYPE STANDARD TABLE OF jest_upd,
         ta_jsto_i TYPE STANDARD TABLE OF jsto,
         wa_jsto_i TYPE jsto,
         ta_jsto_u TYPE STANDARD TABLE OF jsto_upd,
         wa_jsto_u TYPE jsto_upd,
         ta_obj_del TYPE STANDARD TABLE OF onr00,
         wa_imptt TYPE ty_imptt,
         ta_list_meas TYPE STANDARD TABLE OF zpmt_list_meas.

*-- If Functional location Exists, then proceed with the logic.
  IF caufv_bt-tplnr IS NOT INITIAL OR
     caufv_bt-equnr IS NOT INITIAL. " Added by Eldhose

    IF caufv_bt-tplnr IS NOT INITIAL.
      SELECT SINGLE tplnr objnr FROM iflot INTO ls_iflot WHERE
                         tplnr = caufv_bt-tplnr.
      IF ls_iflot IS NOT INITIAL.
        SELECT point mpobj  psort FROM imptt INTO TABLE ta_imptt_f
             WHERE mpobj = ls_iflot-objnr.

        IF ta_imptt_f IS NOT INITIAL.
          SORT ta_imptt_f BY psort.
        ENDIF.
      ENDIF.
    ENDIF.

    IF caufv_bt-equnr IS NOT INITIAL.
      SELECT SINGLE equnr objnr FROM equi INTO ls_equi WHERE
                         equnr = caufv_bt-equnr.
      IF ls_equi IS NOT INITIAL.
        SELECT point mpobj psort FROM imptt INTO TABLE ta_imptt_e
             WHERE mpobj = ls_equi-objnr.

        IF ta_imptt_e IS NOT INITIAL.
          SORT ta_imptt_e BY psort.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND LINES OF ta_imptt_f TO ta_imptt.
    APPEND LINES OF ta_imptt_e TO ta_imptt.
    IF ta_imptt IS NOT INITIAL.
      SORT ta_imptt BY psort.
    ENDIF.

    IF ta_imptt[] IS NOT INITIAL.
      SELECT * FROM zpmt_list_meas
          INTO TABLE ta_list_meas
          FOR ALL ENTRIES IN ta_imptt
          WHERE code = ta_imptt-psort.
      IF sy-subrc IS INITIAL.
        SORT ta_list_meas BY vornr steus.
      ENDIF.
    ENDIF.

    CLEAR : lv_cnt,
            lv_no.
    REFRESH : ta_affh[],
             ta_jsto_i[].

*Code added on 2/4/2014 by sghosh
    SELECT aufpl pzlfh objty objid aplzl psnfh FROM affh INTO TABLE ta_affh FOR ALL ENTRIES IN afvg_bt
                 WHERE aufpl = afvg_bt-aufpl AND
                       aplzl = afvg_bt-aplzl.
*Code added on 2/4/2014 by sghosh
*    SELECT * FROM affh INTO TABLE ta_affh FOR ALL ENTRIES IN afvg_bt
*                 WHERE aufpl = afvg_bt-aufpl AND
*                       aplzl = afvg_bt-aplzl.
    CLEAR : lv_no.

    SORT gt_affh_bt BY pzlfh.

*-- Item counter for PRT
    IF gt_affh_bt[] IS NOT INITIAL."Current PRT lines added
*-- Get the Item counter based on new PRT lines added
      DESCRIBE TABLE gt_affh_bt LINES lv_index.
      READ TABLE gt_affh_bt INTO wa_affh_bt_temp INDEX lv_index.
      IF sy-subrc IS INITIAL.
        lv_no = wa_affh_bt_temp-pzlfh.
      ENDIF.
    ELSEIF ta_affh IS NOT INITIAL.
*-- Get the Item counter based on existing PRT lines
      DESCRIBE TABLE ta_affh LINES lv_index.
      READ TABLE ta_affh INTO wa_affh_temp INDEX lv_index.
      IF sy-subrc IS INITIAL.
        lv_no = wa_affh_temp-pzlfh.
      ENDIF.

    ENDIF.

    SORT ta_affh BY aufpl aplzl objid.
    SORT gt_affh_bt BY aufpl aplzl objid  objid.

*chk if operation lines has task list

*SELECT meas_point objty objid FROM crvp_b INTO TABLE ta_crvp_b.
    LOOP AT afvg_bt[] ASSIGNING <fs_op> WHERE
            plnty = 'A' AND phflg <> 'X'.
      CLEAR : lv_cnt,
              lv_index,
              wa_affh_bt_temp.

      REFRESH:  gt_affh_bt1[]."temporary tables

      gt_affh_bt1[] = gt_affh_bt[].

*-- Get the internal counter for PRT
**pick the last no. from the Current GT_AFFH table
      IF gt_affh_bt1[] IS NOT INITIAL."Current PRT lines added
        SORT gt_affh_bt1 BY psnfh.
        DELETE gt_affh_bt1[] WHERE NOT ( aufpl = <fs_op>-aufpl AND
        aplzl = <fs_op>-aplzl ).
        DESCRIBE TABLE gt_affh_bt1 LINES lv_index.
        READ TABLE gt_affh_bt1 INTO wa_affh_bt_temp INDEX lv_index.
        IF sy-subrc IS INITIAL.
          lv_cnt = wa_affh_bt_temp-psnfh.
        ENDIF.
      ELSEIF ta_affh IS NOT INITIAL.
*-- If there are no current line items, pick the info from the database table.
        ta_affh_temp[] = ta_affh[].
        DELETE ta_affh_temp[] WHERE NOT ( aufpl = <fs_op>-aufpl AND
            aplzl = <fs_op>-aplzl ).

        DESCRIBE TABLE ta_affh_temp LINES lv_index.
        READ TABLE ta_affh_temp INTO wa_affh_temp INDEX lv_index.
        IF sy-subrc IS INITIAL.
          lv_cnt = wa_affh_temp-psnfh.
        ENDIF.
      ENDIF.

*if it has task list---
*chk ztable measuring code matches with function loc measuring point (has measuring code)..
*add these measuring point in the operation lines.
*add the lines based on the sequence in the ztable

*fetch the objid for measuring point if already used in any other Work order

      LOOP AT ta_list_meas INTO wa_list_meas WHERE
                plnnr = <fs_op>-plnnr AND
                plnal  = <fs_op>-plnal AND
                vornr = <fs_op>-vornr.

        CASE wa_list_meas-tcobj.
          WHEN 'E'.
            READ TABLE ta_imptt_e INTO wa_imptt WITH KEY
                                       psort = wa_list_meas-code
                                       BINARY SEARCH.
          WHEN 'F'.
            READ TABLE ta_imptt_f INTO wa_imptt WITH KEY
                                       psort = wa_list_meas-code
                                       BINARY SEARCH.
        ENDCASE.

        IF sy-subrc IS INITIAL. "Meas Position is valid for floc - Check
          SELECT SINGLE objty objid FROM crvp_b
                 INTO (lv_obty , lv_obid)
                 WHERE meas_point = wa_imptt-point.
          IF sy-subrc IS NOT INITIAL.
*Measuring point used first time ,used below FM to get objid
            CALL FUNCTION 'CF_ST_MPO_UPDATE'
              EXPORTING
                iv_aktyp       = 'IK01'
                iv_meas_point  = wa_imptt-point
                iv_in_upd_task = 'X'
              IMPORTING
                ev_objid       = lv_obid
                ev_objty       = lv_obty
              EXCEPTIONS
                wrong_aktyp    = 1
                OTHERS         = 2.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ENDIF.
          ENDIF.


*--check if current Measuring point is addedto the Operation
*-- If yes, skip the record
          READ TABLE gt_affh_bt INTO wa_affh_bt_temp WITH KEY  aufpl = <fs_op>-aufpl
                                                               aplzl = <fs_op>-aplzl
                                                               objty = lv_obty
                                                               objid = lv_obid BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.

            READ TABLE ta_affh INTO wa_affh_temp WITH KEY aufpl = <fs_op>-aufpl
                                                       aplzl = <fs_op>-aplzl
                                                       objty = lv_obty
                                                       objid = lv_obid BINARY SEARCH.
            IF sy-subrc IS NOT INITIAL.
              <fs_op>-tplnr = caufv_bt-tplnr.
              wa_affh-flg_sel  = 'X'.
              wa_affh-mandt = sy-mandt.
              wa_affh-aufpl = <fs_op>-aufpl.
              lv_no = lv_no + 1.
              wa_affh-pzlfh = lv_no .

              wa_affh-datui = sy-datum.
              wa_affh-useri = sy-uname.
              wa_affh-datuc = sy-datum.
              wa_affh-userc = sy-uname.
              wa_affh-objty = lv_obty. "'FH'
              wa_affh-objid = lv_obid. "'10000008'. " measuring point
              wa_affh-objct = 'O'.
              wa_affh-aplzl =  <fs_op>-aplzl.
              lv_cnt = lv_cnt + 10.
              wa_affh-psnfh  = lv_cnt.

              CONCATENATE 'OF' <fs_op>-aufpl wa_affh-pzlfh INTO lv_objnr.
              wa_affh-objnr = lv_objnr.
              wa_affh-steuf = '2'.
              wa_affh-bzoffb = 02.
              wa_affh-offstb = 0.
              wa_affh-bzoffe = 04.
              wa_affh-offste = 0.
              wa_affh-fsfhd = sy-datum.
              wa_affh-fsfhz = sy-uzeit.
              wa_affh-fefhd = sy-datum.
              wa_affh-fefhz = sy-uzeit.
              wa_affh-ssfhd = sy-datum.
              wa_affh-ssfhz = sy-uzeit.
              wa_affh-sefhd = sy-datum.
              wa_affh-sefhz =  sy-uzeit.
              wa_affh-daueh = 'TAG'.
              wa_affh-mgeinh = 'EA'.
              wa_affh-mgvgw = 1.
              wa_affh-mgsoleh  = 'EA'.
              wa_affh-mgsol  = 1.
              wa_affh-mgisteh = 'EA'.
              wa_affh-mgrsteh  = 'EA'.
              wa_affh-mgrst  = 1.
              wa_affh-mgauseh = 'EA'.
              wa_affh-aufnr = caufv_bt-aufnr.
              wa_affh-modkz = 'N'.
              APPEND wa_affh TO  gt_affh_bt.

*-- Create Status table to update
*-- Check if there is an entry already and handle accordingly
              SELECT SINGLE objnr INTO lv_objnr_temp FROM jsto WHERE objnr = lv_objnr.
              IF  sy-subrc EQ 0.
*-- If there is an already entry just update it.
                lv_jsto_flg = 'X'.
                wa_jsto_u-mandt = sy-mandt.
                wa_jsto_u-objnr = lv_objnr.
                wa_jsto_u-obtyp = 'OFP'.
                wa_jsto_u-chgkz = 'X'.
                wa_jsto_u-chgnr =  '001'.
                APPEND  wa_jsto_u  TO ta_jsto_u.
                CLEAR :wa_jsto_u.
              ELSE.
*-- If there is not entry create it.
                lv_jsto_flg = 'X'.
                wa_jsto_i-mandt = sy-mandt.
                wa_jsto_i-objnr = lv_objnr.
                wa_jsto_i-obtyp = 'OFP'.
                wa_jsto_i-chgkz = 'X'.
                wa_jsto_i-chgnr =  '001'.
                APPEND  wa_jsto_i  TO ta_jsto_i.
                CLEAR :wa_jsto_i.
              ENDIF.

            ENDIF." PRT chk
          ENDIF.
        ENDIF. "Meas Position is valid for floc - Check
      ENDLOOP.

    ENDLOOP.

  ENDIF. " Functional location Exists check

  IF lv_jsto_flg = 'X'.

    SORT ta_jsto_i.
    DELETE ADJACENT DUPLICATES FROM ta_jsto_i COMPARING ALL FIELDS.

    SORT ta_jsto_u.
    DELETE ADJACENT DUPLICATES FROM ta_jsto_u COMPARING ALL FIELDS.
*-- Call status update function module
    CALL FUNCTION 'STATUS_UPDATE' IN UPDATE TASK
      TABLES
        jest_ins = ta_jest_i
        jest_upd = ta_jest_u
        jsto_ins = ta_jsto_i
        jsto_upd = ta_jsto_u
        obj_del  = ta_obj_del.

  ENDIF.
