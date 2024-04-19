
*&---------------------------------------------------------------------*
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
*10/Feb/2015 Fix the issue - When there are no Measurement points associated
*            with a Floc, the program was creating PRTs.
*23/Dec/2017 ACR-5771 Issues with maintenance plans
*11/Jan/2018 INC0820246  Issue with SAP PM workorders

  DATA : ta_iflot TYPE STANDARD TABLE OF ty_iflot,
         ta_equi  TYPE STANDARD TABLE OF ty_equi,
         wa_caufv TYPE caufvdb.
  CLEAR : lv_jsto_flg .

  REFRESH : ta_affh[],
            ta_jsto_i[].

*-- Get all the object numbers for functional locations
  IF caufv_bt[] IS NOT INITIAL.
    SELECT tplnr objnr FROM iflot INTO TABLE ta_iflot FOR ALL ENTRIES IN caufv_bt WHERE
                                                          tplnr = caufv_bt-tplnr.

    SELECT equnr objnr FROM equi  INTO TABLE ta_equi  FOR ALL ENTRIES IN caufv_bt WHERE
                                                          equnr = caufv_bt-equnr.
  ENDIF.

*-- Get all the measurement points corresponding to the above FLocs
  IF NOT ta_iflot IS INITIAL.
    SELECT point mpobj  psort FROM imptt INTO TABLE ta_imptt_f FOR ALL ENTRIES IN ta_iflot
                                                          WHERE mpobj = ta_iflot-objnr.
    IF sy-subrc EQ 0.
      SORT ta_imptt_f BY psort.
    ELSE.
      CLEAR ta_imptt_f[].
    ENDIF.
  ENDIF.

  IF NOT ta_equi IS INITIAL.
    SELECT point mpobj  psort FROM imptt INTO TABLE ta_imptt_e FOR ALL ENTRIES IN ta_equi
                                                          WHERE mpobj = ta_equi-objnr.
    IF sy-subrc EQ 0.
      SORT ta_imptt_e BY psort.
    ELSE.
      CLEAR ta_imptt_e[].
    ENDIF.
  ENDIF.

  APPEND LINES OF ta_imptt_e TO ta_imptt.
  APPEND LINES OF ta_imptt_f TO ta_imptt.
  IF ta_imptt IS NOT INITIAL.
    SORT ta_imptt BY psort.
  ENDIF.

*-- Get the measuring positions data from custom table.
  IF ta_imptt[] IS NOT INITIAL.
    SELECT * FROM zpmt_list_meas
        INTO TABLE ta_list_meas
        FOR ALL ENTRIES IN ta_imptt
        WHERE code = ta_imptt-psort.
    IF sy-subrc IS INITIAL.
      SORT ta_list_meas BY vornr steus.
    ENDIF.
  ENDIF.

  CLEAR: lv_jsto_flg,
         ta_jsto_i[],
         ta_jsto_u[].

  LOOP AT caufv_bt INTO wa_caufv WHERE tplnr IS NOT INITIAL .
    READ TABLE ta_iflot INTO ls_iflot WITH KEY tplnr = wa_caufv-tplnr.
    IF sy-subrc EQ 0.
      READ TABLE ta_imptt_f INTO wa_imptt WITH KEY mpobj = ls_iflot-objnr.
      IF  sy-subrc EQ 0. "Measuring Point Exists for floc
        CLEAR : lv_cnt,
                lv_no.
        CLEAR : lv_no.

        SORT gt_affh_bt BY pzlfh.
        SORT gt_affh_bt BY aufpl aplzl objid  objid.

*chk if operation lines has task list
        LOOP AT afvg_bt[] ASSIGNING <fs_op> WHERE
                aufpl = wa_caufv-aufpl AND
                plnty = 'A' AND phflg <> 'X'.
          CLEAR : lv_cnt,
                  lv_index,
                  wa_affh_bt_temp.

          REFRESH:  gt_affh_bt1[]."temporary tables
          gt_affh_bt1[] = gt_affh_bt[].

**pick the last no. from the Current GT_AFFH table
          IF gt_affh_bt1[] IS NOT INITIAL."Current PRT lines added
*BOC by KMB INC0820246  11.01.2018 Issue with SAP PM workorders
*            SORT gt_affh_bt1 BY psnfh.
            SORT gt_affh_bt1 BY aufpl pzlfh.
*            DELETE gt_affh_bt1[] WHERE NOT ( aufpl = <fs_op>-aufpl AND
*            aplzl = <fs_op>-aplzl ).
            DELETE gt_affh_bt1[] WHERE NOT aufpl = <fs_op>-aufpl.
*EOC by KMB INC0820246  11.01.2018 Issue with SAP PM workorders
            DESCRIBE TABLE gt_affh_bt1 LINES lv_index.
            READ TABLE gt_affh_bt1 INTO wa_affh_bt_temp INDEX lv_index.
            IF sy-subrc IS INITIAL.
              lv_no = wa_affh_bt_temp-pzlfh.
            ENDIF.
          ENDIF.

          REFRESH:  gt_affh_bt1[]."temporary tables
          gt_affh_bt1[] = gt_affh_bt[].

**pick the last no. from the Current GT_AFFH table
          IF gt_affh_bt1[] IS NOT INITIAL."Current PRT lines added
*BOC by KMB INC0820246  11.01.2018 Issue with SAP PM workorders
*            SORT gt_affh_bt1 BY psnfh.
            SORT gt_affh_bt1 BY aufpl pzlfh.
*            DELETE gt_affh_bt1[] WHERE NOT ( aufpl = <fs_op>-aufpl AND
*            aplzl = <fs_op>-aplzl ).
            DELETE gt_affh_bt1[] WHERE NOT aufpl = <fs_op>-aufpl.
*EOC by KMB INC0820246  11.01.2018 Issue with SAP PM workorders
            DESCRIBE TABLE gt_affh_bt1 LINES lv_index.
            READ TABLE gt_affh_bt1 INTO wa_affh_bt_temp INDEX lv_index.
            IF sy-subrc IS INITIAL.
              lv_cnt = wa_affh_bt_temp-psnfh.
            ENDIF.
          ENDIF.

*if it has task list---
*chk ztable measuring code matches with function loc measuring point (has measuring code)..
*add these measuring point in the operation lines.
*add the lines based on the sequence in the ztable
          LOOP AT ta_list_meas INTO wa_list_meas WHERE
                    plnnr = <fs_op>-plnnr AND
                    plnal  = <fs_op>-plnal AND
                    vornr = <fs_op>-vornr AND
                    tcobj = 'F'.
            READ TABLE ta_imptt_f INTO wa_imptt WITH KEY mpobj = ls_iflot-objnr
                                                         psort = wa_list_meas-code.
            IF sy-subrc IS INITIAL. "Meas Position exists on the floc
*fetch the objid for measuring point if already used in any other Work order
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

**check if current Measuring point is addedto the Operation
*Skip the record
              READ TABLE gt_affh_bt INTO wa_affh_bt_temp WITH KEY  aufpl = <fs_op>-aufpl
                                                                   aplzl = <fs_op>-aplzl
                                                                   objty = lv_obty
                                                                   objid = lv_obid BINARY SEARCH.
              IF sy-subrc IS NOT INITIAL.
                <fs_op>-tplnr = wa_caufv-tplnr.
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
*BOC by KMB ACR-5771 23.12.2017 Issues with maintenance plans
*                DATA : wa_affh_bt_temp_n TYPE affhb.
*                READ TABLE gt_affh_bt INTO wa_affh_bt_temp_n WITH KEY objnr = lv_objnr.
*                IF sy-subrc = 0.
*                  lv_no = lv_no + 1.
*                  wa_affh-pzlfh = lv_no.
*                  CLEAR : lv_objnr, wa_affh-objnr.
*                  CONCATENATE 'OF' <fs_op>-aufpl wa_affh-pzlfh INTO lv_objnr.
*                  wa_affh-objnr = lv_objnr.
*                ENDIF.
*EOC by KMB ACR-5771 23.12.2017 Issues with maintenance plans

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
                wa_affh-aufnr = wa_caufv-aufnr.
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
            ENDIF. "Meas Position exists on the floc Check
          ENDLOOP.
        ENDLOOP.
      ENDIF. "Measuring Point Exists for floc chk
    ENDIF. " flocation Object Chk
  ENDLOOP.

  LOOP AT caufv_bt INTO wa_caufv WHERE equnr IS NOT INITIAL .
    READ TABLE ta_equi INTO ls_equi WITH KEY equnr = wa_caufv-equnr.
    IF sy-subrc EQ 0.
      READ TABLE ta_imptt_e INTO wa_imptt WITH KEY mpobj = ls_equi-objnr.
      IF  sy-subrc EQ 0. "Measuring Point Exists for floc
        CLEAR : lv_cnt,
                lv_no.
        CLEAR : lv_no.

        SORT gt_affh_bt BY pzlfh.
        SORT gt_affh_bt BY aufpl aplzl objid  objid.

*chk if operation lines has task list
        LOOP AT afvg_bt[] ASSIGNING <fs_op> WHERE
                aufpl = wa_caufv-aufpl AND
                plnty = 'A' AND phflg <> 'X'.
          CLEAR : lv_cnt,
                  lv_index,
                  wa_affh_bt_temp.

          REFRESH:  gt_affh_bt1[]."temporary tables
          gt_affh_bt1[] = gt_affh_bt[].

**pick the last no. from the Current GT_AFFH table
          IF gt_affh_bt1[] IS NOT INITIAL."Current PRT lines added
*BOC by KMB INC0820246  11.01.2018 Issue with SAP PM workorders
*            SORT gt_affh_bt1 BY psnfh.
            SORT gt_affh_bt1 BY aufpl pzlfh.
*            DELETE gt_affh_bt1[] WHERE NOT ( aufpl = <fs_op>-aufpl AND
*            aplzl = <fs_op>-aplzl ).
             DELETE gt_affh_bt1[] WHERE NOT aufpl = <fs_op>-aufpl.
*EOC by KMB INC0820246  11.01.2018 Issue with SAP PM workorders
            DESCRIBE TABLE gt_affh_bt1 LINES lv_index.
            READ TABLE gt_affh_bt1 INTO wa_affh_bt_temp INDEX lv_index.
            IF sy-subrc IS INITIAL.
              lv_no = wa_affh_bt_temp-pzlfh.
            ENDIF.
          ENDIF.

          REFRESH:  gt_affh_bt1[]."temporary tables
          gt_affh_bt1[] = gt_affh_bt[].

**pick the last no. from the Current GT_AFFH table
          IF gt_affh_bt1[] IS NOT INITIAL."Current PRT lines added
*BOC by KMB INC0820246  11.01.2018 Issue with SAP PM workorders
*            SORT gt_affh_bt1 BY psnfh.
            SORT gt_affh_bt1 BY aufpl pzlfh.
*            DELETE gt_affh_bt1[] WHERE NOT ( aufpl = <fs_op>-aufpl AND
*            aplzl = <fs_op>-aplzl ).
            DELETE gt_affh_bt1[] WHERE NOT aufpl = <fs_op>-aufpl.
*EOC by KMB INC0820246  11.01.2018 Issue with SAP PM workorders
            DESCRIBE TABLE gt_affh_bt1 LINES lv_index.
            READ TABLE gt_affh_bt1 INTO wa_affh_bt_temp INDEX lv_index.
            IF sy-subrc IS INITIAL.
              lv_cnt = wa_affh_bt_temp-psnfh.
            ENDIF.
          ENDIF.

*if it has task list---
*chk ztable measuring code matches with function loc measuring point (has measuring code)..
*add these measuring point in the operation lines.
*add the lines based on the sequence in the ztable
          LOOP AT ta_list_meas INTO wa_list_meas WHERE
                    plnnr = <fs_op>-plnnr AND
                    plnal  = <fs_op>-plnal AND
                    vornr = <fs_op>-vornr AND
                    tcobj = 'E'.
            READ TABLE ta_imptt_e INTO wa_imptt WITH KEY mpobj = ls_equi-objnr
                                                         psort = wa_list_meas-code.
            IF sy-subrc IS INITIAL. "Meas Position exists on the floc
*fetch the objid for measuring point if already used in any other Work order
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

**check if current Measuring point is addedto the Operation
*Skip the record
              READ TABLE gt_affh_bt INTO wa_affh_bt_temp WITH KEY  aufpl = <fs_op>-aufpl
                                                                   aplzl = <fs_op>-aplzl
                                                                   objty = lv_obty
                                                                   objid = lv_obid BINARY SEARCH.
              IF sy-subrc IS NOT INITIAL.
                <fs_op>-equnr = wa_caufv-equnr.
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
*BOC by KMB ACR-5771 23.12.2017 Issues with maintenance plans
*                READ TABLE gt_affh_bt INTO wa_affh_bt_temp_n WITH KEY objnr = lv_objnr.
*                IF sy-subrc = 0.
*                  lv_no = lv_no + 1.
*                  wa_affh-pzlfh = lv_no.
*                  CLEAR : lv_objnr, wa_affh-objnr.
*                  CONCATENATE 'OF' <fs_op>-aufpl wa_affh-pzlfh INTO lv_objnr.
*                  wa_affh-objnr = lv_objnr.
*                ENDIF.
*EOC by KMB ACR-5771 23.12.2017 Issues with maintenance plans
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
                wa_affh-aufnr = wa_caufv-aufnr.
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
            ENDIF. "Meas Position exists on the floc Check
          ENDLOOP.
        ENDLOOP.
      ENDIF. "Measuring Point Exists for floc chk
    ENDIF. " flocation Object Chk
  ENDLOOP.

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
