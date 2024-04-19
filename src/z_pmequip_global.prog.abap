*----------------------------------------------------------------------*

***INCLUDE LZPM02F02 .
*&---------------------------------------------------------------------*
*All the subroutine called in Z_PMEQUIP_GLOBAL
* Created by : Jsharma
* Date :  04.12.2014
*----------------------------------------------------------------------*
* 3/12/2015 NR Remove logic related to abc indicator, locaiton and premise
*
* 2015/07/29 - Banner Equipment Interface Changes
* SDP85964
* GYMANA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data TABLES imp_equip STRUCTURE zpms_equip
                     tbl_return_tab STRUCTURE zpms_equip_ret
              CHANGING lv_err_flg.

  FIELD-SYMBOLS : <fs_equip> TYPE zpms_equip.

  DATA : ta_string TYPE  match_result_tab,
         lv_count TYPE i,
*         lv_equnr TYPE equnr,
         s_level TYPE RANGE OF tplma,
         lv_3rdlevel(16) TYPE c,
         lv_equnr TYPE equnr,
         wa_level LIKE LINE OF s_level.

  DATA: ta_equip_temp    TYPE STANDARD TABLE OF zpms_equip,
        ta_fl_level_temp TYPE STANDARD TABLE OF zpmt_fl_level.


  FIELD-SYMBOLS :<fs_iflot> TYPE ty_iflot,
                 <fs_iloa_e> TYPE ty_iloa_e,
                 <fs_inob> TYPE ty_inob.

*  REFRESH : s_level[].

*--------------------------------------------------------------------*
*-- Step 0 Get technical Object / EQART from the custom table
*--------------------------------------------------------------------*
  ta_equip_temp = imp_equip[].
  SORT ta_equip_temp BY zcat_code zgrp_code.
  DELETE ADJACENT DUPLICATES FROM ta_equip_temp COMPARING zcat_code zgrp_code.

*-- Retrieve data from Ztable based on category code and group code
  SELECT * FROM zpmt_fl_level INTO TABLE  ta_fl_level
    FOR ALL ENTRIES IN ta_equip_temp
              WHERE zcat_code = ta_equip_temp-zcat_code AND
                    zgrp_code = ta_equip_temp-zgrp_code.

  IF sy-subrc IS INITIAL.
    SORT ta_fl_level BY zcat_code zgrp_code ztech_obty.
  ENDIF.

*--------------------------------------------------------------------*
*-- Step 1 Convert external equipment number to internal equipment number
*--        Assign technical object from custom tablezpmt_fl_level
*--------------------------------------------------------------------*
  LOOP AT imp_equip ASSIGNING <fs_equip>.
    lv_equnr = <fs_equip>.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_equnr
      IMPORTING
        output = lv_equnr.
    <fs_equip>-equnr = lv_equnr.

    READ TABLE ta_fl_level INTO wa_fl_level
      WITH KEY zcat_code = <fs_equip>-zcat_code
               zgrp_code = <fs_equip>-zgrp_code BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_equip>-eqart = wa_fl_level-ztech_obty.

    ENDIF.

  ENDLOOP.
  CLEAR : lv_equnr.

*--------------------------------------------------------------------*
*-- Step 2 Get data from view v_equi
*--------------------------------------------------------------------*
  SELECT equnr
         eqtyp
         eqart
         ansdt
         herst
         serge
         typbz
         sernr
         mapar
         tidnr
         eqktx
         tplnr
         swerk
         stort
         msgrp
    FROM v_equi
    INTO TABLE ta_vequi
    FOR ALL ENTRIES IN imp_equip
    WHERE equnr = imp_equip-equnr AND
          datbi GT sy-datum AND
          spras = sy-langu.
  IF sy-subrc EQ 0.
    SORT ta_vequi BY equnr.
  ENDIF.

*--------------------------------------------------------------------*
*-- Step 3 Get the characteristis values for
*--------------------------------------------------------------------*
*-- Get Intertnal number for Charactertistic
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'P_FACTOR'
    IMPORTING
      output = gv_atinn.

* Prepare Object Ids to retrieve characteristics
  LOOP AT ta_vequi INTO wa_vequi.
    wa_objek-objek = wa_vequi-equnr.
    APPEND wa_objek TO ta_objek.
  ENDLOOP.

  SELECT cuobj
         objek
         FROM inob
         INTO TABLE ta_inob
         FOR ALL ENTRIES IN ta_objek
         WHERE obtab = 'EQUI' AND
               objek = ta_objek-objek AND
               klart = '002'.

  IF sy-subrc EQ 0.
    SORT ta_inob BY objek.
    LOOP AT ta_inob ASSIGNING <fs_inob>.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_inob>-cuobj
        IMPORTING
          output = <fs_inob>-cuobj.

      wa_objek-objek = <fs_inob>-cuobj.
      APPEND wa_objek TO ta_objek.
    ENDLOOP.
  ENDIF.

* Get Characteristic Values
  SELECT objek
         atflv
         FROM ausp
         INTO TABLE ta_ausp
         FOR ALL ENTRIES IN ta_objek
         WHERE objek = ta_objek-objek AND
               atinn EQ gv_atinn AND
               mafid = 'O' AND
               klart = '002' AND
               adzhl = space.

  IF sy-subrc EQ 0.
    SORT ta_ausp BY objek.
  ENDIF.

*--------------------------------------------------------------------*
*-- Step 4 Fetch Maintenance Plant
*--------------------------------------------------------------------*
  ta_equip_temp = imp_equip[].
  SORT ta_equip_temp BY stort.
  DELETE ADJACENT DUPLICATES FROM ta_equip_temp COMPARING stort.
*fetch maintainenc plant
  SELECT * FROM t499s INTO TABLE ta_t499s
                      FOR ALL ENTRIES IN ta_equip_temp
                      WHERE stand = ta_equip_temp-stort.
*                      ORDER BY stand.
  IF sy-subrc IS INITIAL.
    SORT ta_t499s BY stand.
  ENDIF.

ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  get_data_fl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data_fl.
*--------------------------------------------------------------------*
*-- Step 6 Get data from view v_equi for all the involved flocs
*-- This data will be used to dismantle multiple equipment installations
*--   at a single functional locations
*--------------------------------------------------------------------*
  SELECT equnr
         eqtyp
         eqart
         tplnr
    FROM v_equi
    INTO TABLE ta_vequi_fl
    FOR ALL ENTRIES IN ta_final
    WHERE tplnr = ta_final-tplnr AND
          datbi GT sy-datum AND
          spras = sy-langu.
  IF sy-subrc EQ 0.
    SORT ta_vequi_fl BY tplnr.
  ENDIF.

ENDFORM.                    "get_data_fl
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_FLOC  text
*----------------------------------------------------------------------*
FORM read_maint_plant  TABLES   tbl_return_tab STRUCTURE zpms_equip_ret
                       CHANGING lv_err_flg.

*-- fetch maintainence Plant and location based on ROOM
  READ TABLE ta_t499s INTO wa_t499s WITH KEY
                      stand = wa_equip-stort BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    wa_general-maintloc = wa_equip-stort.
    wa_general-maintplant = wa_t499s-werks.
  ELSE.
    lv_err_flg = 'X'.
    wa_ret_e-equipment = wa_equip-equnr.
    wa_ret_e-status = '2'.
    CONCATENATE 'Error:'  'Location' wa_equip-stort 'is invalid'  INTO lv_text SEPARATED BY space.
    wa_ret_e-message = lv_text.
    wa_equip_log-error = lv_text.
    APPEND wa_ret_e TO tbl_return_tab.
    CLEAR : wa_ret_e,
            lv_text.
  ENDIF.


*-- @TODO Check if this logic is required
*  CLEAR : wa_final,wa_iloa_e.
**Fetch functional location, based on (premises , location, equipmet
**category code, group code)
**override the Plant ,use the Floca plant
*  READ TABLE ta_final INTO wa_final WITH KEY equnr = wa_equip-equnr.
*  IF sy-subrc IS INITIAL.
*    SORT ta_iloa_e BY stort msgrp tplnr.
*    READ TABLE ta_iloa_e  INTO wa_iloa_e WITH KEY
*                                stort = wa_equip-stort
*                                msgrp = wa_equip-premise
*                                tplnr = wa_final-tplnr
*                                BINARY SEARCH.
*    IF sy-subrc IS INITIAL.
*      wa_general-maintplant = wa_iloa_e-swerk.
*    ENDIF.
*
*  ENDIF.

ENDFORM.                    " READ_maint_plant
*&---------------------------------------------------------------------*
*&      Form  read_equip_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TBL_RETURN_TAB  text
*      -->LV_ERR_FLG      text
*----------------------------------------------------------------------*
FORM read_equip_desc  TABLES   tbl_return_tab STRUCTURE zpms_equip_ret
                       CHANGING lv_err_flg.

*-- fetch maintainence Plant and location based on ROOM
  READ TABLE ta_fl_level INTO wa_fl_level WITH KEY
                      zcat_code = wa_equip-zcat_code
                      zgrp_code = wa_equip-zgrp_code BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    IF wa_fl_level-zdesc IS NOT INITIAL.
      wa_general-descript = wa_fl_level-zdesc .
    ELSE.
      wa_general-descript = wa_equip-eqktx .
    ENDIF.
  ELSE.
    lv_err_flg = 'X'.
    wa_ret_e-equipment = wa_equip-equnr.
    wa_ret_e-status = '2'.
    CONCATENATE 'Error:'  'Category' wa_equip-zcat_code 'Group' wa_equip-zgrp_code 'is not matching in ZPMT_FL_LEVEL' INTO lv_text SEPARATED BY space.
    wa_ret_e-message = lv_text.
    wa_equip_log-error = lv_text.
    APPEND wa_ret_e TO tbl_return_tab.
    CLEAR : wa_ret_e,
            lv_text.
  ENDIF.
  CLEAR : wa_fl_level.
ENDFORM.                    " READ_Equip_desc


*&---------------------------------------------------------------------*
*&      Form  validate_loc_premise
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TBL_RETURN_TAB  text
*      -->LV_ERR_FLG      text
*----------------------------------------------------------------------*
FORM validate_loc_premise  TABLES   tbl_return_tab STRUCTURE zpms_equip_ret
                                CHANGING lv_err_flg.

  SORT ta_iloa_e BY stort msgrp tplnr.
  READ TABLE ta_iloa_e  INTO wa_iloa_e WITH KEY
                              stort = wa_equip-stort
                              msgrp = wa_equip-premise
                              BINARY SEARCH.
  IF sy-subrc IS NOT INITIAL.

    lv_err_flg = 'X'.
    wa_ret_e-equipment = wa_equip-equnr.
    wa_ret_e-status = '2'.
    CONCATENATE 'Error:'  'Location' wa_equip-stort 'or Premises' wa_equip-premise  'is invalid'  INTO lv_text.
    wa_ret_e-message = lv_text.
    APPEND wa_ret_e TO tbl_return_tab.
    CLEAR : wa_ret_e,
            lv_text.
  ENDIF.

ENDFORM.                    " READ_DATA


*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CREATE_EQUIPMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TBL_RETURN_TAB  text
*----------------------------------------------------------------------*
FORM create_equipment  TABLES   tbl_return_tab STRUCTURE zpms_equip_ret
                       CHANGING lv_err_flg.
  DATA: lv_class_change_reqd TYPE c VALUE abap_false.

  CLEAR :lv_err_flg.

  lv_external_no = wa_equip-equnr.
*--------------------------------------------------------------------*
*-- @TBD Check what date needs to be used
  wa_equip-datsl = sy-datum.
*--------------------------------------------------------------------*
  wa_specific-equicatgry = wa_equip-eqtyp. " B and C
*  wa_general-descript = wa_equip-eqktx .
  wa_general-objecttype = wa_equip-eqart.
*-- Also set catalog profile, same as object type  "NR
  wa_general-catprofile = wa_equip-eqart. "NR
  wa_general-acqdate = wa_equip-ansdt.
  wa_general-manfacture = wa_equip-herst.
  wa_general-manmodel = wa_equip-typbz.
  wa_general-manserno = wa_equip-serge.
*  wa_general-manparno = wa_equip-mapar.
*-- Do not set the abc indicator
*  wa_general-abcindic =  'C'.".wa_equip-ABCKZ. "C
  wa_specific-techid = wa_equip-tidnr .
*-- Do not populate locaiton and premise
*  wa_general-maintloc = wa_equip-stort.
*  wa_general-maintroom = wa_equip-premise.
  CLEAR : wa_fl_level.

  CALL FUNCTION 'BAPI_EQUI_CREATE'
    EXPORTING
      external_number   = lv_external_no
      data_general      = wa_general
      data_specific     = wa_specific
    IMPORTING
      equipment         = lv_equip_created
*     DATA_GENERAL_EXP  = DATA_GENERAL_EXP
*     DATA_SPECIFIC_EXP = DATA_SPECIFIC_EXP
*     DATA_FLEET_EXP    = DATA_FLEET_EXP
      return            = wa_return.

  IF wa_return-type <>  'E'.
    IF lv_equip_created IS NOT INITIAL.
      wa_ret_e-equipment = lv_equip_created.
      wa_ret_e-status = '1'.
      CONCATENATE 'Success-' 'Equipment created'  lv_equip_created
      INTO lv_text SEPARATED BY space.

      wa_ret_e-message = lv_text.
      wa_equip_log-error = lv_text.
      CLEAR : lv_text.
      APPEND wa_ret_e TO tbl_return_tab.
      CLEAR : wa_ret_e.
      PERFORM commit USING 'X'.

*Add MD_MT class to the created Equipment
*      PERFORM create_classification USING lv_equip_created.
      PERFORM create_classification  TABLES tbl_return_tab
        USING  lv_equip_created abap_true
        CHANGING lv_err_flg
                 lv_class_change_reqd.
    ENDIF.

  ELSE.
*If error found and equipment not created, set the flag
* continue to next record, fill the return table
    lv_err_flg = 'X'.
    CLEAR : lv_text.
    wa_ret_e-equipment = lv_external_no.
    wa_ret_e-status = '2'.
    CONCATENATE 'Error-' wa_return-message INTO lv_text.
    wa_ret_e-message = lv_text.
    wa_equip_log-error = lv_text.
    APPEND wa_ret_e TO tbl_return_tab.
    CLEAR : wa_ret_e,
            lv_text.

  ENDIF.

ENDFORM.                    " CREATE_EQUIPMENT
*&---------------------------------------------------------------------*
*&      Form  DISMANTLE_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FLOC  text
*      -->P_TBL_RETURN_TAB  text
*----------------------------------------------------------------------*
FORM dismantle_equip TABLES tbl_return_tab STRUCTURE zpms_equip_ret
                      USING  lv_floc
                             CHANGING lv_err_flg.

  CALL FUNCTION 'ALM_ME_FUNCLOC_INSTALLED_EQUI'
    EXPORTING
      i_funcloc            = lv_floc
                                                                                                                                                                                                            "'DAWN-PLTE-SFCT-VALV-P1101' "functional location
    TABLES
      t_funcloc_inst_equis = ta_installed_equi.

  LOOP AT ta_installed_equi INTO wa_installed_equi.
    CLEAR: ts_dis_ret,
           wa_ret_e.

    IF wa_installed_equi-equnr = lv_equip_created.
      wa_ret_e-equipment = wa_installed_equi-equnr.
      wa_ret_e-status = '1'.
      CONCATENATE 'Success-'  'Equipment' wa_installed_equi-equnr  ' already installed on Floc' wa_installed_equi-tplnr  INTO lv_text SEPARATED BY space.
      wa_ret_e-message = lv_text.
      APPEND wa_ret_e TO tbl_return_tab.
      CLEAR : lv_text , wa_ret_e.
      lv_err_flg = 'X'.
      RETURN.

    ELSE.
*if exist any equipment , Dismantle the equipment from that location
      CALL FUNCTION 'BAPI_EQMT_DISMANTLEFL'
        EXPORTING
          equipment = wa_installed_equi-equnr
          funcloc   = wa_installed_equi-tplnr
        IMPORTING
          return    = ts_dis_ret.

      IF ts_dis_ret-type <> 'E'.
        wa_ret_e-equipment = wa_installed_equi-equnr.
        wa_ret_e-status = '1'.
        CONCATENATE 'Success-' wa_installed_equi-equnr  'Equipment Dismantle from Floc' wa_installed_equi-tplnr  INTO lv_text SEPARATED BY space.
        wa_ret_e-message = lv_text.
        APPEND wa_ret_e TO tbl_return_tab.
        CLEAR : lv_text , wa_ret_e.
*        PERFORM commit.
      ELSE.
*          lv_del_flag = 'X'.
*            error set flag if any equipment is not dismantle
        RETURN." come out of the loop, no further processing
      ENDIF.
      CLEAR : wa_installed_equi.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " DISMANTLE_EQUIP
*&---------------------------------------------------------------------*
*&      Form  INSTALL_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TBL_RETURN_TAB  text
*----------------------------------------------------------------------*
FORM install_equip  TABLES tbl_return_tab STRUCTURE zpms_equip_ret.
  FIELD-SYMBOLS : <fs_iflot> TYPE ty_iflot.
  CLEAR : wa_iflot,
          wa_fl_level,
          lv_index.
  CLEAR : lv_text, wa_ret_e.
  READ TABLE ta_final INTO wa_final WITH KEY equnr = lv_equip_created.
  IF sy-subrc IS INITIAL.
    IF wa_final-tplnr IS INITIAL.
      wa_ret_e-equipment = lv_equip_created.
      wa_ret_e-status = '2'.
      CONCATENATE 'Error-' 'Equipment No.' lv_equip_created ' not installed , Functional location not found' ts_dis_ret-message INTO  lv_text SEPARATED BY space.
      wa_ret_e-message = lv_text.
      APPEND wa_ret_e TO tbl_return_tab.
      CLEAR : lv_text, wa_ret_e.
    ELSE.
      "Directly install the Equipment
      CALL FUNCTION 'BAPI_EQMT_INSTALLFL'
        EXPORTING
          equipment = lv_equip_created
          funcloc   = wa_final-tplnr "'DAWN-PLTE-SFCT-VALV-P1101'
*         POSEQUI   = POSEQUI
*         date      = sy-datlo
*         time      = sy-timlo
        IMPORTING
          return    = ts_dis_ret.

      IF ts_dis_ret-type <> 'E'.
        wa_ret_e-equipment = lv_equip_created.
        wa_ret_e-status = '1'.
        CONCATENATE 'Success-' ' Equipment No.' lv_equip_created ' installed on the Tech Objtype and FLocation' wa_final-eqart '/' wa_final-tplnr INTO  lv_text SEPARATED BY space.
        wa_ret_e-message = lv_text.
        APPEND wa_ret_e TO tbl_return_tab.
        CLEAR : lv_text, wa_ret_e.
        PERFORM commit USING space.
        EXIT.
      ELSE.
        wa_ret_e-equipment = lv_equip_created.
        wa_ret_e-status = '2'.
        CONCATENATE 'Error-' 'Equipment No.' lv_equip_created ' not installed' ts_dis_ret-message INTO  lv_text SEPARATED BY space.
        wa_ret_e-message = lv_text.
        APPEND wa_ret_e TO tbl_return_tab.
        CLEAR : lv_text, wa_ret_e.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.                    " INSTALL_EQUIP
*&---------------------------------------------------------------------*
*&      Form  CHANGE_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TBL_RETURN_TAB  text
*----------------------------------------------------------------------*
FORM change_equip  TABLES tbl_return_tab STRUCTURE zpms_equip_ret
                   CHANGING lv_err_flg.

  FIELD-SYMBOLS: <lfs_inob> TYPE ty_inob,
               <lfs_ausp> TYPE ty_ausp.
  DATA:         lv_objek TYPE ausp-objek,
              lv_p_factor TYPE p DECIMALS 5,
                 wa_ausp TYPE ty_ausp.

*  DATA: lv_change_char TYPE c.
  DATA: lv_change_class_reqd TYPE c VALUE abap_false.
  CLEAR : wa_fl_level.

  lv_external_no = wa_equip-equnr.
  READ TABLE ta_vequi INTO wa_vequi WITH KEY equnr = lv_external_no BINARY SEARCH.
  IF sy-subrc NE 0.
    RETURN.

  ENDIF.

  wa_general-objecttype = wa_equip-eqart.

  lv_objek = lv_external_no.

  READ TABLE ta_inob ASSIGNING <lfs_inob>
                     WITH KEY objek = lv_objek
                     BINARY SEARCH.

  IF sy-subrc EQ 0.
    lv_objek = <lfs_inob>-cuobj.
  ENDIF.

*  IF  wa_equip-eqtyp <> wa_vequi-eqtyp OR
*      wa_equip-ansdt <> wa_vequi-ansdt OR
*      wa_equip-herst <> wa_vequi-herst OR
*      wa_equip-typbz <> wa_vequi-typbz OR
*      wa_equip-serge <> wa_vequi-serge OR
*      wa_equip-tidnr <> wa_vequi-tidnr.

  IF  ( ( wa_equip-ansdt IS NOT INITIAL ) AND ( wa_vequi-ansdt IS INITIAL ) ) OR
      ( ( wa_equip-herst IS NOT INITIAL ) AND ( wa_vequi-herst IS INITIAL ) ) OR
      ( ( wa_equip-typbz IS NOT INITIAL ) AND ( wa_vequi-typbz IS INITIAL ) ) OR
      ( ( wa_equip-serge IS NOT INITIAL ) AND ( wa_vequi-serge IS INITIAL ) ) OR
      ( ( wa_equip-tidnr IS NOT INITIAL ) AND ( wa_vequi-tidnr IS INITIAL ) ).
* SDP85964 - gymana - Commented out these two conditions
*      ( ( wa_equip-stort IS NOT INITIAL ) AND ( wa_vequi-stort IS INITIAL ) ) OR
*      ( ( wa_equip-premise IS NOT INITIAL ) AND ( wa_vequi-msgrp IS INITIAL ) ).

*-- Do not use locaiton and premise to detect change
*     OR
*      wa_general-maintplant <> wa_vequi-swerk OR
*      wa_equip-stort <> wa_vequi-stort OR
*      wa_equip-premise <> wa_vequi-msgrp.

    PERFORM appl_log USING 'I' 'Begin CHANGE_EQUIP' space.

    wa_specific-equicatgry = wa_equip-eqtyp. " B and C
    wa_general-acqdate = wa_equip-ansdt.
    wa_general-manfacture = wa_equip-herst.
    wa_general-manmodel = wa_equip-typbz.
    wa_general-manserno = wa_equip-serge.
*-- Do not set the abc indicator
*    wa_general-abcindic =  'C'.".wa_equip-ABCKZ. "C
    wa_specific-techid = wa_equip-tidnr .
*-- Do not populate locaiton and premise
*    wa_general-maintloc = wa_equip-stort.
*    wa_general-maintroom = wa_equip-premise.

*-- Update structure
    wa_specificx-equicatgry = 'X'."wa_equip-EQTYP. " B
    wa_generalx-acqdate = 'X'.
    wa_generalx-manfacture = 'X'.
    wa_generalx-manmodel = 'X'.
    wa_generalx-manserno = 'X'.
*-- Do not set the abc indicator
*    wa_generalx-abcindic =  'X'.".wa_equip-ABCKZ. "C
    wa_specificx-techid = 'X' .
*    wa_generalx-maintloc = 'X'.
*    wa_generalx-maintplant = 'X'.
*    wa_generalx-maintroom  = 'X'.
*    wa_generalx-objecttype = 'X'.

    CALL FUNCTION 'BAPI_EQUI_CHANGE'
      EXPORTING
        equipment      = lv_external_no
        data_general   = wa_general
        data_generalx  = wa_generalx
        data_specific  = wa_specific
        data_specificx = wa_specificx
      IMPORTING
        return         = wa_return.

    IF wa_return-type NE 'E'.
      IF lv_external_no IS NOT INITIAL.
        wa_ret_e-equipment = lv_external_no.
        wa_ret_e-status = '1'.
        CONCATENATE 'Success-' 'Equipment changed:' lv_external_no
       INTO lv_text SEPARATED BY space.
        wa_ret_e-message = lv_text.
        wa_equip_log-error = lv_text.
        CLEAR : lv_text.
        APPEND wa_ret_e TO tbl_return_tab.
        CLEAR : wa_ret_e.

        PERFORM appl_log USING 'I' 'End CHANGE_EQUIP' 'X'.

        PERFORM create_classification
          TABLES tbl_return_tab
          USING lv_external_no
                abap_false
          CHANGING lv_err_flg
                   lv_change_class_reqd.

*--- SDP85964 gymana BEGIN
        IF lv_change_class_reqd = abap_false.
           CONCATENATE wa_equip_log-error
                       'No changes to classification'
                  INTO wa_equip_log-error SEPARATED BY space.
        ELSEIF lv_change_class_reqd = abap_true.
               CONCATENATE wa_equip_log-error
                           'Classification Updated'
                      INTO wa_equip_log-error SEPARATED BY space.
        ENDIF.
*--- SDP85964 gymana END

        PERFORM commit USING 'X'.
        gv_ch = gv_ch + 1." chnge happen
      ENDIF.
    ELSE.
      lv_err_flg = 'X'.
      CLEAR : lv_text.
      wa_ret_e-equipment = lv_external_no.
      wa_ret_e-status = '2'.
      CONCATENATE 'Error-' wa_return-message INTO lv_text SEPARATED BY space.
      wa_ret_e-message = lv_text.
      wa_equip_log-error = lv_text.
      APPEND wa_ret_e TO tbl_return_tab.
      CLEAR : wa_ret_e,
              lv_text.
    ENDIF.
*-- Change in only char value
  ELSE.

    PERFORM create_classification
      TABLES tbl_return_tab
      USING lv_external_no
            abap_false
      CHANGING lv_err_flg
               lv_change_class_reqd.

    IF lv_change_class_reqd = abap_false.                "SDP85964
      CLEAR : lv_text.
      wa_ret_e-equipment = lv_external_no.
      wa_ret_e-status = '1'.
      CONCATENATE 'Information-No changes in Equipment' lv_text INTO lv_text SEPARATED BY space.
      wa_ret_e-message = lv_text.
      wa_equip_log-proc_status = '4'.
      wa_equip_log-error = lv_text.
      APPEND wa_ret_e TO tbl_return_tab.
      CLEAR : wa_ret_e,
              lv_text.
      gv_no_ch = gv_no_ch + 1. " no chnge happen
    ELSEIF lv_change_class_reqd = abap_true.
               CONCATENATE wa_equip_log-error
                           'Classification Updated'
                      INTO wa_equip_log-error SEPARATED BY space.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHANGE_EQUIP
*&---------------------------------------------------------------------*
*&      Form  EQUIP_ON_CURRENT_FLOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM equip_on_current_floc TABLES imp_equip STRUCTURE zpms_equip.

  DATA: lv_equip TYPE bapi_itob_parms-equipment,
        lv_floc  TYPE bapi_itob_parms-funcloc,
        lv_index TYPE sy-tabix.

*
  PERFORM appl_log USING 'I' 'Begin Dismantling Equipments' space.

  READ TABLE ta_vequi WITH KEY tplnr = wa_iflot-tplnr
                               TRANSPORTING NO FIELDS
                               BINARY SEARCH.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  lv_index = sy-tabix.
  LOOP AT ta_vequi INTO wa_vequi FROM lv_index.

    IF wa_vequi-tplnr NE wa_iflot-tplnr.
      EXIT.
    ENDIF.
*Dimantle the Equipment only which need create
    READ TABLE ta_final INTO wa_final WITH KEY
                        equnr = wa_vequi-equnr
                        chnge = 'X'.
    IF sy-subrc IS NOT INITIAL.
*Dismanmtle the Equipment which not matches with Banner equipment
      READ TABLE imp_equip WITH KEY equnr = wa_vequi-equnr
                           TRANSPORTING NO FIELDS
                           BINARY SEARCH.

      IF sy-subrc IS NOT INITIAL.
        lv_equip = wa_vequi-equnr.
        lv_floc  = wa_vequi-tplnr.

        CALL FUNCTION 'BAPI_EQUI_DISMANTLE'
          EXPORTING
            equipment = lv_equip
            funcloc   = lv_floc
*           date      = sy-datlo
*           time      = sy-timlo
          IMPORTING
            return    = ts_dis_ret2.
      ENDIF.

    ENDIF.
  ENDLOOP.

  PERFORM appl_log USING 'I' 'End Dismantling Equipments' 'X'.

ENDFORM.                    " EQUIP_ON_CURRENT_FLOC
*&---------------------------------------------------------------------*
*&      Form  INSTALL_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TBL_RETURN_TAB  text
*----------------------------------------------------------------------*
FORM install_change_equip  TABLES tbl_return_tab STRUCTURE zpms_equip_ret.

  FIELD-SYMBOLS : <fs_iflot> TYPE ty_iflot.
  CLEAR : wa_iflot,
          wa_fl_level,
          lv_index.

  READ TABLE ta_final INTO wa_final WITH KEY equnr = lv_external_no
                                    chnge = 'X'. " only chnge equipment
  IF sy-subrc IS INITIAL.

    IF wa_final-installed = ''. " Change Equipment need to installed

      PERFORM appl_log USING 'I' 'Begin INSTALL_EQUIP' space.
      "Directly install the Equipment
      CALL FUNCTION 'BAPI_EQMT_INSTALLFL'
        EXPORTING
          equipment = lv_external_no
          funcloc   = wa_final-tplnr "'DAWN-PLTE-SFCT-VALV-P1101'
*         POSEQUI   = POSEQUI
*         date      = sy-datlo
*         time      = sy-timlo
        IMPORTING
          return    = ts_dis_ret.

      IF ts_dis_ret-type <> 'E'.
        wa_ret_e-equipment = lv_external_no.
        wa_ret_e-status = '1'.
        CONCATENATE 'Success-' ' Equipment No.' lv_external_no ' installed on the Tech Objtype and FLocation' wa_final-eqart '/' wa_final-tplnr INTO  lv_text SEPARATED BY space.
        wa_ret_e-message = lv_text.
        APPEND wa_ret_e TO tbl_return_tab.
        CLEAR : lv_text, wa_ret_e.
        PERFORM commit USING space.
        PERFORM appl_log USING 'I' 'End INSTALL_EQUIP' 'X'.
        EXIT.
      ELSE.
        wa_ret_e-equipment = lv_external_no.
        wa_ret_e-status = '2'.
        CONCATENATE 'Error-' 'Equipment No.' lv_external_no ' not installed' ts_dis_ret-message INTO  lv_text SEPARATED BY space.
        wa_ret_e-message = lv_text.
        APPEND wa_ret_e TO tbl_return_tab.
        CLEAR : lv_text, wa_ret_e.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " INSTALL_EQUIP
