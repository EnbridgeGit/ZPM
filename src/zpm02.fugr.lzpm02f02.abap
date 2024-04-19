**----------------------------------------------------------------------*
*
****INCLUDE LZPM02F02 .
**----------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**All the subroutine called in ZPM02 FG
**&---------------------------------------------------------------------*
**&      Form  GET_DATA
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM get_data TABLES imp_equip STRUCTURE zpms_equip
*                     tbl_return_tab STRUCTURE zpms_equip_ret
*              CHANGING lv_err_flg.
*
*  DATA : ta_string TYPE  match_result_tab,
*         lv_count TYPE i,
*         s_level TYPE RANGE OF tplma,
*         lv_3rdlevel(16) TYPE c,
*         wa_level LIKE LINE OF s_level.
*  FIELD-SYMBOLS :<fs_iflot> TYPE ty_iflot.
*  REFRESH : s_level[].
** fetch all the equipment , if created
*  SELECT equnr FROM equi  INTO TABLE ta_equi
*                       FOR ALL ENTRIES IN imp_equip
*                       WHERE equnr = imp_equip-equnr.
*  IF sy-subrc IS INITIAL.
*    SORT ta_equi BY equnr.
*  ENDIF.
*
**fetch maintainenc plant
*  SELECT * FROM t499s INTO TABLE ta_t499s
*                       FOR ALL ENTRIES IN imp_equip
*                       WHERE stand = imp_equip-stort.
*
*  IF sy-subrc IS INITIAL.
*    SORT ta_t499s BY stand.
*  ENDIF.
*
***fetch the function location level 3 based on premise and station id.
*  SELECT tplnr swerk stort msgrp FROM iloa INTO TABLE ta_iloa_e
*                            FOR ALL ENTRIES IN imp_equip
*                            WHERE stort = imp_equip-stort AND
*                                    msgrp = imp_equip-premise.
**                                                .
*  IF sy-subrc IS INITIAL .
**will get level 3 functional location
*    DELETE ta_iloa_e WHERE tplnr = ''.
*    SORT ta_iloa_e BY tplnr.
*  ENDIF.
*
**Retrieve data from Ztable based on category code and group code
*  SELECT * FROM zpmt_fl_level INTO TABLE  ta_fl_level
*    FOR ALL ENTRIES IN imp_equip
*              WHERE zcat_code = imp_equip-zcat_code AND
*                    zgrp_code =  imp_equip-zgrp_code .
*  IF sy-subrc IS INITIAL.
*    SORT ta_fl_level BY zcat_code zgrp_code ztech_obty.
*  ENDIF.
*
*
**formulate the level 4 flocation
*  LOOP AT imp_equip INTO wa_equip.
*    CLEAR : wa_fl_level,
*            wa_iloa_e,
*            wa_level,
*            lv_floc.
*
*    IF wa_equip-eqtyp = 'B' OR wa_equip-eqtyp = 'C' .
*
*      IF wa_equip-stort IS NOT INITIAL AND wa_equip-premise IS NOT INITIAL." skip the record where any of them is blank
*
*        LOOP AT ta_iloa_e INTO wa_iloa_e WHERE stort = wa_equip-stort AND
*          msgrp = wa_equip-premise.
*
*          CLEAR : lv_count.
*          lv_count = strlen( wa_iloa_e-tplnr ).
*          IF ( lv_count >= 8 )  AND ( lv_count =< 16 ).
**take only level 3 th Level Floc
*            READ TABLE ta_fl_level INTO wa_fl_level WITH KEY
*                             zcat_code = wa_equip-zcat_code
*                             zgrp_code = wa_equip-zgrp_code.
*            IF sy-subrc IS INITIAL.
**stort is 3level has 8 characters
*              lv_3rdlevel = wa_iloa_e-tplnr.
*              CONCATENATE lv_3rdlevel '-' wa_fl_level-zlevel INTO lv_floc RESPECTING BLANKS." 4th Level
*              wa_level-low = lv_floc.
*              wa_level-sign = 'I'.
*              wa_level-option = 'EQ'.
*              APPEND wa_level TO s_level.
*            ENDIF.
*          ENDIF.
*          CLEAR : wa_fl_level,
*                wa_iloa_e.
*        ENDLOOP.
*
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  DELETE ADJACENT DUPLICATES FROM s_level COMPARING low.
*
** from the above code wil get Level 4 flocation
**pass level 4 floc and EQART as Tech_obty in iflot to Superior floc
**fetch the all 5th level Floc(there will be multiple Flocation at level 5)
*  IF s_level[] IS NOT INITIAL.
*
**    SELECT tplnr tplma eqart FROM iflot INTO TABLE ta_iflot
**       FOR ALL ENTRIES IN ta_fl_level
**      WHERE tplma IN s_level AND
**            eqart = ta_fl_level-ztech_obty.
**    IF sy-subrc IS INITIAL.
**      SORT ta_iflot BY eqart.
**    ENDIF.
*
*
*    SELECT tplnr tplma eqart FROM iflot INTO TABLE ta_iflot
*          FOR ALL ENTRIES IN ta_fl_level
*         WHERE eqart = ta_fl_level-ztech_obty.
*
*    IF sy-subrc IS INITIAL.
*      LOOP AT ta_iflot ASSIGNING <fs_iflot>.
*        READ TABLE s_level TRANSPORTING NO FIELDS WITH KEY low = <fs_iflot>-tplma.
*        IF sy-subrc IS NOT INITIAL.
*          <fs_iflot>-del = 'X'.
*        ENDIF.
*      ENDLOOP.
*      DELETE ta_iflot WHERE del = 'X'.
*      SORT ta_iflot BY eqart.
*    ENDIF.
*
*  ENDIF.
*
*ENDFORM.                    " GET_DATA
**&---------------------------------------------------------------------*
**&      Form  READ_DATA
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      <--P_LV_FLOC  text
**----------------------------------------------------------------------*
*FORM read_data  TABLES   tbl_return_tab STRUCTURE zpms_equip_ret
*                       CHANGING lv_err_flg.
*
**fetch maintainence Plant and location based on ROOM
*  READ TABLE ta_t499s INTO wa_t499s WITH KEY
*  stand = wa_equip-stort BINARY SEARCH.
*  IF sy-subrc IS INITIAL.
*    wa_general-maintloc = wa_equip-stort.
*    wa_general-maintplant = wa_t499s-werks.
*  ENDIF.
*
**Fetch functional location, based on (premises , location, equipmet
**category code, group code)
**override the Plant ,use the Floca plant
*  READ TABLE ta_iloa_e  INTO wa_iloa_e WITH KEY
*                            stort = wa_equip-stort
*                            msgrp = wa_equip-premise
*                            BINARY SEARCH.
*  IF sy-subrc IS INITIAL.
**          lv_floc  = wa_iloa_e-tplnr.  "functional location
*    wa_general-maintplant = wa_iloa_e-swerk.
*  ELSE.
*    lv_err_flg = 'X'.
*    wa_ret_e-status = '2'.
*    CONCATENATE 'Error:'  'Location-' wa_equip-stort 'or Premises-' wa_equip-premise  'is invalid'  INTO lv_text.
*    wa_ret_e-message = lv_text.
*    APPEND wa_ret_e TO tbl_return_tab.
*    CLEAR : wa_ret_e,
*            lv_text.
*  ENDIF.
*
*ENDFORM.                    " READ_DATA
**----------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**&      Form  CREATE_EQUIPMENT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_TBL_RETURN_TAB  text
**----------------------------------------------------------------------*
*FORM create_equipment  TABLES   tbl_return_tab STRUCTURE zpms_equip_ret
*                       CHANGING lv_err_flg.
*  CLEAR :lv_err_flg.
*  lv_external_no = wa_equip-equnr.
*  wa_equip-datsl = sy-datum.
*  wa_specific-equicatgry = wa_equip-eqtyp. " B and C
*  wa_general-descript = wa_equip-eqktx .
*  wa_general-objecttype = wa_equip-eqart.
*  wa_general-acqdate = wa_equip-ansdt.
*  wa_general-manfacture = wa_equip-herst.
*  wa_general-manmodel = wa_equip-typbz.
*  wa_general-manserno = wa_equip-serge.
*  wa_general-manparno = wa_equip-mapar.
*  wa_general-abcindic =  'C'.".wa_equip-ABCKZ. "C
*  wa_specific-techid = wa_equip-tidnr .
*  wa_general-maintloc = wa_equip-stort.
*  CLEAR : wa_fl_level.
*  READ TABLE ta_fl_level INTO wa_fl_level WITH KEY
*                           zcat_code = wa_equip-zcat_code
*                           zgrp_code = wa_equip-zgrp_code.
*  IF sy-subrc IS INITIAL.
*    wa_general-objecttype = wa_fl_level-ztech_obty.
*  ENDIF.
*
*  CALL FUNCTION 'BAPI_EQUI_CREATE'
*    EXPORTING
*      external_number   = lv_external_no
*      data_general      = wa_general
*      data_specific     = wa_specific
*    IMPORTING
*      equipment         = lv_equip_created
**     DATA_GENERAL_EXP  = DATA_GENERAL_EXP
**     DATA_SPECIFIC_EXP = DATA_SPECIFIC_EXP
**     DATA_FLEET_EXP    = DATA_FLEET_EXP
*      return            = wa_return.
*
*  IF wa_return IS INITIAL.
*    IF lv_equip_created IS NOT INITIAL.
*      wa_ret_e-equipment = lv_equip_created.
*      wa_ret_e-status = '1'.
*      CONCATENATE 'Success-' 'Equipment created :'  lv_equip_created
*      INTO lv_text.
*      wa_ret_e-message = lv_text.
*      CLEAR : lv_text.
*      APPEND wa_ret_e TO tbl_return_tab.
*      CLEAR : wa_ret_e.
*
*      PERFORM commit.
*
**Add MD_MT class to the created Equipment
*      PERFORM create_classification USING lv_equip_created.
*    ENDIF.
*
*  ELSE.
**If error found and equipment not created, set the flag
** continue to next record, fill the return table
*    lv_err_flg = 'X'.
*    CLEAR : lv_text.
*    wa_ret_e-equipment = lv_external_no.
*    wa_ret_e-status = '2'.
*    CONCATENATE 'Error-' wa_return-message INTO lv_text.
*    wa_ret_e-message = lv_text.
*    APPEND wa_ret_e TO tbl_return_tab.
*    CLEAR : wa_ret_e,
*            lv_text.
*
*  ENDIF.
*
*ENDFORM.                    " CREATE_EQUIPMENT
**&---------------------------------------------------------------------*
**&      Form  DISMANTLE_EQUIP
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_LV_FLOC  text
**      -->P_TBL_RETURN_TAB  text
**----------------------------------------------------------------------*
*FORM dismantle_equip TABLES tbl_return_tab STRUCTURE zpms_equip_ret
*                      USING  lv_floc
*                             CHANGING lv_err_flg.
*
*  CALL FUNCTION 'ALM_ME_FUNCLOC_INSTALLED_EQUI'
*    EXPORTING
*      i_funcloc            = lv_floc
*                                                                                                                                                                                                            "'DAWN-PLTE-SFCT-VALV-P1101' "functional location
*    TABLES
*      t_funcloc_inst_equis = ta_installed_equi.
*
*  LOOP AT ta_installed_equi INTO wa_installed_equi.
*    CLEAR: ts_dis_ret,
*           wa_ret_e.
*
*    IF wa_installed_equi-equnr = lv_equip_created.
*      wa_ret_e-equipment = wa_installed_equi-equnr.
*      wa_ret_e-status = '1'.
*      CONCATENATE 'Success-'  'Equipment:' wa_installed_equi-equnr  ' already installed on FLoc:' wa_installed_equi-tplnr  INTO lv_text.
*      wa_ret_e-message = lv_text.
*      APPEND wa_ret_e TO tbl_return_tab.
*      CLEAR : lv_text , wa_ret_e.
*      lv_err_flg = 'X'.
*      RETURN.
*
*    ELSE.
**if exist any equipment , Dismantle the equipment from that location
*      CALL FUNCTION 'BAPI_EQMT_DISMANTLEFL'
*        EXPORTING
*          equipment     = wa_installed_equi-equnr
*          funcloc       = wa_installed_equi-tplnr
**         date          = sy-datlo
**         time          = sy-timlo
*        IMPORTING
**         EQUIMASTER    = EQUIMASTER
**         EQUITIME      = EQUITIME
**         EQUITEXT      = EQUITEXT
**         EQUILOCATION  = EQUILOCATION
**         EQUISALES     = EQUISALES
**         EQUIHIERARCHY = EQUIHIERARCHY
*          return        = ts_dis_ret.
*
*      IF ts_dis_ret-type <> 'E'.
*        wa_ret_e-equipment = wa_installed_equi-equnr.
*        wa_ret_e-status = '1'.
*        CONCATENATE 'Success-' wa_installed_equi-equnr  ': Equipment Dismantle from FLoc :' wa_installed_equi-tplnr  INTO lv_text.
*        wa_ret_e-message = lv_text.
*        APPEND wa_ret_e TO tbl_return_tab.
*        CLEAR : lv_text , wa_ret_e.
**        PERFORM commit.
*      ELSE.
**          lv_del_flag = 'X'.
**            error set flag if any equipment is not dismantle
*        RETURN." come out of the loop, no further processing
*      ENDIF.
*      CLEAR : wa_installed_equi.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.                    " DISMANTLE_EQUIP
**&---------------------------------------------------------------------*
**&      Form  INSTALL_EQUIP
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_TBL_RETURN_TAB  text
**----------------------------------------------------------------------*
*FORM install_equip  TABLES tbl_return_tab STRUCTURE zpms_equip_ret.
*
*  FIELD-SYMBOLS : <fs_iflot> TYPE ty_iflot.
*  CLEAR : wa_iflot,
*          wa_fl_level.
*
*  SORT ta_iflot BY tplnr ASCENDING.
**read the current Equipment details
*  READ TABLE ta_fl_level INTO wa_fl_level WITH KEY
*                      zcat_code = wa_equip-zcat_code
*                      zgrp_code = wa_equip-zgrp_code .
*  IF sy-subrc IS INITIAL.
**fetch the no of flocation at level 5(will be more than one)
**Assuming at this point , we got 3 (FL1, FL2, FL3) for current techobj
** So the Belwo FM will give one by one- if FL1 has any equipment Installed or not
** and If not then Insatlled the current Equipmnt on curnt FLo
*
*    LOOP AT ta_iflot ASSIGNING <fs_iflot> WHERE eqart = wa_fl_level-ztech_obty.
*      REFRESH : ta_installed_equi[].
*
*      CALL FUNCTION 'ALM_ME_FUNCLOC_INSTALLED_EQUI'
*        EXPORTING
*          i_funcloc            = <fs_iflot>-tplnr
*                                                                                                                                                                                                            "'DAWN-PLTE-SFCT-VALV-P1101' "functional location
*        TABLES
*          t_funcloc_inst_equis = ta_installed_equi.
*
*      IF ta_installed_equi[] IS INITIAL.
*        "If table is blank ,Till now no Equipment installed on Corrent Floc
*        "Directly install the Equipment
*        CALL FUNCTION 'BAPI_EQMT_INSTALLFL'
*          EXPORTING
*            equipment = lv_equip_created
*            funcloc   = <fs_iflot>-tplnr "'DAWN-PLTE-SFCT-VALV-P1101'
**           POSEQUI   = POSEQUI
**           date      = sy-datlo
**           time      = sy-timlo
*          IMPORTING
*            return    = ts_dis_ret.
*
*        IF ts_dis_ret-type <> 'E'.
*          <fs_iflot>-equip_install = 'X'."set flag for current Equipment and current Floc
*          wa_ret_e-equipment = lv_equip_created.
*          wa_ret_e-status = '1'.
*          CONCATENATE 'Success-' ' Equipment No.:' lv_equip_created ' installed on the Tech Objtype and FLocation :' wa_fl_level-ztech_obty '/' <fs_iflot>-tplnr INTO  lv_text.
*          wa_ret_e-message = lv_text.
*          APPEND wa_ret_e TO tbl_return_tab.
*          CLEAR : lv_text, wa_ret_e.
*          PERFORM commit.
*          EXIT.
*        ELSE.
*          wa_ret_e-equipment = lv_equip_created.
*          wa_ret_e-status = '2'.
*          CONCATENATE 'Error-' 'Equipment No.:' lv_equip_created ' not installed :' ts_dis_ret-message INTO  lv_text.
*          wa_ret_e-message = lv_text.
*          APPEND wa_ret_e TO tbl_return_tab.
*          CLEAR : lv_text, wa_ret_e.
*        ENDIF.
*      ENDIF.
*
*    ENDLOOP."final loop
*  ENDIF.
*
*ENDFORM.                    " INSTALL_EQUIP
**&---------------------------------------------------------------------*
**&      Form  CHANGE_EQUIP
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_TBL_RETURN_TAB  text
**----------------------------------------------------------------------*
*FORM change_equip  TABLES tbl_return_tab STRUCTURE zpms_equip_ret
*                   CHANGING lv_err_flg.
*  "Insert correct name for <...>.
*
*  lv_external_no = wa_equip-equnr.
*  wa_specific-equicatgry = wa_equip-eqtyp. " B and C
*  wa_general-descript = wa_equip-eqktx .
*  wa_general-objecttype = wa_equip-eqart.
*  wa_general-acqdate = wa_equip-ansdt.
*  wa_general-manfacture = wa_equip-herst.
*  wa_general-manmodel = wa_equip-typbz.
*  wa_general-manserno = wa_equip-serge.
*  wa_general-manparno = wa_equip-mapar.
*  wa_general-abcindic =  'C'.".wa_equip-ABCKZ. "C
*  wa_specific-techid = wa_equip-tidnr .
*  wa_general-maintloc = wa_equip-stort.
*  CLEAR : wa_fl_level.
*  READ TABLE ta_fl_level INTO wa_fl_level WITH KEY
*                            zcat_code = wa_equip-zcat_code
*                            zgrp_code = wa_equip-zgrp_code.
*  IF sy-subrc IS INITIAL.
*    wa_general-objecttype = wa_fl_level-ztech_obty.
*  ENDIF.
**update
*  wa_specificx-equicatgry = 'X'."wa_equip-EQTYP. " B
*  wa_generalx-descript = 'X'.
*  wa_generalx-objecttype = 'X'.
*  wa_generalx-acqdate = 'X'.
*  wa_generalx-manfacture = 'X'.
*  wa_generalx-manmodel = 'X'.
*  wa_generalx-manserno = 'X'.
*  wa_generalx-manparno = 'X'.
*  wa_generalx-abcindic =  'X'.".wa_equip-ABCKZ. "C
*  wa_specificx-techid = 'X' .
*  wa_generalx-maintloc = 'X'.
*  wa_generalx-maintplant = 'X'.
*  wa_generalx-objecttype = 'X'.
*
*  CALL FUNCTION 'BAPI_EQUI_CHANGE'
*    EXPORTING
*      equipment         = lv_external_no
*      data_general      = wa_general
*      data_generalx     = wa_generalx
*      data_specific     = wa_specific
*      data_specificx    = wa_specificx
**     DATA_FLEET        = DATA_FLEET
**     DATA_FLEETX       = DATA_FLEETX
**     VALID_DATE        = SY-DATUM
**     VALID_TIME        = SY-UZEIT
*    IMPORTING
**     DATA_GENERAL_EXP  = DATA_GENERAL_EXP
**     DATA_SPECIFIC_EXP = DATA_SPECIFIC_EXP
**     DATA_FLEET_EXP    = DATA_FLEET_EXP
*      return            = wa_return.
*  IF wa_return IS INITIAL.
*    lv_equip_created = lv_external_no.
*    IF lv_equip_created IS NOT INITIAL.
*      wa_ret_e-equipment = lv_equip_created.
*      wa_ret_e-status = '1'.
*      CONCATENATE 'Success-' 'Equipment changed:' lv_equip_created
*      INTO lv_text.
*      wa_ret_e-message = lv_text.
*      CLEAR : lv_text.
*      APPEND wa_ret_e TO tbl_return_tab.
*      CLEAR : wa_ret_e.
*
***Add MD_MT class to the created Equipment
*      PERFORM create_classification USING lv_equip_created.
**commit after both equipment change and classification is done
*      PERFORM commit.
*    ENDIF.
*  ELSE.
*    lv_err_flg = 'X'.
*    CLEAR : lv_text.
*    wa_ret_e-equipment = lv_external_no.
*    wa_ret_e-status = '2'.
*    CONCATENATE 'Error-' wa_return-message INTO lv_text.
*    wa_ret_e-message = lv_text.
*    APPEND wa_ret_e TO tbl_return_tab.
*    CLEAR : wa_ret_e,
*            lv_text.
*
*  ENDIF.
*
*ENDFORM.                    " CHANGE_EQUIP
**&---------------------------------------------------------------------*
**&      Form  EQUIP_ON_CURRENT_FLOC
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM equip_on_current_floc TABLES imp_equip STRUCTURE zpms_equip.
*  CLEAR : wa_equip.
*  CALL FUNCTION 'ALM_ME_FUNCLOC_INSTALLED_EQUI'
*    EXPORTING
*      i_funcloc            = wa_iflot-tplnr
*                                                                                                                                                                                                            "'DAWN-PLTE-SFCT-VALV-P1101' "functional location
*    TABLES
*      t_funcloc_inst_equis = ta_installed_equi.
*
*  LOOP AT ta_installed_equi INTO wa_installed_equi.
*
**Dismanmtle the Equipment which not matches with Banner equipment
*    READ TABLE imp_equip INTO wa_equip WITH KEY equnr = wa_installed_equi-equnr.
*    IF sy-subrc IS NOT INITIAL.
*
*      CALL FUNCTION 'BAPI_EQMT_DISMANTLEFL'
*        EXPORTING
*          equipment = wa_installed_equi-equnr
*          funcloc   = wa_installed_equi-tplnr
**         date      = sy-datlo
**         time      = sy-timlo
*        IMPORTING
*          return    = ts_dis_ret.
*
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.                    " EQUIP_ON_CURRENT_FLOC
