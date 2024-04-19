FUNCTION zpm_create_wo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IMP_HEADER) TYPE  ZPMTT_HEADER
*"     VALUE(IMP_OPERATION) TYPE  ZPMTT_OPERATION OPTIONAL
*"  TABLES
*"      TBL_RETURN_TAB STRUCTURE  ZPMS_RETURN
*"----------------------------------------------------------------------

  REFRESH : tbl_return_tab[],
            ta_aufk[],
            ta_banner_type[],
            ta_iloa[].
  CLEAR : wa_ret,
          wa_aufk,
          wa_iloa.
  CLEAR : lv_created_order.
*--Check the  order created with banner order no
  IF imp_header[] IS NOT INITIAL.
*fectch the mainatinence type based on Banner work order
    SELECT * FROM zpmt_banner_type INTO TABLE ta_banner_type FOR ALL ENTRIES IN imp_header
      WHERE zzpmbworktype = imp_header-zzpmbworktype.
    IF sy-subrc IS INITIAL.
      SORT ta_banner_type BY zzpmbworktype.
    ENDIF.

    SELECT aufnr
           zzpmbworkord
           zzpmbworktype
           zzpmbscheddate
           zzpmbstatus FROM aufk INTO TABLE ta_aufk  FOR ALL ENTRIES IN imp_header
                                              WHERE zzpmbworkord = imp_header-zzpmbworkord.
    IF sy-subrc IS INITIAL.
      SORT ta_aufk BY zzpmbworkord.
    ENDIF.

*---fetch the function location based on premise and station id.
    SELECT tplnr stort msgrp FROM iflo INTO TABLE ta_iloa FOR ALL ENTRIES IN imp_header
                                          WHERE eqart IN ('ST_RG', 'ST_TR', 'ST_MD') AND
                                                stort EQ imp_header-station_id AND
                                                msgrp EQ imp_header-premise.
    IF sy-subrc IS INITIAL .
      SORT ta_iloa BY stort msgrp.
    ENDIF.

  ENDIF.
  SORT imp_header BY zzpmbworkord.
  LOOP AT imp_header INTO wa_order_header.
    REFRESH :
          ta_methods[],
          ta_header[],
          ta_objectlist[],
          ta_operation[],
          return[],
          et_numbers[].
    CLEAR : lv_created_order.
    CLEAR : wa_aufk.
    READ TABLE ta_aufk INTO wa_aufk WITH KEY zzpmbworkord = wa_order_header-zzpmbworkord BINARY SEARCH.
    IF ( wa_aufk-aufnr IS INITIAL ).
*      * Validation for Mandatory Fields
      IF ( wa_order_header-zzpmbworkord IS INITIAL ) OR
         ( wa_order_header-order_type IS INITIAL ) OR
*         ( wa_order_header-start_date IS INITIAL ) OR
*         ( wa_order_header-finish_date IS INITIAL ) OR
         ( wa_order_header-station_id IS INITIAL ) OR
         ( wa_order_header-premise IS INITIAL ) .

        wa_ret-wrk_order_ref = wa_order_header-zzpmbworkord.
        wa_ret-status_code = '2'.
        wa_ret-description = text-007.
        APPEND wa_ret TO tbl_return_tab.
        CLEAR : wa_ret.
        CONTINUE.
      ENDIF.
*-----------------Creation of Order----------------------------------------------

      lv_orderid = '%00000000001'.
      CLEAR wa_methods.

* *Start of code or creating Maintainace order based on notification
      wa_methods-refnumber = '000001'.
      wa_methods-objecttype = 'HEADER'.
      wa_methods-method = 'CREATETONOTIF'.
      wa_methods-objectkey = lv_orderid.
      APPEND wa_methods TO ta_methods. CLEAR wa_methods.
      CLEAR wa_methods.
      wa_methods-refnumber = '000001'.
      wa_methods-objecttype = 'HEADER'.
      wa_methods-method = 'RELEASE'.
      wa_methods-objectkey = lv_orderid.
      APPEND wa_methods TO ta_methods. CLEAR wa_methods.

      CLEAR wa_methods.
      wa_methods-refnumber = '000001'.
      wa_methods-objecttype = ''.
      wa_methods-method = 'SAVE'.
      wa_methods-objectkey = lv_orderid.
      APPEND wa_methods TO ta_methods. CLEAR wa_methods.

      CLEAR wa_header.

*chk data validity and format
      CLEAR lv_err_flg.

      wa_header-orderid = lv_orderid.
      wa_header-order_type = wa_order_header-order_type. "'PM10'.   " order type
      wa_header-start_date = sy-datum.
      wa_header-finish_date = sy-datum.
      wa_header-short_text  = wa_order_header-order_desc. "short text value
*      wa_header-notif_no = lv_orderid."notification no
*      wa_header-notif_type = 'M2'.
* * fill activity type
      READ TABLE ta_banner_type INTO wa_banner_type  WITH KEY
                  zzpmbworktype = wa_order_header-zzpmbworktype BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wa_header-pmacttype = wa_banner_type-zpmacttype.
      ENDIF.

      READ TABLE ta_iloa  INTO wa_iloa WITH KEY
                                stort = wa_order_header-station_id
                               msgrp = wa_order_header-premise BINARY SEARCH .
      IF sy-subrc IS INITIAL.
        CLEAR : lv_3tplnr.

*-- Get the H3 functional location for the provided h5
        CALL METHOD zpm_cl_funcloc_util=>get_hierarchy_from_h5
          EXPORTING
            iv_tplnr     = wa_iloa-tplnr
            iv_tplkz     = 'U0002'
            iv_hierarchy = '3'  " Hierarchy 3
          IMPORTING
            ev_tplnr_h3  = lv_3tplnr.
        IF lv_3tplnr IS NOT INITIAL.
          wa_header-funct_loc  = lv_3tplnr.  "functional location
          wa_objectlist-counter = '1'.
          wa_objectlist-funct_loc =  lv_3tplnr.
          wa_objectlist-sortfield = 'X'.
          wa_objectlist-processing_ind = 'X'.
          wa_objectlist-notif_no = lv_orderid.
          APPEND wa_objectlist TO ta_objectlist.
        ELSE.
          wa_ret-wrk_order_ref = wa_order_header-zzpmbworkord.
          wa_ret-status_code = '3'.
          CONCATENATE '3rd Level Functional location not found' lv_error INTO lv_error .
          wa_ret-description = lv_error.
          APPEND wa_ret TO tbl_return_tab.
          CLEAR : wa_ret.
          CONTINUE.
        ENDIF.

      ELSE.
        wa_ret-wrk_order_ref = wa_order_header-zzpmbworkord.
        wa_ret-status_code = '3'.
        CONCATENATE text-003 wa_order_header-station_id text-009 wa_order_header-premise INTO lv_error .
        wa_ret-description = lv_error.
        APPEND wa_ret TO tbl_return_tab.
        CLEAR : wa_ret.
        CONTINUE.
      ENDIF.
      CLEAR lv_error.
      APPEND wa_header TO ta_header.
      CLEAR wa_header.

**operation entry
      CLEAR : lv_vornr .
      LOOP AT imp_operation INTO wa_order_oper
        WHERE zzpmbworkord = wa_order_header-zzpmbworkord.
        lv_vornr = lv_vornr + 10.
***--------- Operation Mapping
        wa_methods-refnumber = '000001'. " Operation-1
        wa_methods-objecttype = 'OPERATION'.
        wa_methods-method = 'CREATE'.
        CONCATENATE lv_orderid lv_vornr  INTO wa_methods-objectkey.
        APPEND wa_methods TO ta_methods. CLEAR wa_methods.
        wa_operation-activity = lv_vornr.
        wa_operation-control_key = 'PM01'.
        wa_operation-acttype = wa_order_oper-acttype ."'0010'.
        wa_operation-description = wa_order_oper-description.
        APPEND wa_operation TO ta_operation.
        CLEAR : wa_operation,
               wa_order_oper.
      ENDLOOP.

      DATA : ta_userstatus TYPE STANDARD TABLE OF bapi_alm_order_usrstat,
            wa_userstatus TYPE bapi_alm_order_usrstat.

      wa_userstatus-user_st_text = 'REL'.
      wa_userstatus-langu = 'E'.
* wa_userstatus-LANGU_ISO
* wa_userstatus-INACTIVE
      APPEND   wa_userstatus TO ta_userstatus.

      CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
        DESTINATION 'NONE'
        TABLES
          it_methods    = ta_methods
          it_header     = ta_header
          it_header_up  = ta_header_up
*         it_userstatus = ta_userstatus
          it_operation  = ta_operation
          it_objectlist = ta_objectlist
*         extension_in  = ta_ext
          return        = return
          et_numbers    = et_numbers.

      READ TABLE return INTO wa_return WITH KEY type = 'E'.
      IF sy-subrc IS INITIAL.
        LOOP AT return INTO wa_return WHERE type = 'E'.
          wa_ret-wrk_order_ref = wa_order_header-zzpmbworkord.
          wa_ret-status_code = '6'.
          CONCATENATE text-006 wa_return-message INTO wa_ret-description.

          APPEND wa_ret TO tbl_return_tab.
        ENDLOOP.

      ELSE.
        IF et_numbers[] IS NOT INITIAL.
          LOOP AT et_numbers INTO wa_numbers.
            wa_ret-wrk_order_ref = wa_order_header-zzpmbworkord.
            lv_created_order = wa_numbers-aufnr_new.
            wa_ret-wrk_order_created = lv_created_order.
            wa_ret-status_code = '1'." success
            wa_ret-description = text-002.
            APPEND wa_ret TO tbl_return_tab.
            CLEAR : wa_ret.
          ENDLOOP.
        ENDIF.
        REFRESH : return[].
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          DESTINATION 'NONE'
          EXPORTING
            wait = 'X'.

        CALL FUNCTION 'RFC_CONNECTION_CLOSE'
          EXPORTING
            DESTINATION          = 'NONE'
          EXCEPTIONS
            destination_not_open = 1
          OTHERS              = 2.

        PERFORM update_zfields .

      ENDIF.
    ELSE.
*-----Change Work Order-------------------------------------------------------------

      CONDENSE wa_order_header-zzpmbworktype.
      CONDENSE wa_order_header-zzpmbstatus .
      IF  wa_aufk-zzpmbworktype <> wa_order_header-zzpmbworktype OR
      wa_aufk-zzpmbscheddate <> wa_order_header-zzpmbscheddate OR
      wa_aufk-zzpmbstatus <> wa_order_header-zzpmbstatus.

        IF wa_order_header-zzpmbworktype IS NOT INITIAL OR
          wa_order_header-zzpmbscheddate IS NOT INITIAL OR
          wa_order_header-zzpmbstatus IS NOT INITIAL.

          UPDATE aufk SET
                                       zzpmbworktype = wa_order_header-zzpmbworktype
                                       zzpmbscheddate = wa_order_header-zzpmbscheddate
                                       zzpmbstatus = wa_order_header-zzpmbstatus
                                  WHERE aufnr = wa_aufk-aufnr.

          IF sy-subrc IS INITIAL.
            wa_ret-wrk_order_ref = wa_order_header-zzpmbworkord.
            lv_created_order = wa_aufk-aufnr.
            wa_ret-wrk_order_created = lv_created_order.
            wa_ret-status_code = '1'.
            wa_ret-description = text-001.
            APPEND wa_ret TO tbl_return_tab.
            CLEAR : wa_ret.
          ELSE.
            wa_ret-wrk_order_ref = wa_order_header-zzpmbworkord.
            lv_created_order = wa_aufk-aufnr.
            wa_ret-wrk_order_created = lv_created_order.
            wa_ret-status_code = '5'.
            wa_ret-description = text-004.
            APPEND wa_ret TO tbl_return_tab.
            CLEAR : wa_ret.
          ENDIF.

        ELSE.
          wa_ret-wrk_order_ref = wa_order_header-zzpmbworkord.
          lv_created_order = wa_aufk-aufnr.
          wa_ret-wrk_order_created = lv_created_order.
          wa_ret-status_code = '6'.
          wa_ret-description = text-008.
          APPEND wa_ret TO tbl_return_tab.
          CLEAR : wa_ret.
        ENDIF.
      ELSE." no chnges
        wa_ret-wrk_order_ref = wa_order_header-zzpmbworkord.
        lv_created_order = wa_aufk-aufnr.
        wa_ret-wrk_order_created = lv_created_order.
        wa_ret-status_code = '1'.
        wa_ret-description = 'No changes in Work Order'.
        APPEND wa_ret TO tbl_return_tab.
        CLEAR : wa_ret.
      ENDIF.
    ENDIF.
    CLEAR : wa_order_header.
  ENDLOOP.


ENDFUNCTION.
