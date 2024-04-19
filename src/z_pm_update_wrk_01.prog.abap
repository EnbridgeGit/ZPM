*&---------------------------------------------------------------------*
*&  Include           Z_PM_UPDATE_WRK_01
*&---------------------------------------------------------------------*
TYPE-POOLS : icon.
FIELD-SYMBOLS : <fs_objects> TYPE gstype_object_tab,
               <fs_obj> TYPE gstype_object_tab,
               <fs_obj3> TYPE gstype_object_tab,
               <fs_obj1> TYPE gstype_object_tab.
DATA : lv_arbpl TYPE arbpl,
       lv_exit_okcode TYPE char1,
       lv_index TYPE sy-tabix,
       lv_aufnr TYPE aufnr,
       lv_order TYPE aufnr,
       lv_dis TYPE c,
       lv_objnr TYPE jsto-objnr,
       lv_status TYPE jest-stat,
       lv_disp_flag TYPE c,
       lv_werks TYPE werks.
TYPES : BEGIN OF ty_error,
          status TYPE char4,
          aufnr TYPE aufnr,
          type TYPE bapi_mtype,
          error_log TYPE string,
         END OF ty_error.
*Internal tables declaration---------------
DATA : ta_methods TYPE STANDARD TABLE OF bapi_alm_order_method,
       ta_header TYPE STANDARD TABLE OF bapi_alm_order_headers_i,
       ta_header_up TYPE STANDARD TABLE OF bapi_alm_order_headers_up,
       ta_operation TYPE STANDARD TABLE OF bapi_alm_order_operation,
       ta_operation_up TYPE STANDARD TABLE OF bapi_alm_order_operation_up,
       ta_numbers TYPE STANDARD TABLE OF bapi_alm_numbers,
       ta_return TYPE STANDARD TABLE OF bapiret2,
       ta_object TYPE STANDARD TABLE OF gstype_object_tab,
       ta_object_final TYPE STANDARD TABLE OF gstype_object_tab,
       ta_error TYPE STANDARD TABLE OF ty_error,
       ta_fcat TYPE slis_t_fieldcat_alv.
*work area
DATA : wa_methods TYPE bapi_alm_order_method,
       wa_header TYPE  bapi_alm_order_headers_i,
       wa_header_up TYPE bapi_alm_order_headers_up,
       wa_operation TYPE bapi_alm_order_operation,
       wa_operation_up TYPE  bapi_alm_order_operation_up,
       wa_numbers TYPE  bapi_alm_numbers,
       wa_return TYPE bapiret2,
       st_layout TYPE slis_layout_alv,
*       wa_object TYPE gstype_object_tab,
       wa_error TYPE ty_error,
       wa_fcat TYPE slis_fieldcat_alv.
CLEAR : lv_exit_okcode,
lv_status.
INCLUDE z_update_wrk_desc_01.
*Get value of lv_exit_okcode from screen 1000
*Purpose is to have okcode(OK, CANCEL) value from screen 1000
GET PARAMETER ID 'EXT' FIELD lv_exit_okcode.

*in case of CANCEL ok_code  value is set to 'X' , it should not proceed
IF lv_exit_okcode <> 'X'." If exit is set to X , no process
  REFRESH: ta_object_final[],
           ta_object.

  CASE p_ucomm.
    WHEN '+CUS1'.

      ta_object_final = object_tab[].
      SORT ta_object_final BY selected.
*fetch only selected records on ta_object
*thid table ta_object_final is used for line item
      DELETE ta_object_final WHERE selected <> 'X'.
      SORT ta_object_final BY aufnr.
      IF sy-subrc IS INITIAL.
        ta_object[] = ta_object_final[].
      ELSE.
        ta_object[] = ta_object_final[].
      ENDIF.

      SORT ta_object BY aufnr.
*fetch unique aufnr records in ta_object, it is used for header
      DELETE ADJACENT DUPLICATES FROM ta_object COMPARING aufnr .
*work center and werks is retrieve from Screen 1000
      GET PARAMETER ID 'WRK' FIELD lv_arbpl.
      GET PARAMETER ID 'PLT' FIELD lv_werks.
      GET PARAMETER ID 'DIS' FIELD lv_dis.

*chk if status is set correct for Dispatch selction
      IF lv_dis = 'X'.
        LOOP AT ta_object ASSIGNING <fs_obj1>.
          IF ( <fs_obj1>-sttxt NS 'REL' ).
            lv_disp_flag = 'X'.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF lv_arbpl IS INITIAL AND lv_werks IS INITIAL.
        IF lv_disp_flag = 'X'.
          MESSAGE e003(zpm).
        ENDIF.
      ENDIF.


*check for Work center and Plant update
      IF lv_arbpl IS NOT INITIAL AND lv_werks IS NOT INITIAL.
*this loop will execute once for every aufnr
        LOOP AT ta_object ASSIGNING <fs_obj1>.

          CLEAR : wa_error,wa_return.
          REFRESH : ta_return[],
                   ta_methods[],
                   ta_header[],
                   ta_header_up[],
                   ta_operation[].
          lv_aufnr = <fs_obj1>-aufnr.


*-----prepare header data for every AUFNR to pass in bapi

          wa_methods-refnumber = '000001'.
          wa_methods-objecttype = ''.
          wa_methods-method = 'SAVE'.
          wa_methods-objectkey = lv_aufnr.
          APPEND wa_methods TO ta_methods.
          CLEAR wa_methods.

          wa_header-orderid = lv_aufnr.
          APPEND wa_header TO ta_header.
          CLEAR wa_header.

          wa_header_up-orderid = lv_aufnr.
          APPEND wa_header_up TO ta_header_up.
          CLEAR : wa_header_up,
                  lv_order.

*------loop for every line item selected
          READ TABLE ta_object_final WITH KEY aufnr = <fs_obj1>-aufnr BINARY SEARCH
             TRANSPORTING NO FIELDS.
          IF sy-subrc IS INITIAL.
            lv_index = sy-tabix.
          ENDIF.
          LOOP AT ta_object_final ASSIGNING <fs_objects>
                         FROM lv_index.
            IF <fs_objects>-aufnr NE <fs_obj1>-aufnr.
              EXIT.
            ENDIF.
*------Prepare Method table for operation
            wa_methods-refnumber = '000001'.
            wa_methods-objecttype = 'OPERATION'.
            wa_methods-method = 'CHANGE'.
            CONCATENATE lv_aufnr <fs_objects>-vornr INTO wa_methods-objectkey.
            APPEND wa_methods TO ta_methods.
            CLEAR wa_methods.

*---------prepare line items for every Aufnr
            CLEAR : wa_operation.
            wa_operation-activity  = <fs_objects>-vornr.
            wa_operation-work_cntr = lv_arbpl.
            wa_operation-plant     = lv_werks.
            APPEND wa_operation TO ta_operation.
            CLEAR : wa_operation.
          ENDLOOP.

*----------oPeration line field update
          wa_operation_up-work_cntr = 'X'.
          wa_operation_up-plant     = 'X'.
          APPEND wa_operation_up TO ta_operation_up.
          CLEAR : wa_operation_up.

          CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
            TABLES
              it_methods      = ta_methods
              it_header       = ta_header
              it_header_up    = ta_header_up
              it_operation    = ta_operation
              it_operation_up = ta_operation_up
              return          = ta_return
              et_numbers      = ta_numbers.

          LOOP AT ta_return INTO wa_return WHERE type EQ 'E' .
*          ---------prepare error records
            wa_error-status = icon_red_light.
            wa_error-aufnr = <fs_obj1>-aufnr.
            wa_error-type = wa_return-type.
            wa_error-error_log = wa_return-message.
            APPEND wa_error TO ta_error.
          ENDLOOP.
          REFRESH ta_return[].

*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*            EXPORTING
*              wait   = 'X'
*            IMPORTING
*              return = wa_return.
*
**---------Prepare Error Records
*          IF  wa_return-type = 'E'.
*            wa_error-status = icon_red_light.
*            wa_error-aufnr = <fs_obj1>-aufnr.
*            wa_error-type  = wa_return-type.
*            wa_error-error_log = wa_return-message.
*            APPEND wa_error TO ta_error.
*          ENDIF.

          IF ta_error[] IS INITIAL.
*------------------Update OBJECT_TAB with work center and plant
            LOOP AT ta_operation INTO wa_operation.
              READ TABLE object_tab ASSIGNING <fs_obj> WITH KEY
                    aufnr = <fs_obj1>-aufnr
                    vornr = wa_operation-activity.
              IF sy-subrc IS INITIAL.
*---------------**---Success pass to Output Screen
                <fs_obj>-v_arbpl = lv_arbpl.
                <fs_obj>-iwerk   = lv_werks.
                SELECT SINGLE objid INTO lv_objid FROM crhd WHERE
                            objty = 'A' AND
                            arbpl = lv_arbpl.

                IF lv_objid IS NOT INITIAL.

                  CALL FUNCTION 'CR_WORKSTATION_READ'
                    EXPORTING
                      id        = lv_objid
                    IMPORTING
                      ktext     = lv_ktext
                    EXCEPTIONS
                      not_found = 1
                      OTHERS    = 2.
                  IF sy-subrc = 0.
                    <fs_obj>-zdescription = lv_ktext.
                  ENDIF.
                ENDIF.
**----------------mark success records
                <fs_obj>-pm_selected = sym_check_mark.
              ENDIF.

            ENDLOOP.
          ENDIF.



          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait   = 'X'
            IMPORTING
              return = wa_return.

        ENDLOOP.
      ENDIF. "chk for work center update

** Update the operation user status to DISP
      IF lv_dis = 'X' AND lv_disp_flag NE 'X'."no error and statu need to set
        LOOP AT ta_object_final ASSIGNING <fs_objects>.


          CONCATENATE 'OV' <fs_objects>-aufpl <fs_objects>-aplzl INTO lv_objnr.
          lv_status = 'E0002'. "for display
          CALL FUNCTION 'STATUS_CHANGE_EXTERN'
            EXPORTING
              objnr               = lv_objnr
              user_status         = lv_status
              set_chgkz           = 'X'
            EXCEPTIONS
              object_not_found    = 1
              status_inconsistent = 2
              status_not_allowed  = 3
              OTHERS              = 4.
          IF sy-subrc = 0.
            READ TABLE object_tab ASSIGNING <fs_obj3> WITH KEY
            aufnr = <fs_objects>-aufnr
            vornr = <fs_objects>-vornr.
            <fs_obj3>-v_ustxt = 'DISP'.
            <fs_obj3>-pm_selected = sym_check_mark.

          ENDIF.

        ENDLOOP.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'
          IMPORTING
            return = wa_return.
      ENDIF.

      CLEAR : lv_arbpl, lv_werks, lv_dis,lv_status.
      SET PARAMETER ID 'WRK' FIELD lv_arbpl.
      SET PARAMETER ID 'PLT' FIELD lv_werks.
      SET PARAMETER ID 'DIS' FIELD lv_dis.
*-------------Hit ALV list for Error records
*-------------ALV Display in a POPUP window
      IF ta_error[] IS NOT INITIAL.
        st_layout-colwidth_optimize = 'X'.
        wa_fcat-row_pos   = '1'.
        wa_fcat-col_pos   = '1'.
        wa_fcat-fieldname = 'STATUS'.
        wa_fcat-tabname   = 'TA_ERROR'.
        wa_fcat-icon   = 'X'.
        wa_fcat-seltext_m = 'Work Order No.'.
        APPEND wa_fcat TO ta_fcat.
        CLEAR : wa_fcat.

        wa_fcat-row_pos   = '1'.
        wa_fcat-col_pos   = '2'.
        wa_fcat-fieldname = 'AUFNR'.
        wa_fcat-tabname   = 'TA_ERROR'.
        wa_fcat-seltext_m = 'Work Order No.'.
        APPEND wa_fcat TO ta_fcat.
        CLEAR : wa_fcat.

        wa_fcat-row_pos   = '1'.
        wa_fcat-col_pos   = '3'.
        wa_fcat-fieldname = 'TYPE'.
        wa_fcat-tabname   = 'TA_ERROR'.
        wa_fcat-seltext_m = 'Type'.
        APPEND wa_fcat TO ta_fcat.
        CLEAR : wa_fcat.

        wa_fcat-row_pos   = '1'.
        wa_fcat-col_pos   = '4'.
        wa_fcat-fieldname = 'ERROR_LOG'.
        wa_fcat-tabname   = 'TA_ERROR'.
        wa_fcat-seltext_m = 'Message'.
        APPEND wa_fcat TO ta_fcat.
        CLEAR : wa_fcat.


        CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
            i_callback_program    = g_repid
            i_grid_title          = 'Error Log'
            is_layout             = st_layout
            it_fieldcat           = ta_fcat
            i_screen_start_column = 30
            i_screen_start_line   = 10
            i_screen_end_column   = 100
            i_screen_end_line     = 30
          TABLES
            t_outtab              = ta_error[]
          EXCEPTIONS
            program_error         = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDIF.
