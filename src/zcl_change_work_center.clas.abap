class ZCL_CHANGE_WORK_CENTER definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_CHANGE_WORK_CENTER
*"* do not include other source files here!!!

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_BADI_EAM_LIST_FCODE .
protected section.
*"* protected components of class ZCL_CHANGE_WORK_CENTER
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_CHANGE_WORK_CENTER
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_CHANGE_WORK_CENTER IMPLEMENTATION.


METHOD if_ex_badi_eam_list_fcode~execute_function_code.

  FIELD-SYMBOLS : <fs_selected_data> TYPE any,
                  <fs_select> TYPE any,
                  <fs_status> TYPE any,
                  <fs_plant> TYPE any,
                  <fs_txt> TYPE any.
  DATA : lv_status_op TYPE ivrg_sttxt,
        lv_status_or TYPE ivrg_sttxt,
         lv_werks TYPE werks,
         lv_wc_flag TYPE c,
         lv_disp_flag TYPE c,
         lv_select TYPE c.
  CLEAR : lv_select,
          lv_status_or,
          lv_status_op,
          lv_wc_flag,
          lv_disp_flag.

  IF sy-tcode = 'IW37N' OR sy-tcode = 'ZCPMAIN' OR sy-tcode = 'ZACPPEM' OR sy-tcode = 'ZCPOPS' OR
     sy-tcode = 'ZCMCOR' OR sy-tcode = 'ZCPENGR' OR sy-tcode = 'ZCPUSER' .

    LOOP AT it_selected_objects ASSIGNING <fs_selected_data> .
      ASSIGN COMPONENT 'SELECTED' OF STRUCTURE <fs_selected_data> TO <fs_select>.
      IF sy-subrc IS INITIAL.
        lv_select = <fs_select>.
        IF lv_select = 'X'.
          ASSIGN COMPONENT 'V_STTXT' OF STRUCTURE <fs_selected_data> TO <fs_status>.
          IF sy-subrc IS INITIAL.
            lv_status_op = <fs_status>.
          ENDIF.
          ASSIGN COMPONENT 'STTXT' OF STRUCTURE <fs_selected_data> TO <fs_status>.
          IF sy-subrc IS INITIAL.
            lv_status_or = <fs_status>.
          ENDIF.
*check for work center update , if fail then error
          IF ( lv_status_op CS 'DLT' ) AND ( ( lv_status_or NS 'REL' ) OR ( lv_status_or NS 'CRTD' ) ).
            MESSAGE text-002 TYPE 'E'.
            RETURN.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDLOOP.

    READ TABLE it_selected_objects WITH KEY  ('SELECTED') = 'X'
                                     TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'ZPM_POPUP_FOR_WRK_CENTER'.
    ENDIF.

  ENDIF.
ENDMETHOD.
ENDCLASS.
