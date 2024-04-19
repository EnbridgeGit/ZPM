*----------------------------------------------------------------------*
***INCLUDE Z_PM_BANNER_INTERFACE_ANALYF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PREPARE
*&---------------------------------------------------------------------*
*       Prepare
*----------------------------------------------------------------------*
FORM prepare .

  DATA: lt_logs     TYPE STANDARD TABLE OF zpmt_equip_log,
        lt_fl_level TYPE STANDARD TABLE OF zpmt_fl_level,

        lv_count    TYPE int4,
        lv_3rdlevel TYPE char16,
        lv_tabix    TYPE sy-tabix,
        lv_tabix2   TYPE sy-tabix,
        lv_floc     TYPE tplnr,

        lr_level    TYPE RANGE OF tplma,

        ls_final    LIKE LINE OF gt_final,
        ls_level    LIKE LINE OF lr_level.

* Get data from Banner Interface log table
  SELECT * FROM zpmt_equip_log
           INTO TABLE gt_logs
           WHERE equnr IN s_equnr.

  IF gt_logs IS INITIAL.
    MESSAGE i000(zpm) WITH 'No records found in Banner Log Table'(001)
                           'for selected criteria'(002).
  ENDIF.

* Get Current Installed Functional Locations for the Equipments
  SELECT equnr
         datab
         tplnr
         msgrp
         stort
         FROM v_equi
         INTO TABLE gt_equi
         FOR ALL ENTRIES IN gt_logs
         WHERE equnr EQ gt_logs-equnr AND
               datbi GT sy-datum      AND
               spras EQ sy-langu.

  IF sy-subrc EQ 0.
    SORT gt_equi BY equnr.
  ENDIF.

  lt_logs = gt_logs.
  SORT lt_logs BY zcat_code zgrp_code.
  DELETE ADJACENT DUPLICATES FROM lt_logs COMPARING zcat_code zgrp_code.

*-- Retrieve data from Ztable based on category code and group code
  SELECT * FROM zpmt_fl_level INTO TABLE  gt_fl_level
    FOR ALL ENTRIES IN lt_logs
              WHERE zcat_code = lt_logs-zcat_code AND
                    zgrp_code = lt_logs-zgrp_code.

  IF sy-subrc IS INITIAL.
    SORT gt_fl_level BY zcat_code zgrp_code ztech_obty.
  ENDIF.

  lt_fl_level =  gt_fl_level.
  SORT lt_fl_level BY ztech_obty.
  DELETE ADJACENT DUPLICATES FROM lt_fl_level COMPARING ztech_obty.

  SELECT tplnr tplma eqart FROM iflot
        INTO TABLE gt_iflot
        FOR ALL ENTRIES IN lt_fl_level
       WHERE eqart = lt_fl_level-ztech_obty.

  IF sy-subrc EQ 0.
    SORT gt_iflot BY tplnr.

    lt_logs = gt_logs.
    SORT lt_logs BY stort premise.
    DELETE ADJACENT DUPLICATES FROM lt_logs COMPARING stort premise.

**fetch the function location level 3 based on premise and station id.
    SELECT tplnr stort msgrp FROM iloa INTO TABLE gt_iloa_e
                                  FOR ALL ENTRIES IN lt_logs
                                  WHERE stort EQ lt_logs-stort AND
                                        msgrp EQ lt_logs-premise.

    IF sy-subrc EQ 0.
      SORT gt_iloa_e BY tplnr.
      DELETE gt_iloa_e WHERE tplnr = ''.
      SORT gt_iloa_e BY stort msgrp.
    ENDIF.

    LOOP AT gt_logs ASSIGNING <fs_logs>.

      gv_date = <fs_logs>-zzdate.
      gv_time = <fs_logs>-zztime.

      IF <fs_logs>-eqtyp = 'B' OR <fs_logs>-eqtyp = 'C' .

        IF <fs_logs>-stort  IS NOT INITIAL AND
          <fs_logs>-premise IS NOT INITIAL." skip the record where any of them is blank

          READ TABLE gt_iloa_e WITH KEY stort = <fs_logs>-stort
                                        msgrp = <fs_logs>-premise
                                        BINARY SEARCH
                                        TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.

          lv_tabix2 = sy-tabix.
          LOOP AT gt_iloa_e ASSIGNING <fs_iloa_e> FROM lv_tabix2.

            IF <fs_iloa_e>-stort NE <fs_logs>-stort OR
               <fs_iloa_e>-msgrp NE <fs_logs>-premise.
              EXIT.
            ENDIF.

            CLEAR lv_count.
            lv_count = strlen( <fs_iloa_e>-tplnr ).
            IF ( lv_count GE 8 )  AND ( lv_count LE 16 ).
*take only level 3 th Level Floc
              READ TABLE gt_fl_level ASSIGNING <fs_fl_level> WITH KEY
                               zcat_code = <fs_logs>-zcat_code
                               zgrp_code = <fs_logs>-zgrp_code
                               BINARY SEARCH.
              IF sy-subrc EQ 0.
                lv_3rdlevel = <fs_iloa_e>-tplnr.
                CONCATENATE lv_3rdlevel '-' <fs_fl_level>-zlevel INTO lv_floc RESPECTING BLANKS. "4th Level
                ls_level-low = lv_floc.
                ls_level-sign = 'I'.
                ls_level-option = 'EQ'.
                APPEND ls_level TO lr_level.
                CLEAR ls_level.
              ENDIF.
            ENDIF.

          ENDLOOP.
        ENDIF.
      ENDIF.
      CLEAR: lv_floc.
    ENDLOOP.

    SORT lr_level[] BY low.
    DELETE ADJACENT DUPLICATES FROM lr_level[] COMPARING low.

** from the above code wil get Level 4 flocation
**pass level 4 floc and EQART as Tech_obty in iflot to Superior floc
**fetch the all 5th level Floc(there will be multiple Flocation at level 5)
    SORT gt_iloa_e BY tplnr.
    IF lr_level[] IS NOT INITIAL.
      LOOP AT gt_iflot ASSIGNING <fs_iflot>.
        READ TABLE lr_level WITH KEY low = <fs_iflot>-tplma
                            BINARY SEARCH
                            TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          <fs_iflot>-del = abap_true.
        ELSE.
          READ TABLE gt_iloa_e ASSIGNING <fs_iloa_e> WITH KEY tplnr = <fs_iflot>-tplnr
                                                     BINARY SEARCH.
          IF sy-subrc EQ 0.
            <fs_iflot>-stort = <fs_iloa_e>-stort.
            <fs_iflot>-msgrp = <fs_iloa_e>-msgrp.
          ENDIF.
        ENDIF.
      ENDLOOP.

      DELETE gt_iflot WHERE del = abap_true.
    ENDIF.
  ENDIF.

  IF gt_iflot IS NOT INITIAL.
    SELECT qmnum
           tplnr
           FROM viqmel
           INTO TABLE gt_qmel
           FOR ALL ENTRIES IN gt_iflot
           WHERE tplnr = gt_iflot-tplnr AND
                 qmart = 'M4'.

    IF sy-subrc EQ 0.
      SORT gt_qmel BY tplnr.
    ENDIF.
    SORT gt_iflot BY eqart stort msgrp.
  ENDIF.

  LOOP AT gt_logs ASSIGNING <fs_logs>.
    ls_final-equnr = <fs_logs>-equnr.
    ls_final-equip_desc = <fs_logs>-eqktx.

    PERFORM update_msg USING <fs_logs>-error
                       CHANGING ls_final.

    CASE <fs_logs>-proc_status.
*      WHEN '1'.
*        ls_final-update_stat = 'Equipment Created'(003).
*        gv_created = gv_created + 1.
      WHEN '2'.
        ls_final-update_stat = 'Equipment Changed'(004).
        gv_changed = gv_changed + 1.
      WHEN '3'.
        ls_final-update_stat = 'Failed'(005).
        gv_errored  = gv_errored + 1.
      WHEN '5'.
        ls_final-update_stat = 'Equipment Create'(055).
        gv_created = gv_created + 1.
      WHEN OTHERS.
        ls_final-update_stat = 'No changes'(006).
        gv_nochang = gv_nochang + 1.
    ENDCASE.

    ls_final-banner_grp = <fs_logs>-zgrp_code.
    ls_final-banner_code = <fs_logs>-zcat_code.

    READ TABLE gt_iflot ASSIGNING <fs_iflot> WITH KEY eqart = <fs_logs>-eqart
                                                      stort = <fs_logs>-stort
                                                      msgrp = <fs_logs>-premise
                                                      BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_final-tplnr_banner = <fs_iflot>-tplnr.
    ENDIF.

    ls_final-banner_prm = <fs_logs>-premise.
    ls_final-banner_stort = <fs_logs>-stort.

    IF ls_final-tplnr_banner IS INITIAL.
      PERFORM update_msg USING 'Failed to determine Banner recommended Func. Loc'(026)
                         CHANGING ls_final.
    ENDIF.

    READ TABLE gt_equi ASSIGNING <fs_equi> WITH KEY equnr = <fs_logs>-equnr
                                                    BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_final-tplnr_sap = <fs_equi>-tplnr.
      IF <fs_equi>-tplnr IS NOT INITIAL.
        WRITE <fs_equi>-datab TO ls_final-install_date.
        ls_final-sap_prm = <fs_equi>-msgrp.
        ls_final-sap_stort = <fs_equi>-stort.
      ELSE.
        PERFORM update_msg USING 'Equipment currently not installed'(024)
                           CHANGING ls_final.
        ls_final-tplnr_sap = 'Not Installed'(046).
      ENDIF.
    ENDIF.

    ls_final-p_factor_new = <fs_logs>-p_factor.
    ls_final-p_factor_old = <fs_logs>-p_factor_old.
    ls_final-equip_status = <fs_logs>-status.

    IF ls_final-tplnr_banner NE ls_final-tplnr_sap.
      ls_final-tplnr_change = 'Yes'(025).
    ELSE.
      ls_final-tplnr_change = 'No'(029).
    ENDIF.
    IF ls_final-p_factor_old NE ls_final-p_factor_new.
      ls_final-p_factor_ch = 'Yes'(025).
    ELSE.
      ls_final-tplnr_change = 'No'(029).
    ENDIF.

    IF ls_final-tplnr_change IS NOT INITIAL.
      READ TABLE gt_qmel WITH KEY tplnr = ls_final-tplnr_banner
                         BINARY SEARCH
                         TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
        APPEND ls_final TO gt_final.
        APPEND ls_final TO gt_email.

        CLEAR ls_final.
        CONTINUE.
      ELSE.
        lv_tabix = sy-tabix.
        LOOP AT gt_qmel ASSIGNING <fs_qmel> FROM lv_tabix.
          IF <fs_qmel>-tplnr NE ls_final-tplnr_banner.
            EXIT.
          ENDIF.
          ls_final-banner_notif = <fs_qmel>-qmnum.
          APPEND ls_final TO gt_final.
          APPEND ls_final TO gt_email.
        ENDLOOP.
        CLEAR ls_final.
      ENDIF.
    ELSE.
      APPEND ls_final TO gt_final.
      APPEND ls_final TO gt_email.
      CLEAR ls_final.
    ENDIF.
  ENDLOOP.

  SORT gt_final BY equnr.

ENDFORM.                    " prepare
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_alv .

  DATA: lv_title  TYPE lvc_title,
        lv_title2 TYPE lvc_title,
        lv_ltext  TYPE scrtext_l,
        lv_mtext  TYPE scrtext_m,
        lv_stext  TYPE scrtext_s.

  DATA ls_layout_key TYPE salv_s_layout_key.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = rf_alv
        CHANGING
          t_table      = gt_final.
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  rf_columns = rf_alv->get_columns( ).
  rf_display = rf_alv->get_display_settings( ).

*   set ZEBRA pattern
  rf_display->set_striped_pattern( abap_true ).

* Title of ALV
  WRITE gv_date TO lv_title.
  WRITE gv_time TO lv_title2.
  CONCATENATE lv_title lv_title2 INTO lv_title SEPARATED BY space.
  CONCATENATE 'Banner Interface Report'(007) '-' lv_title INTO lv_title SEPARATED BY space.

*   Title to ALV
  rf_display->set_list_header( lv_title ).

  TRY.

      lv_ltext = 'Equipment'(008).
      lv_mtext = 'Equipment'(008).
      lv_stext = 'Equipment'(008).
      rf_column = rf_columns->get_column('EQUNR').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Equipment Desc.'(009).
      lv_mtext = 'Equipment Desc.'(009).
      lv_stext = 'Eqp. Desc.'(010).
      rf_column = rf_columns->get_column('EQUIP_DESC').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Tnx. Status'(038).
      lv_mtext = 'Tnx. Status'(038).
      lv_stext = 'Tnx.Status'(011).
      rf_column = rf_columns->get_column('UPDATE_STAT').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Func. loc mismatch'(012).
      lv_mtext = 'Func. loc mismatch'(012).
      lv_stext = 'FLMisMatch'(013).
      rf_column = rf_columns->get_column('TPLNR_CHANGE').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Banner FuncLoc'(039).
      lv_mtext = 'Banner FuncLoc'(039).
      lv_stext = 'BannerFLoc'(014).
      rf_column = rf_columns->get_column('TPLNR_BANNER').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'SAP FuncLoc'(040).
      lv_mtext = 'SAP FuncLoc'(040).
      lv_stext = 'SAPFuncLoc'(015).
      rf_column = rf_columns->get_column('TPLNR_SAP').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'PFactor Changed'(037).
      lv_mtext = 'PFactor Changed'(037).
      lv_stext = 'PF Changed'(016).
      rf_column = rf_columns->get_column('P_FACTOR_CH').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'PFactor Old'(036).
      lv_mtext = 'PFactor Old'(036).
      lv_stext = 'PF Old'(017).
      rf_column = rf_columns->get_column('P_FACTOR_OLD').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'PFactor New'(035).
      lv_mtext = 'PFactor New'(035).
      lv_stext = 'PF New'(018).
      rf_column = rf_columns->get_column('P_FACTOR_NEW').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Install Date'(019).
      lv_mtext = 'Install Date'(019).
      lv_stext = 'Install Dt'(041).
      rf_column = rf_columns->get_column('INSTALL_DATE').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Banner Station'(043).
      lv_mtext = 'Banner Station'(043).
      lv_stext = 'BnrStation'(044).
      rf_column = rf_columns->get_column('BANNER_STORT').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'SAP Station'(045).
      lv_mtext = 'SAP Station'(045).
      lv_stext = 'SAPStation'.
      rf_column = rf_columns->get_column('SAP_STORT').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Banner Premise'(031).
      lv_mtext = 'Banner Premise'(031).
      lv_stext = 'BnrPremise'(032).
      rf_column = rf_columns->get_column('BANNER_PRM').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).
      rf_column->set_output_length( 12 ).

      lv_ltext = 'SAP Premise'(033).
      lv_mtext = 'SAP Premise'(033).
      lv_stext = 'SAPPremise'(034).
      rf_column = rf_columns->get_column('SAP_PRM').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).
      rf_column->set_output_length( 12 ).

      lv_ltext = 'Banner Floc Notif'(020).
      lv_mtext = 'Banner Floc Notif'(020).
      lv_stext = 'BannerNtif'(042).
      rf_column = rf_columns->get_column('BANNER_NOTIF').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Banner Cat. Group'(053).
      lv_mtext = 'Banner Cat. Group'(053).
      lv_stext = 'BannerGrp'(054).
      rf_column = rf_columns->get_column('BANNER_GRP').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Banner Cat. Code'(051).
      lv_mtext = 'Banner Cat. Code'(051).
      lv_stext = 'BannerCode'(052).
      rf_column = rf_columns->get_column('BANNER_CODE').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Messages'(030).
      lv_mtext = 'Messages'(030).
      lv_stext = 'Messages'(030).
      rf_column = rf_columns->get_column('MESSAGE').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  rf_events = rf_alv->get_event( ).
  CREATE OBJECT rf_event.
  SET HANDLER rf_event->handle_double_click FOR rf_events. "register the event handler

*** Functions
  rf_alv_functions = rf_alv->get_functions( ).
  rf_alv_functions->set_all( abap_true ).
  rf_columns->set_optimize( 'X' ).

* Allow users to Save layout
  rf_layout = rf_alv->get_layout( ).
  ls_layout_key-report = sy-repid.
  rf_layout->set_key( ls_layout_key ).
  rf_layout->set_save_restriction( if_salv_c_layout=>restrict_user_dependant ).
  rf_layout->set_default( if_salv_c_bool_sap=>true ).

  event_class=>set_top_of_page( CHANGING co_alv = rf_alv ).
** Display the table
  rf_alv->display( ).

ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       Send email
*----------------------------------------------------------------------*
FORM send_email .

  CONSTANTS: c_tab TYPE c VALUE cl_bcs_convert=>gc_tab,
             c_cr  TYPE c VALUE cl_bcs_convert=>gc_crlf,
             c_ext TYPE soodk-objtp VALUE 'XLS',
             c_x   TYPE c VALUE 'X'.

  DATA:
  lr_recipient     TYPE REF TO if_recipient_bcs,
  lr_sender        TYPE REF TO cl_sapuser_bcs,
  lr_bcs_exception TYPE REF TO cx_bcs,
  lr_document      TYPE REF TO cl_document_bcs,
  lr_send_request  TYPE REF TO cl_bcs,

  lv_attcdoctype    TYPE soodk-objtp,
  lv_atttitle       TYPE sood-objdes,
  lv_num_rows       TYPE i,
  lv_size           TYPE so_obj_len,
  lv_subject        TYPE so_obj_des,
  lv_subject2       TYPE so_obj_des,
  lv_text_length    TYPE so_obj_len,
  lv_line           TYPE string,

  lt_mail_text      TYPE bcsy_text,
  lt_binary_content TYPE solix_tab,

  ls_mail_text_row TYPE soli.

  TRY.

      ls_mail_text_row = 'Dear User,'(021).
      APPEND ls_mail_text_row TO lt_mail_text.
      CLEAR ls_mail_text_row.

      APPEND ls_mail_text_row TO lt_mail_text.
      CLEAR ls_mail_text_row.

      ls_mail_text_row = 'Please find attached Banner Interface Execution Report.'(022).
      APPEND ls_mail_text_row TO lt_mail_text.
      CLEAR ls_mail_text_row.

      APPEND ls_mail_text_row TO lt_mail_text.
      CLEAR ls_mail_text_row.

      ls_mail_text_row = 'Thanks & Regards'(023).
      APPEND ls_mail_text_row TO lt_mail_text.
      CLEAR ls_mail_text_row.

      ls_mail_text_row = sy-uname.
      APPEND ls_mail_text_row TO lt_mail_text.
      CLEAR ls_mail_text_row.

* Define rows and file size
      DESCRIBE TABLE lt_mail_text LINES lv_num_rows.
      lv_num_rows = lv_num_rows * 255.
      MOVE lv_num_rows TO lv_text_length.

      WRITE gv_date TO lv_subject.
      WRITE gv_time TO lv_subject2.
      CONCATENATE lv_subject lv_subject2 INTO lv_subject SEPARATED BY space.
      CONCATENATE 'Banner Interface Report'(007) '-' lv_subject INTO lv_subject SEPARATED BY space.

      TRY.
          CALL METHOD cl_document_bcs=>create_document
            EXPORTING
              i_type    = 'RAW'
              i_subject = lv_subject
              i_length  = lv_text_length
              i_text    = lt_mail_text
            RECEIVING
              result    = lr_document.
        CATCH cx_document_bcs .                         "#EC NO_HANDLER
      ENDTRY.

      CONCATENATE
      'Equipment'(008) c_tab
      'Equipment Desc.'(009) c_tab
      'Equipment Status' c_tab
      'Tnx. Status'(038) c_tab
      'Func. loc mismatch'(012) c_tab
      'Banner FuncLoc'(039) c_tab
      'SAP FuncLoc'(040) c_tab
      'PFactor Changed'(037) c_tab
      'PFactor Old'(036) c_tab
      'PFactor New'(035) c_tab
      'Banner Station'(043) c_tab
      'SAP Station'(045) c_tab
      'Banner Premise'(031) c_tab
      'SAP Premise'(033) c_tab
      'Banner Cat. Group'(053) c_tab
      'Banner Cat. Code'(051) c_tab
      'Install Date'(019) c_tab
      'Banner Floc Notif'(020) c_tab
      'Messages'(030)
             c_cr INTO lv_line.

      LOOP AT gt_email ASSIGNING <fs_final>.
        CONCATENATE lv_line
              <fs_final>-equnr c_tab
              <fs_final>-equip_desc c_tab
              <fs_final>-equip_status c_tab
              <fs_final>-update_stat c_tab
              <fs_final>-tplnr_change c_tab
              <fs_final>-tplnr_banner c_tab
              <fs_final>-tplnr_sap c_tab
              <fs_final>-p_factor_ch c_tab
              <fs_final>-p_factor_old c_tab
              <fs_final>-p_factor_new c_tab
              <fs_final>-banner_stort c_tab
              <fs_final>-sap_stort c_tab
              <fs_final>-banner_prm c_tab
              <fs_final>-sap_prm c_tab
              <fs_final>-banner_grp c_tab
              <fs_final>-banner_code c_tab
              <fs_final>-install_date c_tab
              <fs_final>-banner_notif c_tab
              <fs_final>-message c_tab
              c_cr INTO lv_line.
      ENDLOOP.

      TRY.
          cl_bcs_convert=>string_to_solix(
          EXPORTING
          iv_string = lv_line
          iv_codepage = '4103' "suitable for MS Excel, leave empty
          iv_add_bom = c_x "for other doc types
          IMPORTING
          et_solix = lt_binary_content
          ev_size = lv_size ).
        CATCH cx_bcs.
          MESSAGE e445(so).
      ENDTRY.

      lv_attcdoctype = c_ext.
      lv_atttitle = lv_subject.

      CALL METHOD lr_document->add_attachment(
        i_attachment_type = lv_attcdoctype
        i_attachment_subject = lv_atttitle
        i_attachment_size = lv_size
        i_att_content_hex = lt_binary_content ).

      lr_send_request = cl_bcs=>create_persistent( ).
      lr_send_request->set_document( lr_document ).
* Define Sender
      lr_sender = cl_sapuser_bcs=>create( sy-uname ).
      TRY.
          CALL METHOD lr_send_request->set_sender
            EXPORTING
              i_sender = lr_sender.
        CATCH cx_send_req_bcs .                         "#EC NO_HANDLER
      ENDTRY.

* Define Recipient
*      lr_recipent = cl_cam_address_bcs=>create_internet_address( 'MEldhose@spectraenergy.com' ).
      lr_recipient = cl_distributionlist_bcs=>getu_persistent(
               i_dliname = 'ZBANNER_REP'
               i_private = space ).

      lr_send_request->add_recipient( EXPORTING i_recipient = lr_recipient
                                                i_express   = c_x ).
* Schedule
      lr_send_request->set_send_immediately( c_x ).

      lr_send_request->send( ).

      COMMIT WORK.
* Catch Execptions
    CATCH cx_bcs INTO lr_bcs_exception.                 "#EC NO_HANDLER
  ENDTRY.

ENDFORM.                    " SEND_EMAIL
*----------------------------------------------------------------------*
* CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS event_class IMPLEMENTATION.
  METHOD handle_double_click.

    DATA: ls_final TYPE ty_final.

    READ TABLE gt_final INTO ls_final INDEX row.
    IF sy-subrc EQ 0.
      SET PARAMETER ID 'EQN' FIELD ls_final-equnr.
      CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN.
    ENDIF.

  ENDMETHOD.                    "handle_double_click
  METHOD set_top_of_page.

    DATA: lo_header  TYPE REF TO cl_salv_form_layout_grid,
          lo_h_label TYPE REF TO cl_salv_form_label,
          lo_h_flow  TYPE REF TO cl_salv_form_layout_flow,

          lv_string TYPE string.

    CREATE OBJECT lo_header.

    lo_h_label = lo_header->create_label( row = 1 column = 1 ).
    lv_string = 'Run Statistics'(047).
    lo_h_label->set_text( lv_string ).
    lo_h_flow = lo_header->create_flow( row = 2 column = 1 ).

    lv_string = 'No of Equipments to be Created:'(048).
    lo_h_flow->create_text( text = lv_string ).
    lo_h_flow = lo_header->create_flow( row = 2 column = 2 ).
    lo_h_flow->create_text( text = gv_created ).
    lo_h_flow = lo_header->create_flow( row = 3 column = 1 ).

    lv_string = 'No of Equipments Changed:'(049).
    lo_h_flow->create_text( text = lv_string  ).
    lo_h_flow = lo_header->create_flow( row = 3 column = 2 ).
    lo_h_flow->create_text( text = gv_changed ).
    lo_h_flow = lo_header->create_flow( row = 4 column = 1 ).

    lv_string = 'No of Euipments in Error:'(050).
    lo_h_flow->create_text( text = lv_string ).
    lo_h_flow = lo_header->create_flow( row = 4 column = 2 ).
    lo_h_flow->create_text( text = gv_errored ).
    lo_h_flow = lo_header->create_flow( row = 5 column = 1 ).

    lv_string = 'No of Euipments without Changes: '.
    lo_h_flow->create_text( text = lv_string ).
    lo_h_flow = lo_header->create_flow( row = 5 column = 2 ).
    lo_h_flow->create_text( text = gv_nochang ).
    lo_h_flow = lo_header->create_flow( row = 6 column = 1 ).

    co_alv->set_top_of_list( lo_header ).
    co_alv->set_top_of_list_print( lo_header ).

  ENDMETHOD.                    "set_top_of_page

ENDCLASS. "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  UPDATE_MSG
*&---------------------------------------------------------------------*
*       Update Message
*----------------------------------------------------------------------*
FORM update_msg  USING p_message TYPE c
                 CHANGING p_final TYPE ty_final.

  IF p_final-message IS INITIAL.
    p_final-message = p_message.
  ELSE.
    CONCATENATE p_final-message p_message INTO p_final-message
                SEPARATED BY ';'.
  ENDIF.

ENDFORM.                    " UPDATE_MSG
