*&---------------------------------------------------------------------*
* Report  Z_PM_SHOPPAPER
* Author             : Eldhose Mathew
* Date               : 18/07/2014 (dd/mm/yyyy)
* Technical Contact  : Eldhose Mathew
* Business Contact   : Ron Torrance
* Purpose            : This is the driver program for Shop paper
*----------------------------------------------------------------------*
REPORT  z_pm_shoppaper.

DATA: gt_order_tline    TYPE STANDARD TABLE OF tline,
      gt_tcf10          TYPE STANDARD TABLE OF tcf10.

INCLUDE riprid01.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

* Print PDF
  PERFORM print_paper_pdf.

  INCLUDE riprif1a.

*&---------------------------------------------------------------------*
*&      Form  print_paper_pdf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM print_paper_pdf.

* pre fetch config data
  PERFORM pre_fetch_config_data.

* Import data
  PERFORM z_data_import.

* Print PDF
  PERFORM main_print_pdf.

ENDFORM.                    "PRINT_PAPER_PDF
*&---------------------------------------------------------------------*
*&      Form  main_print_pdf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM main_print_pdf.

* Open PDF form
  PERFORM open_form_pdf  USING space
                               caufvd-aufnr
                               space.

* Set Form title
  PERFORM set_title.

* Read order texts
  PERFORM read_order_text_tables.

* Read long texts for work order header
  PERFORM order_header_short_pdf.

  MOVE  gt_tline1[] TO gt_order_tline[].
  CLEAR gt_tline1[].

* Read Permits
  PERFORM permits_pdf USING caufvd-objnr.

* Read operations, material and PRT information
  PERFORM operations_with_mat_pdf.

* Prepare object list
  PERFORM object_list_pdf USING yes.

* Prepare notification header data
  PERFORM notification_header_detail_pdf.

* Print PDF
  PERFORM print_pdf.

* Close form
  PERFORM close_form_pdf.

ENDFORM.                    "MAIN_PRINT_PDF

INCLUDE riprif01.                     " General PRINT routines
INCLUDE riprif02.                     " General PRINT routines ORDERS
INCLUDE riprif0t.                     " Time zone support

*&---------------------------------------------------------------------*
*&      Form  print_pdf
*&---------------------------------------------------------------------*
*       PRint PDF
*----------------------------------------------------------------------*
FORM print_pdf .

  DATA: lv_sess_tzone  TYPE ttzz-tzone.
  DATA: ls_docparams   TYPE sfpdocparams.
  CONSTANTS lc_x       TYPE c VALUE 'X'.

* Expland long texts to 132 characters per line display
  PERFORM convert_hdrlongtxt_longer CHANGING gt_order_tline.

* Expland long texts to 132 characters per line display
  PERFORM convert_oprlongtxt_longer CHANGING gt_opr_text.

  IF NOT gv_fm_name IS INITIAL.

* CAll Form Function module
    CALL FUNCTION gv_fm_name
      EXPORTING
        /1bcdwb/docparams   = ls_docparams
        order_header        = gs_caufvd1
        object_connection   = gs_riwo1
        order_head_longtext = gt_order_tline
        notification_header = gs_viqmel
        priority            = t356_t
        object_list         = gt_ripw0
        revisions           = t352r
        order_operation     = gt_afvgd
        permits             = gt_t357g
        material_info       = gt_material
        services            = gt_service
        work_center         = gt_rcr01
        prt_order           = gt_affhd
        operation_longtext  = gt_opr_text
      IMPORTING
        /1bcdwb/formoutput  = gv_fpformoutput
      EXCEPTIONS
        usage_error         = 1
        system_error        = 2
        internal_error      = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.


  REFRESH : gt_tline1,gt_qkat_head, gt_qkat, lt_lines1,gt_afvgd,gt_ihpad1,gt_opr_text,
            gt_api_val,gt_ihgns,gt_ripw0,gt_api_kssk,gt_rcr01,gt_t357g.
  CLEAR : gs_wiprt, gs_viqmel, gs_tq80_t, gs_to24i,gs_wqmfe, gs_makt,gs_iloa, gt_wqmfe, gt_qkat_item.
  CLEAR : gt_qkat_header, gt_prt_longtext.
  CLEAR : gt_srvtyp_stlpos, gt_srvtyp_kt, gt_srvtyp_pn, gt_srv_time, gt_srv_text, gt_formula_body.
  CLEAR : gt_formulahdrs, gt_srv_lines, gt_srhdrs, gt_service.
  CLEAR : gt_material, gt_mat_longtext, gt_diadr, gt_bsvz, gt_wqmsm, gt_wqmur, gt_affhd, gt_tline3.

  gv_send_fax_ind = yes.
  gv_archive_ind  = yes.
  IF gv_send_fax = yes.
    PERFORM send_fax_pdf.
  ENDIF.
  IF gv_archive =  yes.
    PERFORM archive_pdf.
  ENDIF.

ENDFORM.                    " PRINT_PDF
*&---------------------------------------------------------------------*
*&      Form  operations_with_mat_pdf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM operations_with_mat_pdf.

  iafvgd = space.
  CLEAR gt_affhd.
  LOOP AT iafvgd WHERE aufpl = caufvd-aufpl.
    afvgd = iafvgd.

* Check if Operation is allowed for print
    PERFORM check_print_status USING afvgd-objnr
                                     wworkpaper-pm_delta_p
                                     rc.
    CHECK rc = 0.

* Check if operation is selected in case of Print - Operations
    IF op_entries > 0.
      LOOP AT op_print_tab WHERE
              flg_sel = 'X'
         AND  vornr   = afvgd-vornr
         AND  uvorn   = afvgd-uvorn.
      ENDLOOP.
      CHECK syst-subrc = 0.
    ENDIF.

* Read Operation texts
    PERFORM read_op_text_tables.

    iafvgd-flg_frd = afvgd-flg_frd.
    CHECK t430-vrgd = yes.
    gs_afvgd = iafvgd.
    APPEND gs_afvgd TO gt_afvgd.

    gs_rcr01-arbid = afvgd-arbid.
    gs_rcr01-arbpl = rcr01-arbpl.
    gs_rcr01-ktext = rcr01-ktext.
    gs_rcr01-werks = rcr01-werks.
    gs_rcr01-vornr = afvgd-vornr.
    APPEND gs_rcr01 TO gt_rcr01.
    CLEAR gs_rcr01.
    DELETE ADJACENT DUPLICATES FROM gt_rcr01.

* Prepare operation texts for print
    PERFORM print_operation_text_pdf.

* Prepare operations service details for print
    IF NOT afvgd-packno IS INITIAL.
      PERFORM service_package_pdf USING afvgd-packno afvgd-flg_frd.
    ENDIF.

*  Prepare materials in operation for print
    iresbd = space.
    LOOP AT iresbd WHERE xloek = space
                   AND   aufpl = afvgd-aufpl
                   AND   aplzl = afvgd-aplzl.
      resbd = iresbd.
      MOVE-CORRESPONDING resbd TO gs_material.
      APPEND gs_material TO gt_material.
      CLEAR gs_material.
    ENDLOOP.

* Prepare PRT details for Print
    PERFORM prt_print_pdf USING afvgd-aufpl
                            afvgd-aplzl.

  ENDLOOP.

ENDFORM.                    "OPERATIONS_WITH_MAT_PDF
*&---------------------------------------------------------------------*
*&      Form  PRT_PRINT_PDF
*&---------------------------------------------------------------------*
*       Production resources and tools
*----------------------------------------------------------------------*
FORM  prt_print_pdf USING aufpl LIKE afvgd-aufpl
                      aplzl LIKE afvgd-aplzl.

  LOOP AT iaffhd WHERE aufpl = aufpl
                 AND   aplzl = aplzl.

* Check if PRT is applicable for Print
    affhd = iaffhd.
    READ TABLE gt_tcf10 INTO tcf10 WITH KEY steuf = affhd-steuf.
    IF tcf10-xdruck = 'X'.  " FHM is to be printed

      gs_affhd = affhd.
      APPEND gs_affhd TO gt_affhd.
      CLEAR gs_affhd.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "PRT_PRINT_PDF
*&---------------------------------------------------------------------*
*&      Form  z_data_import
*&---------------------------------------------------------------------*
*       Order data import
*----------------------------------------------------------------------*
FORM z_data_import .

  DATA h_t024i LIKE t024i.

* Import Notification data
  PERFORM notification_data_import
    TABLES
         iviqmfe
         iviqmma
         iviqmsm
         iviqmur
         iqkat
         ihpad_tab
    CHANGING
         iviqmel
         riwo1
         riwo00
         rqm00.

  viqmel = iviqmel.

* Import general data
  PERFORM general_data_import.

* Read texts of technical attributes
  IF original_print_language <> print_language.
    PERFORM read_object_texts USING riwo1 print_language.
  ENDIF.

  REFRESH iriwo1.
  iriwo1 = riwo1.
  APPEND iriwo1.

* Read work order data
  PERFORM only_order_data_import
    TABLES
     op_print_tab
     kbedp_tab
     ihpad_tab
     ihsg_tab
     ihgns_tab
     iafvgd
     iripw0
     iresbd
     iaffhd
    CHANGING
     caufvd
     riwo1
     iloa.

* Read more texts of technical objects
  IF original_print_language <> print_language.
    PERFORM read_object_texts USING riwo1 print_language.
  ENDIF.

  iriwo1 = riwo1.
  APPEND iriwo1.

  READ TABLE iriwo1 WITH KEY objnr = viqmel-objnr.
  riwo1 = iriwo1.

  CLEAR afvgd.
  CLEAR resbd.

  DESCRIBE TABLE op_print_tab LINES op_entries.

ENDFORM.                    " Z_DATA_IMPORT
*&---------------------------------------------------------------------*
*&      Form  convert_oprlongtxt_longer
*&---------------------------------------------------------------------*
*       Elongate the operation longtext table
*----------------------------------------------------------------------*
FORM convert_oprlongtxt_longer CHANGING p_table TYPE ops_table_for_matlongtxt.

  DATA: lit_opr_long TYPE STANDARD TABLE OF material_longtext,
        lit_string   TYPE TABLE OF string,
        lwa_opr_long TYPE material_longtext,
        lwa_oplong   TYPE material_longtext,
        lwa_opr_text TYPE material_longtext.

  DATA: lwa_line      TYPE tline,
        lwa_line_next TYPE tline,
        lwa_long_line TYPE tline,
        lv_tdformat   TYPE tline-tdformat,
        lv_length     TYPE sy-tabix,
        lv_start      TYPE c,
        lv_space      TYPE c,
        lv_string     TYPE string.

  lit_opr_long = p_table.
  REFRESH p_table.

  SORT lit_opr_long BY vornr uvorn.
  DELETE ADJACENT DUPLICATES FROM lit_opr_long COMPARING vornr uvorn.
  LOOP AT lit_opr_long INTO lwa_opr_long.
    LOOP AT gt_opr_text INTO lwa_opr_text WHERE vornr = lwa_opr_long-vornr AND
                                                uvorn = lwa_opr_long-uvorn.

      IF lwa_opr_text-tdline IS INITIAL.
        REFRESH: lit_string.
        CALL FUNCTION 'ZPM_WORD_WRAP'
          EXPORTING
            textline  = lv_string
            outputlen = 132
          TABLES
            out_lines = lit_string.

        LOOP AT lit_string INTO lv_string.
          MOVE-CORRESPONDING lwa_opr_text TO lwa_oplong.
          lwa_oplong-tdline = lv_string.
          APPEND lwa_oplong TO p_table.
          CLEAR lwa_oplong.
        ENDLOOP.
        MOVE-CORRESPONDING lwa_opr_text TO lwa_oplong.
        APPEND lwa_oplong TO p_table.
        CLEAR lwa_oplong.
        CLEAR lv_string.
      ELSE.
        IF lv_string IS  NOT INITIAL.
          CONCATENATE lv_string lwa_opr_text-tdline INTO lv_string SEPARATED BY space.
        ELSE.
          lv_string = lwa_opr_text-tdline.
        ENDIF.
      ENDIF.
    ENDLOOP.

    REFRESH: lit_string.
    CALL FUNCTION 'ZPM_WORD_WRAP'
      EXPORTING
        textline  = lv_string
        outputlen = 132
      TABLES
        out_lines = lit_string.

    LOOP AT lit_string INTO lv_string.
      MOVE-CORRESPONDING lwa_opr_text TO lwa_oplong.
      lwa_oplong-tdline = lv_string.
      APPEND lwa_oplong TO p_table.
      CLEAR lwa_oplong.
    ENDLOOP.
    MOVE-CORRESPONDING lwa_opr_text TO lwa_oplong.
    APPEND lwa_oplong TO p_table.
    CLEAR lwa_oplong.
    CLEAR lv_string.

  ENDLOOP.

ENDFORM.                    " convert_hdrlongtxt_longer
*&---------------------------------------------------------------------*
*&      Form  PRE_FETCH_CONFIG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pre_fetch_config_data .

  SELECT * FROM  tcf10
           INTO TABLE gt_tcf10.

ENDFORM.                    " PRE_FETCH_CONFIG_DATA
*&---------------------------------------------------------------------*
*&      Form  CONVERT_HDRLONGTXT_LONGER
*&---------------------------------------------------------------------*
*       Convert header long text to longer lines to fit form
*----------------------------------------------------------------------*
FORM convert_hdrlongtxt_longer CHANGING p_table TYPE tline_t.

  DATA: lwa_line        TYPE tline,
        lwa_line_next   TYPE tline,
        lwa_long_line   TYPE tline,
        lv_tdformat     TYPE tline-tdformat,
        lit_string      TYPE TABLE OF string,
        lv_length       TYPE sy-tabix,
        lv_start        TYPE c,
        lit_table_short TYPE tline_t,
        lv_space        TYPE c,
        lv_string       TYPE string.

  lit_table_short = p_table.
  REFRESH p_table.

  LOOP AT lit_table_short INTO lwa_line.
    IF lwa_line-tdformat IS NOT INITIAL.
      IF sy-tabix NE 1.
        REFRESH: lit_string.
        CALL FUNCTION 'ZPM_WORD_WRAP'
          EXPORTING
            textline  = lv_string
            outputlen = 132
          TABLES
            out_lines = lit_string.

        LOOP AT lit_string INTO lv_string.
          IF sy-tabix EQ 1.
            lwa_long_line-tdformat = lv_tdformat.
            CLEAR lv_tdformat.
          ENDIF.
          lwa_long_line-tdline = lv_string.
          APPEND lwa_long_line TO p_table.
          CLEAR lwa_long_line.
        ENDLOOP.
        IF sy-subrc NE 0.
          lwa_long_line-tdformat = lv_tdformat.
          APPEND lwa_long_line TO p_table.
          CLEAR lwa_long_line.
        ENDIF.
        CLEAR lv_string.
        lv_tdformat = lwa_line-tdformat.
        CLEAR lv_space.
      ELSE.
        lv_tdformat = lwa_line-tdformat.
      ENDIF.
    ENDIF.

    IF lv_string IS  NOT INITIAL.
      CONCATENATE lv_string lwa_line-tdline INTO lv_string SEPARATED BY space.
    ELSE.
      lv_string = lwa_line-tdline.
    ENDIF.
  ENDLOOP.

  REFRESH: lit_string.
  CALL FUNCTION 'ZPM_WORD_WRAP'
    EXPORTING
      textline  = lv_string
      outputlen = 132
    TABLES
      out_lines = lit_string.

  LOOP AT lit_string INTO lv_string.
    IF sy-tabix EQ 1.
      lwa_long_line-tdformat = lv_tdformat.
      CLEAR lv_tdformat.
    ENDIF.
    lwa_long_line-tdline = lv_string.
    APPEND lwa_long_line TO p_table.
    CLEAR lwa_long_line.
  ENDLOOP.
  CLEAR lv_string.

ENDFORM.                    " CONVERT_HDRLONGTXT_LONGER
