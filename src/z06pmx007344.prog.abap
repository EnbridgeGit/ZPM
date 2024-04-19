*&---------------------------------------------------------------------*
*&  Include           Z06PMX007344
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SAVE_DEFAULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_default .

  DATA : x_zntdeflt TYPE z06pmt_cp_deflt,
           l_key      TYPE indx-srtfd.

  IF x_defaults <> x_nt_defaults.
    MOVE-CORRESPONDING x_defaults TO x_zntdeflt.
    x_zntdeflt-uname = sy-uname.

    MODIFY z06pmt_cp_deflt FROM x_zntdeflt.
    IF sy-subrc = 0..
      COMMIT WORK.
    ENDIF.
    x_nt_defaults = x_defaults.
  ENDIF.


*  IF s_arbpl[] <> r_arbpl[] OR s_strno[] <> r_strno[] OR s_eqn[] <> r_equnr[].
*    CONCATENATE sy-cprog
*                sy-uname
*                INTO l_key.
**    EXPORT s_arbpl
**           s_strno
**           TO DATABASE indx(nt) ID l_key.
*    r_arbpl[] = s_arbpl[].
*    r_strno[] = s_strno[].
*    r_equnr[] = s_eqn[].
*
*  ENDIF.
*{   INSERT         D30K924358                                        1
  IF s_arbpl[] <> r_arbpl[] OR s_strno[] <> r_strno[] OR s_eqn[] <> r_equnr[] OR s_date[] <> r_date[].
    CONCATENATE sy-cprog
                sy-uname
                INTO l_key.
    EXPORT s_arbpl
           s_strno
           s_eqn
           s_date
           TO DATABASE indx(nt) ID l_key.
    r_arbpl[] = s_arbpl[].
    r_strno[] = s_strno[].
    r_equnr[] = s_eqn[].
    r_date[]  = s_date[].
  ENDIF.
*}   INSERT

ENDFORM.                    " SAVE_DEFAULT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_logo .

*  TYPES: BEGIN OF ty_graphic_table,
*              line(255) TYPE x,
*           END OF ty_graphic_table.
*
*  DATA:  ls_stxbmaps TYPE stxbitmaps,
*         ls_pict_tab TYPE ty_graphic_table.
*
*  DATA:  lv_graphic_url(255),
*         lv_pic_name     TYPE tdobname,
*         lv_url          TYPE char255,
*         lv_graphic_size TYPE i,
*         lv_bytecnt TYPE i,
*         lt_content TYPE STANDARD TABLE OF bapiconten INITIAL SIZE 0,
*         lv_graphic_xstr TYPE xstring,
*         lv_graphic_conv TYPE i,
*         lv_graphic_offs TYPE i,
*         lv_container TYPE scrfname VALUE 'LOGO_9000'.
*
*  CONSTANTS:  co_sap_tab_unknown(9)   TYPE c VALUE 'X-UNKNOWN',
*              co_lifetime_transaction TYPE c VALUE 'T'.
*
*  DATA: lt_pict_tab TYPE TABLE OF ty_graphic_table.         "image table
*
**  CHECK z06pmt_cp_logo-logo IS NOT INITIAL.
*
*  CASE 'X'.
*    WHEN gs_defaults-zzall.
*      lv_pic_name = co_all_logo.
*    WHEN gs_defaults-zzdy.
*      lv_pic_name = co_dy_logo.
*    WHEN gs_defaults-zzla.
*      lv_pic_name = co_la_logo.
*    WHEN gs_defaults-zzld.
*      lv_pic_name = co_ld_logo.
*    WHEN OTHERS.
*  ENDCASE.
*
*  IF lv_pic_name IS INITIAL.
*    lv_pic_name = co_all_logo.
*  ENDIF.
*
*
**- GET BMP image *-
*  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
*    EXPORTING
*      p_object  = 'GRAPHICS'
*      p_name    = lv_pic_name
*      p_id      = 'BMAP'
*      p_btype   = 'BCOL' "(BMON = black&white, BCOL = colour)
*    RECEIVING
*      p_bmp     = lv_graphic_xstr
*    EXCEPTIONS
*      not_found = 1
*      OTHERS    = 2.
*
*  lv_graphic_size = xstrlen( lv_graphic_xstr ).
*  CHECK lv_graphic_size > 0.                  "check whether image exists
*
*  lv_graphic_conv = lv_graphic_size.
*  lv_graphic_offs = 0.
*
**- prepare the image as data table *-
*  WHILE lv_graphic_conv > 255.
*    ls_pict_tab-line = lv_graphic_xstr+lv_graphic_offs(255).
*    APPEND ls_pict_tab TO lt_pict_tab.
*    lv_graphic_offs = lv_graphic_offs + 255.
*    lv_graphic_conv = lv_graphic_conv - 255.
*  ENDWHILE.
*
**- append rest of the lines of the image *-
*  ls_pict_tab-line = lv_graphic_xstr+lv_graphic_offs(lv_graphic_conv).
*  APPEND ls_pict_tab TO lt_pict_tab .
*  CLEAR ls_pict_tab.
*
**- create URL for the image *-
*  CALL FUNCTION 'DP_CREATE_URL'
*    EXPORTING
*      type     = 'IMAGE'
*      subtype  = co_sap_tab_unknown " 'X-UNKNOWN'
*      size     = lv_graphic_size
*      lifetime = co_lifetime_transaction "'T'
*    TABLES
*      data     = lt_pict_tab
*    CHANGING
*      url      = lv_url.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    EXIT.
*  ENDIF.
*
*  IF go_pic_container IS INITIAL.
*
**- create picture container *-
*    CREATE OBJECT go_pic_container
*      EXPORTING
*        container_name = lv_container.
*
**- create picture control *-
*    CREATE OBJECT go_picture_control
*      EXPORTING
*        parent = go_pic_container.
*
*
**- display image from URL *-
*    CALL METHOD go_picture_control->load_picture_from_url_async
*      EXPORTING
*        url = lv_url.
*
*
*  ELSE.
*
*    CALL METHOD go_picture_control->clear_picture.
*
**- display image from URL *-
*    CALL METHOD go_picture_control->load_picture_from_url_async
*      EXPORTING
*        url = lv_url.
*
*  ENDIF.

ENDFORM.                    " DISPLAY_LOGO
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_NOTIFICATION_CHART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_notification_chart .

*  IF gv_firstcall IS INITIAL.

  gv_retval = cl_gfw=>ok.
  CREATE OBJECT go_inst.
  go_manager = go_inst.
  CALL METHOD go_manager->init
    IMPORTING
      id     = gv_id
      retval = gv_retval.

  IF gv_retval NE cl_gfw=>ok.
    CALL METHOD cl_gfw=>show_msg
      EXPORTING
        msgno = gv_retval.
    CLEAR: go_manager, go_inst.
  ELSE.
    PERFORM fill_dc.
    IF gv_retval NE cl_gfw=>ok.
      CALL METHOD cl_gfw=>show_msg
        EXPORTING
          msgno = gv_retval.
    ELSE.
      CREATE OBJECT go_chart_container
        EXPORTING
          container_name = 'CHART_9000'.
      CREATE OBJECT go_chart_inst.
      CALL METHOD go_chart_inst->if_graphic_proxy~init
        EXPORTING
          parent     = go_chart_container
          dc         = go_inst
          prod_id    = cl_gui_gp_pres=>co_prod_chart
          force_prod = gfw_true
        IMPORTING
          retval     = gv_retval.

      IF gv_retval = cl_gfw=>ok.
        CALL METHOD go_chart_inst->set_dc_names
          EXPORTING
            obj_id = 'OBJID'
            dim1   = 'X_VAL'
            dim2   = 'Y_VAL'
            grp_id = 'GRPID'
            text   = 'TEXT'
          IMPORTING
            retval = gv_retval.
      ENDIF.
      IF gv_retval = cl_gfw=>ok.
*        PERFORM set_customzing.
      ENDIF.
      IF gv_retval = cl_gfw=>ok.
        CALL METHOD go_chart_inst->if_graphic_proxy~activate
          IMPORTING
            retval = gv_retval.
      ENDIF.
      IF gv_retval NE cl_gfw=>ok.
        CALL METHOD cl_gfw=>show_msg
          EXPORTING
            msgno = gv_retval.
      ENDIF.
    ENDIF.
    gv_firstcall = 1.
  ENDIF.
*  ENDIF.

  IF go_manager IS NOT INITIAL.
    CALL METHOD go_manager->distribute_changes
      IMPORTING
        retval = gv_retval.

    IF gv_retval NE cl_gfw=>ok.
      CALL METHOD cl_gfw=>show_msg
        EXPORTING
          msgno = gv_retval.
    ENDIF.
  ENDIF.
ENDFORM.                    " DISPLAY_NOTIFICATION_CHART
*&---------------------------------------------------------------------*
*&      Form  FILL_DC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_dc .

  DATA:   lv_obj   TYPE gfwdcpres,
          lv_objid TYPE i,
          lv_line  TYPE i,
          lv_per   TYPE p DECIMALS 2,
          lv_per_text(6) TYPE c,
          lv_index TYPE i.

* fill dc with initial data
  CASE gv_save_code.
    WHEN 'F61'.  " Notification
      IF go_manager IS INITIAL.
        gv_retval = cl_gfw=>e_gp_dchandle.
        EXIT.
      ENDIF.
      gv_retval = cl_gfw=>ok.

      SORT gt_notif BY qmart.
      DESCRIBE TABLE gt_notif LINES lv_line.


      LOOP AT gt_notif.
        AT NEW qmart.
          lv_index = 0.
          lv_obj-x_val = gt_notif-qmart.
        ENDAT.
        lv_index = lv_index + 1.
        CLEAR gt_notif.
        AT END OF qmart.
          lv_per = lv_index / lv_line * 100.
          lv_per_text = lv_per.

          lv_obj-objid = lv_objid.
          lv_obj-grpid = 'Notifications'.
          CONCATENATE lv_per_text '%' INTO lv_obj-text.
          CONCATENATE lv_obj-x_val lv_obj-text
                      INTO lv_obj-text SEPARATED BY space.
          lv_obj-y_val = lv_index.
          CALL METHOD go_inst->set_obj_values
            EXPORTING
              id     = gv_id
              obj    = lv_obj
            IMPORTING
              retval = gv_retval.
          IF gv_retval <> cl_gfw=>ok. EXIT. ENDIF.
          lv_objid = lv_objid + 1.
        ENDAT.
      ENDLOOP.

    WHEN 'F62'. " Order

      IF go_manager IS INITIAL.
        gv_retval = cl_gfw=>e_gp_dchandle.
        EXIT.
      ENDIF.
      gv_retval = cl_gfw=>ok.

      SORT gt_order BY auart.
      DESCRIBE TABLE gt_order LINES lv_line.

      LOOP AT gt_order.
        AT NEW auart.
          lv_index = 0.
          lv_obj-x_val = gt_order-auart.
        ENDAT.
        lv_index = lv_index + 1.
        CLEAR gt_order.
        AT END OF auart.
          lv_per = lv_index / lv_line * 100.
          lv_per_text = lv_per.

          lv_obj-objid = lv_objid.
          lv_obj-grpid = 'Orders'.
          CONCATENATE lv_per_text '%' INTO lv_obj-text.
          CONCATENATE lv_obj-x_val lv_obj-text
                      INTO lv_obj-text SEPARATED BY space.
          lv_obj-y_val = lv_index.
          CALL METHOD go_inst->set_obj_values
            EXPORTING
              id     = gv_id
              obj    = lv_obj
            IMPORTING
              retval = gv_retval.
          IF gv_retval <> cl_gfw=>ok. EXIT. ENDIF.
          lv_objid = lv_objid + 1.
        ENDAT.
      ENDLOOP.

  ENDCASE.

ENDFORM.                    " FILL_DC
*&---------------------------------------------------------------------*
*&      Form  SET_CUSTOMZING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_customzing .

  CLASS cl_cu_drawing_area DEFINITION LOAD.
  CLASS cl_cu_values       DEFINITION LOAD.
  CLASS cl_cu_point        DEFINITION LOAD.
  CLASS cl_cu_axis         DEFINITION LOAD.
  CLASS cl_cu_data_sheet   DEFINITION LOAD.

  DATA: lt_groups TYPE gfw_grpid_list,
        lt_points TYPE gfw_refobj_list,
        lv_title  TYPE gfwcuvac VALUE 'Orders'.

  DATA: lo_bundle_display TYPE REF TO cl_cu_display_context,
        lo_bundle         TYPE REF TO if_customizing.

* let the proxy create customizing-bundles
*  APPEND co_gfw_prog_objid_1 TO points. " for explosion of first piece
  APPEND 'Orders' TO lt_groups.

  CALL METHOD go_chart_inst->create_customizing
    EXPORTING
      instance_id = 'PIE'
      grpids      = lt_groups
*     pointids    = points
      title       = lv_title
    IMPORTING
      retval      = gv_retval.

  IF gv_retval <> cl_gfw=>ok.
    CALL METHOD cl_gfw=>show_msg
      EXPORTING
        msgno = gv_retval.
    EXIT.
  ENDIF.

* Remove Legend

  CALL METHOD go_chart_inst->if_graphic_proxy~get_cu_bundle
    EXPORTING
      port        = if_graphic_proxy=>co_port_chart
      bundle_type = cl_cu=>co_clsid_data_sheet
    IMPORTING
      bundle      = lo_bundle.

  CALL METHOD lo_bundle->set
    EXPORTING
      attr_id = cl_cu_data_sheet=>co_legend_symbol
      value   = 0.


* get values and set chart type

  IF 1 = 1.

    CALL METHOD go_chart_inst->if_graphic_proxy~get_cu_bundle
      EXPORTING
        port        = if_graphic_proxy=>co_port_chart
        bundle_type = cl_cu=>co_clsid_values
        key         = 'Orders'
      IMPORTING
        bundle      = lo_bundle.

    CALL METHOD lo_bundle->set
      EXPORTING
        attr_id = cl_cu_values=>co_style
        value   = 27. " pie

    CALL METHOD lo_bundle->set
      EXPORTING
        attr_id = cl_cu_values=>co_label_auto
        value   = gfw_false.

    CALL METHOD lo_bundle->set
      EXPORTING
        attr_id = cl_cu_values=>co_label_type
        value   = 3.

  ENDIF.
* get drawing area and remove border line


  CALL METHOD go_chart_inst->if_graphic_proxy~get_cu_bundle
    EXPORTING
      port        = if_graphic_proxy=>co_port_chart
      bundle_type = cl_cu=>co_clsid_drawing_area
    IMPORTING
      bundle      = lo_bundle.

  CALL METHOD lo_bundle->get
    EXPORTING
      attr_id = cl_cu_drawing_area=>co_display_context
    IMPORTING
      value   = lo_bundle_display.

  CALL METHOD lo_bundle_display->if_customizing~set
    EXPORTING
      attr_id = cl_cu_display_context=>co_le_style
      value   = 1. " none

  CALL METHOD lo_bundle_display->if_customizing~set
    EXPORTING
      attr_id = cl_cu_display_context=>co_visibility
      value   = 1.

  CALL METHOD lo_bundle->set
    EXPORTING
      attr_id = cl_cu_drawing_area=>co_display_context
      value   = lo_bundle_display.

*  Testing

*  CALL METHOD gp_inst->if_graphic_proxy~get_cu_bundle
*    EXPORTING
*      port        = if_graphic_proxy=>co_port_chart_x_prim_axis
*      bundle_type = cl_cu=>co_clsid_axis
*    IMPORTING
*      bundle      = bundle.

ENDFORM.                    " SET_CUSTOMZING
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ORDER_CHART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_floc_chart_type .

  DATA: y_values  TYPE TABLE OF gprval WITH HEADER LINE,
  x_texts        TYPE TABLE OF gprtxt WITH HEADER LINE.
  DATA ok_code LIKE sy-ucomm.
  DATA : lv_label TYPE string,
        lv_count TYPE string,
        lv_cnt TYPE i.
  FIELD-SYMBOLS <fs_dynamic> TYPE any.
  REFRESH y_values.
  REFRESH x_texts.
  lv_cnt = 1.


*  IF ta_statistics[] IS NOT INITIAL.

  LOOP AT  ta_statistics .
    IF lv_cnt <= 10." this is the maximum limit a graph can hol
      lv_count = lv_cnt.
      y_values-rowtxt = 'Functional Location'.
*    lv_label = 'Y_VALUES-VA'.
*    lv_label = '(Y_VALUES-VAL)'+ lv_count.
      CONCATENATE 'Y_VALUES-VAL' lv_count INTO lv_label.
      lv_cnt = lv_cnt + 1.
      ASSIGN (lv_label) TO <fs_dynamic>.
      <fs_dynamic> = ta_statistics-count.
      UNASSIGN <fs_dynamic>.
      APPEND y_values.

      x_texts-coltxt = ta_statistics-field.
      APPEND x_texts.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'GFW_PRES_SHOW'
    EXPORTING
      container         = 'CONTAINER'
      presentation_type = gfw_prestype_vertical_bars
    TABLES
      values            = y_values
      column_texts      = x_texts
    EXCEPTIONS
      error_occurred    = 1
      OTHERS            = 2.

*  ENDIF.
ENDFORM.                    "display_floc_chart_type
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ORDER_CHART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_order_chart_type .

  DATA: y_values  TYPE TABLE OF gprval WITH HEADER LINE,
  x_texts        TYPE TABLE OF gprtxt WITH HEADER LINE.
  DATA ok_code LIKE sy-ucomm.
  DATA : lv_label TYPE string,
        lv_count TYPE string,
        lv_cnt TYPE i.
  FIELD-SYMBOLS <fs_dynamic> TYPE any.
  REFRESH y_values.
  REFRESH x_texts.
  lv_cnt = 1.

*  IF ta_statistics[] IS NOT INITIAL.

  LOOP AT  ta_statistics .
    IF lv_cnt <= 32." this is the maximum limit a graph can hol
      lv_count = lv_cnt.
      y_values-rowtxt = 'ORDER TYPE'.
*    lv_label = 'Y_VALUES-VA'.
*    lv_label = '(Y_VALUES-VAL)'+ lv_count.
      CONCATENATE 'Y_VALUES-VAL' lv_count INTO lv_label.
      lv_cnt = lv_cnt + 1.
      ASSIGN (lv_label) TO <fs_dynamic>.
      <fs_dynamic> = ta_statistics-count.
      UNASSIGN <fs_dynamic>.
      APPEND y_values.

      x_texts-coltxt = ta_statistics-field.
      APPEND x_texts.
    ENDIF.
  ENDLOOP.


  CALL FUNCTION 'GFW_PRES_SHOW'
    EXPORTING
      container         = 'CONTAINER'
      presentation_type = gfw_prestype_vertical_bars
    TABLES
      values            = y_values
      column_texts      = x_texts
    EXCEPTIONS
      error_occurred    = 1
      OTHERS            = 2.

*  ENDIF.
ENDFORM.                    " DISPLAY_ORDER_CHART

*&---------------------------------------------------------------------*
*&      Form  display_order_fail_chart
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_fail_order_chart .

  DATA: y_values  TYPE TABLE OF gprval WITH HEADER LINE,
  x_texts        TYPE TABLE OF gprtxt WITH HEADER LINE.
  DATA ok_code LIKE sy-ucomm.
  DATA : lv_label TYPE string,
        lv_count TYPE string,
        lv_cnt TYPE i.
  FIELD-SYMBOLS <fs_dynamic> TYPE any.
  REFRESH y_values.
  REFRESH x_texts.
  lv_cnt = 1.

*  IF ta_statistics[] IS NOT INITIAL.

  LOOP AT  ta_statistics .
    IF lv_cnt <= 10." this is the maximum limit a graph can hol
      lv_count = lv_cnt.
      y_values-rowtxt = 'CODE GROUP'.
*    lv_label = 'Y_VALUES-VA'.
*    lv_label = '(Y_VALUES-VAL)'+ lv_count.
      CONCATENATE 'Y_VALUES-VAL' lv_count INTO lv_label.
      lv_cnt = lv_cnt + 1.
      ASSIGN (lv_label) TO <fs_dynamic>.
      <fs_dynamic> = ta_statistics-count.
      UNASSIGN <fs_dynamic>.
      APPEND y_values.

      x_texts-coltxt = ta_statistics-field.
      APPEND x_texts.
    ENDIF.
  ENDLOOP.


  CALL FUNCTION 'GFW_PRES_SHOW'
    EXPORTING
      container         = 'CONTAINER'
      presentation_type = gfw_prestype_vertical_bars
    TABLES
      values            = y_values
      column_texts      = x_texts
    EXCEPTIONS
      error_occurred    = 1
      OTHERS            = 2.

*  ENDIF.
ENDFORM.                    " DISPLAY_ORDER_CHART
*&---------------------------------------------------------------------*
*&      Form  display_fail_floc_chart
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_fail_floc_chart .

  DATA: y_values  TYPE TABLE OF gprval WITH HEADER LINE,
  x_texts        TYPE TABLE OF gprtxt WITH HEADER LINE.
  DATA ok_code LIKE sy-ucomm.
  DATA : lv_label TYPE string,
        lv_count TYPE string,
        lv_cnt TYPE i.
  FIELD-SYMBOLS <fs_dynamic> TYPE any.
  REFRESH y_values.
  REFRESH x_texts.
  lv_cnt = 1.

*  IF ta_statistics[] IS NOT INITIAL.

  LOOP AT  ta_statistics .
    IF lv_cnt <= 10." this is the maximum limit a graph can hol
      lv_count = lv_cnt.
      y_values-rowtxt = 'FUNCTIONAL LOCATION'.
*    lv_label = 'Y_VALUES-VA'.
*    lv_label = '(Y_VALUES-VAL)'+ lv_count.
      CONCATENATE 'Y_VALUES-VAL' lv_count INTO lv_label.
      lv_cnt = lv_cnt + 1.
      ASSIGN (lv_label) TO <fs_dynamic>.
      <fs_dynamic> = ta_statistics-count.
      UNASSIGN <fs_dynamic>.
      APPEND y_values.

      x_texts-coltxt = ta_statistics-field.
      APPEND x_texts.
    ENDIF.
  ENDLOOP.


  CALL FUNCTION 'GFW_PRES_SHOW'
    EXPORTING
      container         = 'CONTAINER'
      presentation_type = gfw_prestype_vertical_bars
    TABLES
      values            = y_values
      column_texts      = x_texts
    EXCEPTIONS
      error_occurred    = 1
      OTHERS            = 2.

*  ENDIF.
ENDFORM.                    " DISPLAY_ORDER_CHART

*&---------------------------------------------------------------------*
*&      Form  SUB_START_URL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_URL  text
*----------------------------------------------------------------------*
FORM sub_start_url USING p_url.

  CALL FUNCTION 'CALL_BROWSER'
    EXPORTING
      url                    = p_url
    EXCEPTIONS
      frontend_not_supported = 1
      frontend_error         = 2
      prog_not_found         = 3
      no_batch               = 4
      unspecified_error      = 5
      OTHERS                 = 6.

  IF sy-subrc <> 0.
    MESSAGE 'Can not start web browser'(004) TYPE 'I'.
  ENDIF.

ENDFORM.                    "sub_start_url

" SUB_HANDLE_CORR
*&---------------------------------------------------------------------*
*&      Form  SUB_GET_PROG_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_TCODE_IW38  text
*      <--P_L_PROG_NAME  text
*----------------------------------------------------------------------*
FORM sub_get_prog_name USING p_tcode
                       CHANGING p_cprog.

  SELECT SINGLE pgmna
       FROM tstc
       INTO p_cprog
       WHERE tcode = p_tcode.

ENDFORM.                    " SUB_GET_PROG_NAME


*&---------------------------------------------------------------------*
*&      Form  sub_submit_report
*&---------------------------------------------------------------------*
*       Submit Report Program with selection screen variant and
*       selection screen parameters
*----------------------------------------------------------------------*
*      -->P_T_SELPARAMS  Internal Table - Seltion Parameters
*      -->P_CPROG        Program Name
*      -->P_VARIANT      Selection Screen Variant
*----------------------------------------------------------------------*
FORM sub_submit_report1 TABLES p_t_selparams STRUCTURE rsparams
                       USING  p_cprog p_variant p_selscr.
  IF p_selscr IS INITIAL.
    SUBMIT (p_cprog) USING SELECTION-SET p_variant
                     WITH SELECTION-TABLE p_t_selparams
                     VIA SELECTION-SCREEN AND RETURN.
  ELSE.
    SUBMIT (p_cprog) USING SELECTION-SET p_variant
                     WITH SELECTION-TABLE p_t_selparams
                     AND RETURN.
  ENDIF.

ENDFORM.                    "s                     "sub_submit_report
*&---------------------------------------------------------------------*
*&      Form  sub_submit_report
*&---------------------------------------------------------------------*
*       Submit Report Program with selection screen variant and
*       selection screen parameters
*----------------------------------------------------------------------*
*      -->P_T_SELPARAMS  Internal Table - Seltion Parameters
*      -->P_CPROG        Program Name
*      -->P_VARIANT      Selection Screen Variant
*----------------------------------------------------------------------*
FORM sub_submit_report TABLES p_t_selparams STRUCTURE rsparams
                       USING  p_cprog p_variant p_selscr.

  IF p_selscr IS INITIAL.
    SUBMIT (p_cprog) USING SELECTION-SET p_variant
                     WITH SELECTION-TABLE p_t_selparams
                     VIA SELECTION-SCREEN AND RETURN.
  ELSE.
    SUBMIT (p_cprog) USING SELECTION-SET p_variant
                     WITH SELECTION-TABLE p_t_selparams
                     AND RETURN.
  ENDIF.

ENDFORM.                    "sub_submit_report


*&---------------------------------------------------------------------*
*&      Form  SUB_CHECK_TCODE_AUTHORITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TCODE_IW38  text
*      <--P_RETURN_CODE  text
*----------------------------------------------------------------------*
FORM sub_check_tcode_authority USING    p_tcode  TYPE sy-tcode
                               CHANGING p_return TYPE sy-subrc.

  CLEAR p_return.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = p_tcode
    EXCEPTIONS
      ok     = 1
      not_ok = 2
      OTHERS = 3.

  p_return = sy-subrc.

ENDFORM.                   " SUB_CHECK_TCODE_AUTHORITY

*&---------------------------------------------------------------------*
*&      Form  SUB_GET_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTO_DATE  text
*      <--P_LFR_DATE  text
*----------------------------------------------------------------------*
FORM sub_get_date  TABLES p_t_selparams STRUCTURE rsparams.

  DATA : l_mm     TYPE c LENGTH 2,
         l_dd     TYPE c LENGTH 2,
         l_yy     TYPE c LENGTH 4,
         lv_date  TYPE datum,
         lv_edate TYPE datum.
  CLEAR : lv_date , lv_edate.
  l_yy = sy-datum+0(4).
  l_mm = sy-datum+4(2).
  l_dd = sy-datum+6(2).
  CONCATENATE l_yy l_mm '01' INTO lv_date.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = sy-datum
    IMPORTING
      last_day_of_month = lv_edate.
*
  p_t_selparams-selname = 'GSTRP'.
  p_t_selparams-kind    = 'S'.
  p_t_selparams-sign    = 'I'.
  p_t_selparams-option  = 'BT'.
  p_t_selparams-low     = lv_date.
  p_t_selparams-high    = lv_edate.
  APPEND p_t_selparams.

  p_t_selparams-selname = 'AUART'.
  p_t_selparams-kind    = 'S'.
  p_t_selparams-sign    = 'I'.
  p_t_selparams-option  = 'EQ'.
  p_t_selparams-low     = 'PM20'.
  APPEND p_t_selparams.

  p_t_selparams-selname = 'DY_OFN'.
  p_t_selparams-kind    = 'P'.
  p_t_selparams-sign    = 'I'.
  p_t_selparams-option  = 'EQ'.
  p_t_selparams-low     = 'X'.
  p_t_selparams-high    = ''.
  APPEND p_t_selparams.

  p_t_selparams-selname = 'DY_IAR'.
  p_t_selparams-kind    = 'P'.
  p_t_selparams-sign    = 'I'.
  p_t_selparams-option  = 'EQ'.
  p_t_selparams-low     = 'X'.
  p_t_selparams-high    = ''.
  APPEND p_t_selparams.

ENDFORM.                    " SUB_GET_DATE
*&---------------------------------------------------------------------*
*&      Form  SUB_SUBMIT_IW38
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CPROG  text
*      -->P_FR_DATE  text
*      -->P_TO_DATE  text
*----------------------------------------------------------------------*
FORM sub_submit_iw38  TABLES p1_t_selparams USING p_cprog.
  RANGES lt_termab FOR rihea-termab.


  SUBMIT (p_cprog) WITH SELECTION-TABLE p1_t_selparams[]
                   AND RETURN.

ENDFORM.                    " SUB_SUBMIT_IW38

*&---------------------------------------------------------------------*
*&      Form  SUB_SUBMIT_MCI7
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_SELPARAMS  text
*      -->P_L_PROG_NAME  text
*----------------------------------------------------------------------*
FORM sub_submit_mci7  TABLES   p1_t_selparams STRUCTURE rsparams
                      USING    p_l_cprog.

  SUBMIT (p_l_cprog) WITH SELECTION-TABLE p1_t_selparams[]
                                AND RETURN.

ENDFORM.                    "SUB_SUBMIT_MCI7

*&---------------------------------------------------------------------*
*&      Form  SUB_READ_DEFAULT_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_read_default_values .

  DATA : l_key TYPE indx-srtfd.
  SELECT SINGLE *
         INTO CORRESPONDING FIELDS
         OF x_nt_defaults
         FROM z06pmt_cp_deflt
         WHERE uname = sy-uname.

  IF sy-subrc = 0.
    x_defaults = x_nt_defaults.
  ENDIF.

  CONCATENATE sy-cprog
              sy-uname
              INTO l_key.

*{   REPLACE        D30K924358                                        1
*\*  IMPORT s_arbpl
*\*         s_strno
*\*         FROM DATABASE indx(nt)
*\*         ID l_key.
  IMPORT s_arbpl
         s_strno
         s_eqn
         s_date
         FROM DATABASE indx(nt)
         ID l_key.
*}   REPLACE

  r_arbpl[] = s_arbpl[].
  r_strno[] = s_strno[].
  r_equnr[] = s_eqn[].
*{   INSERT         D30K924358                                        2
  r_date[] = s_date[].
*}   INSERT

ENDFORM.                    " SUB_READ_DEFAULT_VALUES

*&---------------------------------------------------------------------*
*&      Form  SUB_POPULATE_SELTAB
*&---------------------------------------------------------------------*
*       Fill selection table for program submit from Defaults settings
*----------------------------------------------------------------------*

FORM sub_populate_seltab USING p_s_mwctr p_s_sysno
                               p_s_ortyp p_s_nttyp
                               p_p_parnr.

  DATA : o_ref_type TYPE REF TO cl_abap_structdescr,
         l_pernr    TYPE pa0000-pernr.

  FIELD-SYMBOLS : <fs_comp_line>,
                  <fs_name>,
                  <fs_value>.

  CLEAR t_selparams.

  IF p_s_mwctr IS NOT INITIAL AND s_arbpl[] IS NOT INITIAL.

    LOOP AT s_arbpl.
      CLEAR t_selparams.
      MOVE-CORRESPONDING s_arbpl TO t_selparams.
      t_selparams-selname = p_s_mwctr.
      t_selparams-kind    = 'S'.
      APPEND t_selparams.
    ENDLOOP.

  ENDIF.

  IF p_s_nttyp IS NOT INITIAL.

    CLEAR t_selparams.
    t_selparams-selname = 'QMART'.
    t_selparams-kind    = 'S'.
    t_selparams-sign    = 'I'.

    IF x_defaults-notif IS NOT INITIAL.
      o_ref_type ?= cl_abap_structdescr=>describe_by_data( x_defaults-notif ).
      t_selparams-option = 'EQ'.
      LOOP AT o_ref_type->components ASSIGNING <fs_comp_line>.
        ASSIGN COMPONENT 'NAME' OF STRUCTURE <fs_comp_line>
                                TO <fs_name>.
        ASSIGN COMPONENT <fs_name> OF STRUCTURE x_defaults
                                TO <fs_value>.

        IF <fs_value> = 'X'.
          t_selparams-low = <fs_name>+7(2).
          APPEND t_selparams.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDIF.

  IF p_s_ortyp IS NOT INITIAL.

    CLEAR t_selparams.
    t_selparams-selname = 'AUART'.
    t_selparams-kind    = 'S'.
    t_selparams-sign    = 'I'.

    IF x_defaults-order IS NOT INITIAL.
      o_ref_type ?= cl_abap_structdescr=>describe_by_data( x_defaults-order ).

      t_selparams-option = 'EQ'.
      LOOP AT o_ref_type->components ASSIGNING <fs_comp_line>.
        ASSIGN COMPONENT 'NAME' OF STRUCTURE <fs_comp_line>
                                TO <fs_name>.
        ASSIGN COMPONENT <fs_name> OF STRUCTURE x_defaults
                                TO <fs_value>.

        IF <fs_value> = 'X'.
          t_selparams-low = <fs_name>+6(4).
          APPEND t_selparams.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF p_s_sysno IS NOT INITIAL.
    IF s_strno[] IS NOT INITIAL.
      t_selparams-selname = p_s_sysno.
      t_selparams-kind    = 'S'.
      LOOP AT s_strno.

        t_selparams-sign    = s_strno-sign.

        IF s_strno-low IS NOT INITIAL.
          t_selparams-option  = 'CP'.
          CONCATENATE s_strno-low c_asterisk
                      INTO t_selparams-low.
        ENDIF.

        IF s_strno-high IS NOT INITIAL.
          t_selparams-option  = 'BT'.
          CONCATENATE s_strno-high c_asterisk
                      INTO t_selparams-high.
        ENDIF.

        PERFORM sub_determine_func_loc CHANGING t_selparams.

      ENDLOOP.

    ELSE.

      t_selparams-sign    = 'I'.
      t_selparams-option  = 'CP'.
      t_selparams-selname = p_s_sysno.
      t_selparams-kind    = 'S'.

*      IF x_defaults-zzunit1 IS NOT INITIAL.
*        t_selparams-low     = 'N1-*'.
*        APPEND t_selparams.
*      ENDIF.
*
*      IF x_defaults-zzunit2 IS NOT INITIAL.
*        t_selparams-low     = 'N2-*'.
*        APPEND t_selparams.
*      ENDIF.
*
*      IF  x_defaults-zzunit1 IS INITIAL
*      AND x_defaults-zzunit2 IS INITIAL.
*        t_selparams-low     = 'N*-*'.
*        APPEND t_selparams.
*      ENDIF.
    ENDIF.

  ENDIF.

*  IF p_p_parnr IS NOT INITIAL.
*    CLEAR t_selparams.
*    t_selparams-selname = p_p_parnr.
*    t_selparams-kind    = 'S'.
*    t_selparams-sign    = 'I'.
*    t_selparams-option  = 'EQ'.
*
*    PERFORM sub_get_employee USING    sy-uname
*                             CHANGING l_pernr.
*
*    t_selparams-low     = l_pernr.
*    APPEND t_selparams.
*  ENDIF.



ENDFORM.                    "sub_populate_seltab                    " SUB_POPULATE_SELTAB
" SUB_SUBMIT_MCI7

*&---------------------------------------------------------------------*
*&      Form  SUB_START_URL_SSO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_CONFIG_LINKPATH  text
*----------------------------------------------------------------------*
FORM sub_start_url_sso  USING p_url_sso.
  CALL FUNCTION 'Z_LAUNCHURL_MYSAPSSO'
    EXPORTING
      p_url = p_url_sso.
  IF sy-subrc <> 0.
    MESSAGE 'Can not start web browser'(004) TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_START_URL_SSO
