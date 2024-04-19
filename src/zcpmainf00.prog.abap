*&---------------------------------------------------------------------*
*&  Include           ZCPMAINF01
*&---------------------------------------------------------------------*
* 3/12/2015 NR When there are no orders selected, do not show [*] to the user

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

ENDFORM.                    "sub_check_tcode_authority
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
*&      Form  SUB_DISPLAY_LOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_display_logo .

*- data for picture control *-
  DATA:l_picture_control TYPE REF TO cl_gui_picture,
       l_pic_container TYPE REF TO cl_gui_custom_container,
       l_graphic_url(255),
       l_graphic_size TYPE i,
       l_stxbmaps TYPE stxbitmaps,
       l_bytecnt TYPE i,
       l_content TYPE STANDARD TABLE OF bapiconten INITIAL SIZE 0,
       l_graphic_xstr TYPE xstring,
       l_graphic_conv TYPE i,
       l_graphic_offs TYPE i.

*- custom container to display image *-
  DATA:l_container TYPE scrfname VALUE 'LOGO_9000_CONT'. "custom container defined in screen

*- image table type *-
  TYPES: BEGIN OF ty_graphic_table,
          line(255) TYPE x,
        END OF ty_graphic_table.

  CONSTANTS: c_sap_tab_unknown(9)   TYPE c VALUE 'X-UNKNOWN',
             c_lifetime_transaction TYPE c VALUE 'T'.

  DATA: t_pict_tab TYPE TABLE OF ty_graphic_table,          "image table
        x_pict_tab TYPE ty_graphic_table,
        l_pic_name TYPE tdobname,
        l_url      TYPE char255.

  CHECK zntlogo-logo IS NOT INITIAL.
  l_pic_name = zntlogo-logo.

*- GET BMP image *-
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object  = 'GRAPHICS'
      p_name    = l_pic_name
      p_id      = 'BMAP'
      p_btype   = 'BCOL' "(BMON = black&white, BCOL = colour)
    RECEIVING
      p_bmp     = l_graphic_xstr
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  l_graphic_size = xstrlen( l_graphic_xstr ).
  CHECK l_graphic_size > 0.                  "check whether image exists

  l_graphic_conv = l_graphic_size.
  l_graphic_offs = 0.

*- prepare the image as data table *-
  WHILE l_graphic_conv > 255.
    x_pict_tab-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND x_pict_tab TO t_pict_tab.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.

*- append rest of the lines of the image *-
  x_pict_tab-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND x_pict_tab TO t_pict_tab .
  CLEAR x_pict_tab.

*- create URL for the image *-
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = c_sap_tab_unknown " 'X-UNKNOWN'
      size     = l_graphic_size
      lifetime = c_lifetime_transaction "'T'
    TABLES
      data     = t_pict_tab
    CHANGING
      url      = l_url.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.


  IF l_pic_container IS INITIAL.

*- create picture container *-
    CREATE OBJECT l_pic_container
      EXPORTING
        container_name = l_container.

*- create picture control *-
    CREATE OBJECT l_picture_control
      EXPORTING
        parent = l_pic_container.

*- display image from URL *-
    CALL METHOD l_picture_control->load_picture_from_url_async
      EXPORTING
        url = l_url.

*-- Set the mode of display
    CALL METHOD l_picture_control->set_display_mode
      EXPORTING
        display_mode = cl_gui_picture=>display_mode_fit.

  ENDIF.
ENDFORM.                    " SUB_DISPLAY_LOGO
*&---------------------------------------------------------------------*
*&      Form  SUB_NAVIGATE_SAP_INBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sub_navigate_sap_inbox .

  DATA : l_ret_code TYPE sy-subrc.
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
    PERFORM sub_check_tcode_authority USING    t_config-tcode  " SBWP
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003. " You are not authorized to use this function
      RETURN.
    ENDIF.
    PERFORM sub_call_tcode USING t_config-tcode    " SBWP
                                 space.
  ELSE.
    MESSAGE 'Tcode is not exist in the table ZNTCONFIG' TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_NAVIGATE_SAP_INBOX
*&---------------------------------------------------------------------*
*&      Form  SUB_CALL_TCODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TCODE  Transaction Code
*      -->P_LEAVE  Flag : Levent to Transaction Code
*----------------------------------------------------------------------*
FORM sub_call_tcode  USING p_tcode TYPE sy-tcode
                           p_leave TYPE char1.

  IF p_leave IS NOT INITIAL.
    LEAVE TO TRANSACTION p_tcode.
  ELSE.
    CALL TRANSACTION p_tcode.
  ENDIF.

ENDFORM.                    " SUB_CALL_TCODE
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
    MESSAGE i000 WITH 'Can not start web browser'.
  ENDIF.

ENDFORM.                    " SUB_START_URL
*&---------------------------------------------------------------------*
*&      Form  SUB_READ_DEFAULT_VALUES
*&---------------------------------------------------------------------*
*       Read Default Order Type, Notification Type, Work Centre
*       and System Number settings
*----------------------------------------------------------------------*
FORM sub_read_default_values .

  DATA : l_key TYPE indx-srtfd.
  SELECT SINGLE *
         INTO CORRESPONDING FIELDS
         OF x_nt_defaults
         FROM zntdeflt
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
         s_date
         FROM DATABASE indx(nt)
         ID l_key.
*}   REPLACE

  r_arbpl[] = s_arbpl[].
*{   INSERT         D30K924358                                        2
  r_date[]  = s_date[].
*}   INSERT
  r_strno[] = s_strno[].

ENDFORM.                    " SUB_READ_DEFAULT_VALUES
*&---------------------------------------------------------------------*
*&      Form  SUB_SAVE_DEFAULTS
*&---------------------------------------------------------------------*
*       Save Default Order Type, Notification Type, Work Centre
*       and System Number settings
*----------------------------------------------------------------------*
FORM sub_save_defaults .

  DATA : x_zntdeflt TYPE zntdeflt,
         l_key      TYPE indx-srtfd.

  IF x_defaults <> x_nt_defaults.
    MOVE-CORRESPONDING x_defaults TO x_zntdeflt.
    x_zntdeflt-uname = sy-uname.

    MODIFY zntdeflt FROM x_zntdeflt.
    IF sy-subrc = 0..
      COMMIT WORK.
    ENDIF.
    x_nt_defaults = x_defaults.
  ENDIF.

*{   DELETE         D30K924358                                        1
*\  IF s_arbpl[] <> r_arbpl[] OR s_strno[] <> r_strno[].
*}   DELETE
*{   INSERT         D30K924358                                        2
  IF s_arbpl[] <> r_arbpl[] OR s_strno[] <> r_strno[] OR s_date[] <> r_date[].
*}   INSERT
    CONCATENATE sy-cprog
                sy-uname
                INTO l_key.
*{   REPLACE        D30K924358                                        3
*\*    EXPORT s_arbpl
*\*           s_strno
*\*           TO DATABASE indx(nt) ID l_key.
    EXPORT s_arbpl
           s_strno
           s_date
           TO DATABASE indx(nt) ID l_key.
*}   REPLACE
    r_arbpl[] = s_arbpl[].
    r_strno[] = s_strno[].
*{   INSERT         D30K924358                                        4
    r_date[] = s_date[].
*}   INSERT
  ENDIF.

ENDFORM.                    " SUB_SAVE_DEFAULTS
*&---------------------------------------------------------------------*
*&      Form  sub_populate_seltab
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

  REFRESH t_selparams. CLEAR t_selparams.

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
    t_selparams-selname = p_s_nttyp.
    t_selparams-kind    = 'S'.
    t_selparams-sign    = 'I'.

    IF x_defaults-notif IS INITIAL.
*-- When there are no orders selected, do not show [*] to the user
*      t_selparams-option  = 'CP'.
*      t_selparams-low     = c_pattern_n.
*      APPEND t_selparams.
    ELSE.

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
    t_selparams-selname = p_s_ortyp.
    t_selparams-kind    = 'S'.
    t_selparams-sign    = 'I'.

    IF x_defaults-order IS INITIAL.
*-- When there are no orders selected, do not show [*] to the user
*      t_selparams-option  = 'CP'.
*      t_selparams-low     = c_pattern_n.
*      APPEND t_selparams.
    ELSE.

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
*{   DELETE         D30K925363                                        1
*\      t_selparams-kind    = 'S'.
*}   DELETE
      LOOP AT s_strno.

        t_selparams-sign    = s_strno-sign.
        t_selparams-low = s_strno-low.
        t_selparams-high = s_strno-high.
        t_selparams-option = s_strno-option.

*        t_selparams-sign    = s_strno-sign.
*
*        IF s_strno-low IS NOT INITIAL.
**{   INSERT         D30K925363                                        2
*          t_selparams-kind = 'P'.
**}   INSERT
*          t_selparams-option  = 'CP'.
*          CONCATENATE s_strno-low c_asterisk
*                      INTO t_selparams-low.
*        ENDIF.
*
*        IF s_strno-high IS NOT INITIAL.
**{   INSERT         D30K925363                                        3
*          t_selparams-kind = 'S'.
**}   INSERT
*          t_selparams-option  = 'BT'.
*          CONCATENATE s_strno-high c_asterisk
*                      INTO t_selparams-high.
*        ENDIF.

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

ENDFORM.                    "sub_populate_seltab
*&---------------------------------------------------------------------*
*&      Form  SUB_DISPLAY_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DOKNAME  Dialog Text - Name
*----------------------------------------------------------------------*
FORM sub_display_info USING p_dokname.
  TYPES: BEGIN OF ty_doc,
          col1(30) TYPE c,
          col2(30) TYPE c,
          col3(30) TYPE c,
          col4(30) TYPE c,
          col5(30) TYPE c,
          col6(30) TYPE c,
        END OF ty_doc.

  DATA:l_header TYPE thead,
       t_itf TYPE TABLE OF tline,
       x_itf TYPE tline,
       t_data TYPE TABLE OF string WITH HEADER LINE,
       t_doc_tab TYPE TABLE OF ty_doc,
       x_doc TYPE ty_doc,
       l_cnt TYPE i,
       l_cnt1 TYPE i.


*- get Dialog Text in ITF format *-
  CALL FUNCTION 'DOC_OBJECT_GET'
    EXPORTING
      class            = 'DT'
      name             = p_dokname
    IMPORTING
      header           = l_header
    TABLES
      itf_lines        = t_itf
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  REFRESH: t_heading,t_footer,gt_events.
  CLEAR: x_heading,x_footer.

*- format ITF data in table and header *-
  LOOP AT t_itf INTO x_itf.

    REFRESH t_data.
    SPLIT x_itf-tdline AT ',' INTO TABLE t_data.
*- table lines *-
    IF x_itf-tdformat = 'K4'.
      LOOP AT t_data.
        l_cnt = l_cnt + 1.
        CASE l_cnt.
          WHEN 1.
            x_doc-col1 = t_data.
          WHEN 2.
            x_doc-col2 = t_data.
          WHEN 3.
            x_doc-col3 = t_data.
          WHEN 4.
            x_doc-col4 = t_data.
          WHEN 5.
            x_doc-col5 = t_data.
          WHEN 6.
            x_doc-col6 = t_data.
        ENDCASE.
      ENDLOOP.
      APPEND x_doc TO t_doc_tab.
      CLEAR: x_doc,l_cnt.

*- table header lines *-
    ELSEIF x_itf-tdformat = 'U2'.
      LOOP AT t_data.
        l_cnt1 = l_cnt1 + 1.
        CASE l_cnt1.
          WHEN 1.
            l_titl1 = t_data.
          WHEN 2.
            l_titl2 = t_data.
          WHEN 3.
            l_titl3 = t_data.
          WHEN 4.
            l_titl4 = t_data.
          WHEN 5.
            l_titl5 = t_data.
          WHEN 6.
            l_titl6 = t_data.
        ENDCASE.
      ENDLOOP.
*- any other lines *-
    ELSEIF x_itf-tdformat = 'U3'.
      MOVE x_itf-tdline TO x_footer-tdline.
      APPEND x_footer TO t_footer.
    ELSE.
      MOVE x_itf-tdline TO x_heading-tdline.
      APPEND x_heading TO t_heading.
    ENDIF.
  ENDLOOP.

*- If Dialog Text is not for Deafult section *-
*- display the ALV list *-

  IF p_dokname <> c_doc_defaults.
    PERFORM sub_build_field_cat.
*    PERFORM sub_gen_top.             "for grid display
    PERFORM sub_build_events.         "for top of page for list display
    PERFORM sub_exclude_fcodes.       "exclude unwanted buttons
    i_layout-colwidth_optimize  = 'X'.


*    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*      EXPORTING
*        i_callback_program     = sy-repid
*        i_callback_top_of_page = 'HEADING_TOP_OF_PAGE'
*        is_layout              = i_layout
*        it_fieldcat            = i_fieldtab
**      i_grid_title           = 'Info'
*        i_screen_start_column  = 1
*        i_screen_start_line    = 1
*        i_screen_end_column    = 70
*        i_screen_end_line      = 20
*      TABLES
*        t_outtab               = t_doc_tab
*      EXCEPTIONS
*        program_error          = 1
*        OTHERS                 = 2.
*    IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.

*- all other section help list except default *-
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program    = sy-repid
        is_layout             = i_layout
        it_fieldcat           = i_fieldtab
        it_events             = gt_events
        it_excluding          = t_excld
        i_screen_start_column = 1
        i_screen_start_line   = 1
        i_screen_end_column   = 75
        i_screen_end_line     = 25
      TABLES
        t_outtab              = t_doc_tab
      EXCEPTIONS
        program_error         = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.
    EXPORT t_itf TO MEMORY ID 'ITF'.      "Export to program ydefaultdoc
*    SUBMIT ydefaultdoc AND RETURN.

    CLEAR i_layout.
    PERFORM sub_build_field_cat.
    REFRESH t_doc_tab.
    PERFORM sub_build_events_def.
    PERFORM sub_exclude_fcodes.

*- default section help list *-
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program    = sy-repid
        is_layout             = i_layout
        it_fieldcat           = i_fieldtab
        it_events             = gt_events
        it_excluding          = t_excld
        i_screen_start_column = 1
        i_screen_start_line   = 1
        i_screen_end_column   = 80
        i_screen_end_line     = 70
      TABLES
        t_outtab              = t_doc_tab
      EXCEPTIONS
        program_error         = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*    CALL FUNCTION 'ZDEFINFODIALOG'
*      TABLES
*        t_itf         = t_itf
*              .

  ENDIF.


*    CALL FUNCTION 'DSYS_SHOW_FOR_F1HELP'
*      EXPORTING
*        dokclass         = 'DT'
*        dokname          = p_dokname
**- start of change Rahul 10/09/2009 *-
*        "short_text       = 'X'
**- end of change Rahul *-
*      EXCEPTIONS
*        class_unknown    = 1
*        object_not_found = 2
*        OTHERS           = 3.
*
*    IF sy-subrc <> 0.
*      MESSAGE i000 WITH 'Can not display information'.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " SUB_DISPLAY_INFO
*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_F4_ON_STRNO
*&---------------------------------------------------------------------*
*       Handle F4 help request on Selection Screen field System Number
*----------------------------------------------------------------------*
FORM sub_handle_f4_on_strno .

  DATA : l_field       TYPE dynfnam,
         l_unit        TYPE char2,
         l_system      TYPE char2,
         l_t_floc      TYPE STANDARD TABLE OF ty_floc WITH HEADER LINE,
         l_t_floc_help TYPE STANDARD TABLE OF ty_floc WITH HEADER LINE,
         l_s_floc_help TYPE ty_floc,
         l_len         TYPE i,
         t_ret_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
         l_lines       TYPE i.

*- get functional locations and their descriptions *-
  SELECT tplnr
         pltxt
         FROM iflm_view
         INTO TABLE l_t_floc
         WHERE iwerk = c_pl_plant_9000
         AND   spras = sy-langu.

  IF sy-subrc <> 0.
    MESSAGE s000 WITH text-008. " No Functional Locations defined
    RETURN.
  ENDIF.

*- prepare the search help table *-
  LOOP AT l_t_floc.
    CLEAR l_t_floc_help.
*- convert functional location to external format *-
    CALL FUNCTION 'CONVERSION_EXIT_TPLNR_OUTPUT'
      EXPORTING
        input  = l_t_floc-tplnr
      IMPORTING
        output = l_s_floc_help-tplnr.

*- check if length of functional location > 5 *-
*- i.e functional locations is not of the form e.g N1-04 *-
    l_len = strlen( l_s_floc_help-tplnr ).
    IF l_len > 5.
      CONTINUE.
    ENDIF.

    l_t_floc_help-tplnr = l_s_floc_help-tplnr+3(2).

    SPLIT l_s_floc_help-tplnr
          AT '-'
          INTO l_unit l_system.

    CONCATENATE l_system
                '-'
                l_t_floc-pltxt
                INTO l_t_floc_help-pltxt
                SEPARATED BY space.

    APPEND l_t_floc_help.

  ENDLOOP.

  SORT l_t_floc_help BY tplnr.
  DELETE ADJACENT DUPLICATES FROM l_t_floc_help COMPARING tplnr.

*- get the name of the field for which help is sought *-
  GET CURSOR FIELD l_field.

*- if help is sought for Low value of the select option system no *-
*- call help display tab with multiple selection facilty *-
  IF l_field = 'S_STRNO-LOW'.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'TPLNR'
        value_org       = 'S'
        multiple_choice = 'X'
        window_title    = 'System'
      TABLES
        value_tab       = l_t_floc_help
        return_tab      = t_ret_tab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

*- Problem in generating F4 help,notify the user *-
    IF sy-subrc <> 0.
      MESSAGE s000 WITH text-009.  " Can not generate F4 help
    ENDIF.

    s_strno-sign   = 'I'.
    s_strno-option = 'EQ'.

*- append the values selected into the select option *-
*- start of change by Rahul 10/09/2009*-
    CLEAR l_lines.
    DESCRIBE TABLE t_ret_tab LINES l_lines.
    IF l_lines > 1.
*- end of change by Rahul *-
      LOOP AT t_ret_tab.
        READ TABLE s_strno WITH KEY low = t_ret_tab-fieldval+0(2).
        IF sy-subrc NE 0.
          s_strno-low = t_ret_tab-fieldval+0(2).
          APPEND s_strno.
        ENDIF.
      ENDLOOP.
*      READ TABLE s_strno INDEX 1.
*- start of change by Rahul 10/09/2009*-
    ELSEIF l_lines = 1.
      READ TABLE t_ret_tab INDEX 1.
      IF sy-subrc = 0.
        s_strno-low = t_ret_tab-fieldval+0(2).
        MODIFY s_strno INDEX 1.
      ENDIF.
    ENDIF.


  ELSE.

*- help value is sought for high value of system no *-
*- check if user has already selected multiple values *-
*- if yes restrict user from entering any value in high field *-
*    DESCRIBE TABLE s_strno LINES l_lines.
*    IF l_lines <= 1.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'TPLNR'
        dynpprog        = sy-cprog
        dynpnr          = '9100'
        dynprofield     = l_field
        value_org       = 'S'
        window_title    = 'System'
      TABLES
        value_tab       = l_t_floc_help
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
*- Problem in generating F4 help,notify the user *-
    IF sy-subrc <> 0.
      MESSAGE s000 WITH text-009.  " Can not generate F4 help
    ENDIF.
*    ELSE.
**- notify the user that he cant select high value as already *-
**- are selected *-
*      MESSAGE i000 WITH 'Multiple values are already selected'(010).
*    ENDIF.
  ENDIF.
  SORT s_strno BY low ASCENDING.
  READ TABLE s_strno INDEX 1.
ENDFORM.                    " SUB_HANDLE_F4_ON_STRNO
*&---------------------------------------------------------------------*
*&      Form  SUB_GET_PROG_NAME
*&---------------------------------------------------------------------*
*       Get program name attached to a T-Code
*----------------------------------------------------------------------*
*      -->P_TCODE  Transaction Code
*      <--P_CPROG  Attached program name
*----------------------------------------------------------------------*
FORM sub_get_prog_name  USING    p_tcode TYPE sy-tcode
                        CHANGING p_cprog.

  SELECT SINGLE pgmna
         FROM tstc
         INTO p_cprog
         WHERE tcode = p_tcode.

ENDFORM.                    " SUB_GET_PROG_NAME
*&---------------------------------------------------------------------*
*&      Form  SUB_BDC_DYNPRO
*&---------------------------------------------------------------------*
*       Populate Batch Data screen information
*----------------------------------------------------------------------*
*      -->P_PROG    Program
*      -->P_DYNNR   Screen Number
*----------------------------------------------------------------------*
FORM sub_bdc_dynpro USING p_prog p_dynnr.
  CLEAR t_bdcdata.
  t_bdcdata-program  = p_prog.
  t_bdcdata-dynpro   = p_dynnr.
  t_bdcdata-dynbegin = 'X'.
  APPEND t_bdcdata.
ENDFORM.                    " SUB_BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  SUB_BDC_FIELD
*&---------------------------------------------------------------------*
*       Populate batch data field information
*----------------------------------------------------------------------*
*      -->P_FNAM   Field Name
*      -->P_FVAL   Field Value
*----------------------------------------------------------------------*
FORM sub_bdc_field  USING p_fnam p_fval.
  CLEAR t_bdcdata.
  t_bdcdata-fnam = p_fnam.
  t_bdcdata-fval = p_fval.
  APPEND t_bdcdata.
ENDFORM.                    " SUB_BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  SUB_DETERMINE_FUNC_LOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_SELPARAMS_LOW  text
*----------------------------------------------------------------------*
FORM sub_determine_func_loc CHANGING p_x_selparams TYPE rsparams.

  DATA : l_x_selparams TYPE rsparams.

  l_x_selparams = p_x_selparams.
  APPEND l_x_selparams TO t_selparams.

* Commendted rest of code by Eldhose Mathew for issue reported by Ron when
* he trying to execute change notifications, he sees a M* Concatenated
* to the functional location selection field
*
*  IF  x_defaults-zzunit1 IS INITIAL
*  AND x_defaults-zzunit2 IS INITIAL.
*    CONCATENATE c_n
*                c_asterisk
*                c_hyphen
*                p_x_selparams-low
*                INTO p_x_selparams-low.
*
*    IF p_x_selparams-high IS NOT INITIAL.
*      CONCATENATE c_n
*                  c_asterisk
*                  c_hyphen
*                  p_x_selparams-high
*                  INTO p_x_selparams-high.
*    ENDIF.
*
*    APPEND p_x_selparams TO t_selparams.
*
*  ELSE.
*
*    IF x_defaults-zzunit1 IS NOT INITIAL.
*      CONCATENATE c_n
*                  c_1
*                  c_hyphen
*                  p_x_selparams-low
*                  INTO l_x_selparams-low.
*
*      IF p_x_selparams-high IS NOT INITIAL.
*        CONCATENATE c_n
*                    c_1
*                    c_hyphen
*                    p_x_selparams-high
*                    INTO l_x_selparams-high.
*      ENDIF.
*
*      APPEND l_x_selparams TO t_selparams.
*    ENDIF.
*
*    IF x_defaults-zzunit2 IS NOT INITIAL.
*      CONCATENATE c_n
*                  c_2
*                  c_hyphen
*                  p_x_selparams-low
*                  INTO l_x_selparams-low.
*
*      IF p_x_selparams-high IS NOT INITIAL.
*        CONCATENATE c_n
*                    c_2
*                    c_hyphen
*                    p_x_selparams-high
*                    INTO l_x_selparams-high.
*      ENDIF.
*
*      APPEND l_x_selparams TO t_selparams.
*
*    ENDIF.
*
*  ENDIF.
ENDFORM.                    " SUB_DETERMINE_FUNC_LOC

*&---------------------------------------------------------------------*
*&      Form  HEADING_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM heading_top_of_page.
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*        it_list_commentary = gt_list_top_of_page.
  LOOP AT t_heading INTO x_heading.
    IF sy-tabix = 1.
      WRITE: x_heading-tdline INTENSIFIED ON.
    ELSE.
      WRITE:/, x_heading-tdline.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "HEADING_TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  sub_gen_top
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sub_gen_top .

  DATA: info(60).
  CLEAR  gt_list_top_of_page[].

  LOOP AT t_heading INTO x_heading.
    CLEAR ls_line.
    IF sy-tabix = 1.
      ls_line-typ = 'H'.
    ELSE.
      ls_line-typ = 'S'.
    ENDIF.
    ls_line-info = x_heading-tdline.
    APPEND ls_line TO gt_list_top_of_page.
  ENDLOOP.

ENDFORM.                    "sub_gen_top
*&---------------------------------------------------------------------*
*&      Form  SUB_BUILD_FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_build_field_cat .

  CLEAR : s_fieldtab,
              i_fieldtab[].

  s_fieldtab-fieldname = 'COL1'.
  s_fieldtab-seltext_l = l_titl1.
  APPEND s_fieldtab TO i_fieldtab.
  CLEAR s_fieldtab.

  s_fieldtab-fieldname = 'COL2'.
*  s_fieldtab-seltext_l = 'Button Name'.
  s_fieldtab-seltext_l = l_titl2.
  APPEND s_fieldtab TO i_fieldtab.
  CLEAR s_fieldtab.

  s_fieldtab-fieldname = 'COL3'.
*  s_fieldtab-seltext_l = 'Main Work Center'.
  s_fieldtab-seltext_l = l_titl3.
  APPEND s_fieldtab TO i_fieldtab.
  CLEAR s_fieldtab.

  s_fieldtab-fieldname = 'COL4'.
*  s_fieldtab-seltext_l = 'System'.
  s_fieldtab-seltext_l = l_titl4.
  APPEND s_fieldtab TO i_fieldtab.
  CLEAR s_fieldtab.

  s_fieldtab-fieldname = 'COL5'.
*  s_fieldtab-seltext_l = 'Unit'.
  s_fieldtab-seltext_l = l_titl5.
  APPEND s_fieldtab TO i_fieldtab.
  CLEAR s_fieldtab.

  s_fieldtab-fieldname = 'COL6'.
*  s_fieldtab-seltext_l = 'Notif Type'.
  s_fieldtab-seltext_l = l_titl6.
  APPEND s_fieldtab TO i_fieldtab.
  CLEAR s_fieldtab.
ENDFORM.                    " SUB_BUILD_FIELD_CAT
*&---------------------------------------------------------------------*
*&      Form  SUB_BUILD_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_build_events .

  DATA: l_i_event TYPE slis_alv_event.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = gt_events.
  READ TABLE gt_events WITH KEY name = slis_ev_top_of_page
                           INTO l_i_event.
  IF sy-subrc = 0.
    MOVE top_of_page TO l_i_event-form.
    APPEND l_i_event TO gt_events.
  ENDIF.

  READ TABLE gt_events WITH KEY name = slis_ev_end_of_list
                           INTO l_i_event.
  IF sy-subrc = 0.
    MOVE end_of_page TO l_i_event-form.
    APPEND l_i_event TO gt_events.
  ENDIF.
ENDFORM.                    " SUB_BUILD_EVENTS
*&---------------------------------------------------------------------*
*&      Form  SUB_EXCLUDE_FCODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_exclude_fcodes .
  x_excld-fcode = '&ETA'.
  APPEND x_excld TO t_excld.
  x_excld-fcode = '%SC'.
  APPEND x_excld TO t_excld.
  x_excld-fcode = '&RNT'.
  APPEND x_excld TO t_excld.
  x_excld-fcode = '%SC+'.
  APPEND x_excld TO t_excld.
  x_excld-fcode = '&LFO'.
  APPEND x_excld TO t_excld.
  x_excld-fcode = '&CRB'.
  APPEND x_excld TO t_excld.
  x_excld-fcode = '&CRE'.
  APPEND x_excld TO t_excld.
  x_excld-fcode = '&OUP'.
  APPEND x_excld TO t_excld.
  x_excld-fcode = '&OL0'.
  APPEND x_excld TO t_excld.
  x_excld-fcode = '&ODN'.
  APPEND x_excld TO t_excld.
  x_excld-fcode = '&CRL'.
  APPEND x_excld TO t_excld.
  x_excld-fcode = '&CRR'.
  APPEND x_excld TO t_excld.
  x_excld-fcode = '&ILT'.
  APPEND x_excld TO t_excld.
  x_excld-fcode = '&ELP'.
  APPEND x_excld TO t_excld.
ENDFORM.                    " SUB_EXCLUDE_FCODES
*&---------------------------------------------------------------------*
*&      Form  SUB_BUILD_EVENTS_DEF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_build_events_def .
  DATA: l_i_event TYPE slis_alv_event.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = gt_events.
  READ TABLE gt_events WITH KEY name = slis_ev_top_of_page
                           INTO l_i_event.
  IF sy-subrc = 0.
    MOVE top_of_page_def TO l_i_event-form.
    APPEND l_i_event TO gt_events.
  ENDIF.
ENDFORM.                    " SUB_BUILD_EVENTS_DEF

*&---------------------------------------------------------------------*
*&      Form  def_top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM def_top_of_page.
  DATA:t_itf TYPE TABLE OF tline,
       x_itf TYPE tline,
       l_data1(5) TYPE c,
       l_data2(50) TYPE c.

  REFRESH t_itf.
  IMPORT t_itf FROM MEMORY ID 'ITF'.
  FREE MEMORY ID 'ITF'.
  LOOP AT t_itf INTO x_itf.
    IF x_itf-tdformat = 'K4'.
      SPLIT x_itf-tdline AT '-' INTO l_data1 l_data2.
      CONDENSE:l_data1 NO-GAPS.
      SHIFT l_data2 LEFT DELETING LEADING space.
      WRITE:/, l_data1 INTENSIFIED ON COLOR 2, ' - ' , l_data2.
    ELSE.
      WRITE:/, x_itf-tdline.
    ENDIF.
  ENDLOOP.
  WRITE:/,/,/,/,/,/,/,/.
ENDFORM.                    "def_top_of_page

*&---------------------------------------------------------------------*
*&      Form  footer_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM footer_of_page.
  LOOP AT t_footer INTO x_footer.
    WRITE:/, x_footer-tdline INTENSIFIED ON.
  ENDLOOP.
ENDFORM.                    "footer_of_page
