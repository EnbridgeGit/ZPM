*&---------------------------------------------------------------------*
*& Report  Z_PM_MEAS_POINT_EXTRACT
*& Author: Nageshwar Reddy
*&---------------------------------------------------------------------*
*& Purpose: Extract Measuring Point list for conversion activity
*&          This program is expected to be used only during conversion
*&          This program was written to avoid cumbersome manual process
*& Date: 23-Feb-2015
*&---------------------------------------------------------------------*

REPORT  z_pm_meas_point_extract.

TABLES: v_equi.

TYPES: BEGIN OF ty_vequi,
        equnr TYPE equnr,
        objnr TYPE j_objnr,
        datbi TYPE datbi,
        iwerk TYPE iwerk,
        tplnr TYPE tplnr,
        equ_mp TYPE point,
        floc_mp TYPE point,
      END OF ty_vequi,
      tt_vequi TYPE STANDARD TABLE OF ty_vequi,
      BEGIN OF ty_iflot,
        tplnr TYPE tplnr,
        objnr TYPE j_objnr,
      END OF ty_iflot,
      tt_iflot TYPE STANDARD TABLE OF ty_iflot,
      BEGIN OF ty_mpobj ,
        mpobj TYPE imrc_mpobj,
      END OF ty_mpobj,
      tt_mpobj TYPE STANDARD TABLE OF ty_mpobj,
      BEGIN OF ty_imptt,
        point TYPE imrc_point,
        mpobj TYPE imrc_mpobj,
        psort TYPE imrc_psort,
      END OF ty_imptt,
      tt_imptt TYPE STANDARD TABLE OF ty_imptt,
      BEGIN OF ty_meas_point,
        equnr TYPE equnr,
        eq_point TYPE imrc_point,
        tplnr TYPE tplnr,
        fl_point TYPE imrc_point,
      END OF ty_meas_point,
      tt_meas_point TYPE STANDARD TABLE OF ty_meas_point.



*-- Global variables
DATA: gt_vequi TYPE tt_vequi,
      gt_iflot TYPE tt_iflot,
      gt_mpobj TYPE tt_mpobj,
      gt_imptt TYPE tt_imptt,
      gt_meas_point TYPE tt_meas_point.

*-- Local   variables
DATA: lv_mpobj TYPE imrc_mpobj,
      lw_meas_point TYPE ty_meas_point.

FIELD-SYMBOLS: <fs_vequi> TYPE ty_vequi,
               <fs_iflot> TYPE ty_iflot,
               <fs_imptt> TYPE ty_imptt,
               <fs_imptt2> TYPE ty_imptt.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_equnr FOR v_equi-equnr.
SELECT-OPTIONS: s_eqtyp FOR v_equi-eqtyp.
SELECT-OPTIONS: s_tplnr FOR v_equi-tplnr.
SELECT-OPTIONS: s_iwerk FOR v_equi-iwerk DEFAULT 'P103'.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  SELECT equnr objnr datbi iwerk tplnr
    FROM v_equi
    INTO CORRESPONDING FIELDS OF TABLE gt_vequi
    WHERE equnr IN s_equnr AND
          eqtyp IN s_eqtyp AND
          tplnr IN s_tplnr AND
          iwerk IN s_iwerk.
  IF sy-subrc NE 0.
    MESSAGE 'Please provide valid input data' TYPE 'E'.
  ENDIF.

  LOOP AT gt_vequi ASSIGNING <fs_vequi>.
    lv_mpobj = <fs_vequi>-objnr.
    APPEND lv_mpobj TO gt_mpobj.
  ENDLOOP.

  SELECT tplnr objnr
    FROM iflot
    INTO TABLE gt_iflot
    FOR ALL ENTRIES IN gt_vequi
    WHERE tplnr = gt_vequi-tplnr.
  IF sy-subrc EQ 0.
    LOOP AT gt_iflot ASSIGNING <fs_iflot>.
      lv_mpobj = <fs_iflot>-objnr.
      APPEND lv_mpobj TO gt_mpobj.
    ENDLOOP.
  ENDIF.

  IF gt_mpobj IS NOT INITIAL.
    SELECT point mpobj psort
      FROM imptt
      INTO TABLE gt_imptt
      FOR ALL ENTRIES IN gt_mpobj
      WHERE mpobj = gt_mpobj-mpobj.
  ENDIF.

  SORT gt_iflot BY tplnr.
  SORT gt_imptt BY mpobj psort.

  LOOP AT gt_vequi ASSIGNING <fs_vequi>.
    LOOP AT gt_imptt ASSIGNING <fs_imptt> WHERE mpobj = <fs_vequi>-objnr.
      CLEAR: lw_meas_point.
      lw_meas_point-equnr = <fs_vequi>-equnr.
      lw_meas_point-eq_point = <fs_imptt>-point.
      lw_meas_point-tplnr = <fs_vequi>-tplnr.
      READ TABLE gt_iflot ASSIGNING <fs_iflot> WITH KEY tplnr = <fs_vequi>-tplnr BINARY SEARCH.
      IF  sy-subrc EQ 0.
        READ TABLE gt_imptt  ASSIGNING <fs_imptt2> WITH KEY mpobj = <fs_iflot>-objnr
                                                            psort = <fs_imptt>-psort BINARY SEARCH.
        IF sy-subrc EQ 0.
          lw_meas_point-fl_point = <fs_imptt2>-point.
        ENDIF.
      ENDIF.
      APPEND lw_meas_point TO gt_meas_point.
    ENDLOOP.
  ENDLOOP.


END-OF-SELECTION.
  PERFORM display_alv.


*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .
  DATA: lr_salv_table TYPE REF TO cl_salv_table,
        lr_layout        TYPE REF TO cl_salv_layout,
        lr_alv_functions TYPE REF TO cl_salv_functions_list,
        lr_columns       TYPE REF TO cl_salv_columns_table ,
        lr_column        TYPE REF TO cl_salv_column_table .

  DATA lw_layout_key TYPE salv_s_layout_key.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lr_salv_table
        CHANGING
          t_table      = gt_meas_point.

      lr_columns = lr_salv_table->get_columns( ).
      lr_columns->set_optimize( 'X' ).

** Functions
      lr_alv_functions = lr_salv_table->get_functions( ).
      lr_alv_functions->set_default( 'X' ).
      lr_alv_functions->set_all( 'X' ).
** Display the table

      lr_layout = lr_salv_table->get_layout( ).
      lw_layout_key-report = sy-repid.
      lr_layout->set_key( lw_layout_key ).
      lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
      lr_layout->set_default( if_salv_c_bool_sap=>true ).

      lr_salv_table->display( ).

    CATCH cx_salv_msg.

  ENDTRY.


ENDFORM.                    " DISPLAY_ALV
