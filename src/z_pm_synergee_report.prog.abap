*&---------------------------------------------------------------------*
*& Report  Z_PM_SYNERGEE_REPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_pm_synergee_report.
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS: hotspot_click  FOR EVENT link_click OF cl_salv_events_table
                         IMPORTING row column.
ENDCLASS.               "LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_event_handler
*&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD hotspot_click.
    PERFORM handle_click USING row column.
  ENDMETHOD.                    "hotspot_click1
ENDCLASS.               "lcl_event_handler

TABLES: iflo,
        iflos,
        cabn,
        ausp.
TYPES: BEGIN OF ty_charac,
        atwtb TYPE cawnt-atwtb,
        atwrt TYPE cawn-atwrt,
       END OF ty_charac.
TYPES: BEGIN OF ty_output,
       stort TYPE iloa-stort, "Station id
       pltxt TYPE iflo-pltxt, "Station name
       tplnr TYPE iflo-tplnr, "functional location
       dname(4),              "District Name
       bname(2),              "Branch Name
       sclassd TYPE cawnt-ATWTB, " Station class description
       spurpos TYPE cawnt-ATWTB, "station purpose description
       dsgncode1 TYPE ausp-atwrt, "Stn Design code1
       dsgncode2 TYPE ausp-atwrt, "Stn Design code2
       dsgncode3 TYPE ausp-atwrt, "Stn Design code3
       stnstage  TYPE ausp-atwrt, "Stn Stage
       inletmaop TYPE ausp-atwrt, "Inlet Maop KPa
       inletmin  TYPE ausp-atwrt, "ausp-atflv, "Inlet Min KPa
       outletmaop TYPE ausp-atwrt, "ausp-atflv, "Outlet Maop
       outletmin  TYPE ausp-atwrt, "ausp-atflv, "Outlet Min
       maxpress   TYPE ausp-atwrt, "ausp-atflv, "MaxPress
       capQty     TYPE ausp-atwrt, "ausp-atflv, "Capacity Qty
       dsgncap    TYPE ausp-atwrt, "Design Cap Status
       END OF ty_output.
CONSTANTS: gc_stn_class_code TYPE cabn-atnam VALUE 'STN_CLASS_CODE',
           gc_stn_purpose_code TYPE cabn-atnam VALUE 'STN_PURPOSE_CODE'.
DATA: gt_f4_char TYPE TABLE OF ty_charac,
      gs_f4_char TYPE ty_charac,
      gt_output TYPE TABLE OF ty_output,
      gs_output TYPE ty_output.
data: gt_iflo TYPE TABLE OF iflo,
      gs_iflo type iflo,
      gt_inob TYPE TABLE OF inob,
      gs_inob TYPE inob,
      gt_ausp TYPE TABLE OF ausp,
      gs_ausp TYPE ausp,
      gt_cabn TYPE TABLE OF cabn,
      gs_cabn TYPE cabn,
      gt_cawn TYPE TABLE OF cawn,
      gs_cawn TYPE cawn,
      gt_cawnt TYPE TABLE OF cawnt,
      gs_cawnt TYPE cawnt,
      gt_comp_tab TYPE TABLE OF rstrucinfo WITH HEADER LINE,
      gt_output1 LIKE gt_output WITH HEADER LINE.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_stort  FOR iflo-stort,
                s_tplnr  for iflo-tplnr,
                s_atinn  FOR ausp-atinn no-DISPLAY,
                s_atwrt  for ausp-atwrt no-DISPLAY.
                "s_strno  FOR iflos-strno.
PARAMETERS: p_stncls TYPE cawn-atwrt,
            p_stnpur TYPE cawn-atwrt.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_stncls.
  PERFORM f4_stn_class_code.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_stnpur.
  PERFORM f4_stn_purpose_code.


START-OF-SELECTION.
CLEAR:  gt_iflo,
        gs_iflo,
        gt_inob,
        gs_inob,
        gt_ausp,
        gs_ausp,
        gt_cawnt,
        gt_Cawn,
        gs_cawnt,
        gs_Cawn,
        gt_output.
**************Get Components
  PERFORM get_components.
  refresh: s_atinn,
           s_atwrt.
  CLEAR: s_atinn,
         s_atwrt.
IF p_stncls is not INITIAL.

CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
  EXPORTING
    input         = gc_stn_class_code
 IMPORTING
   OUTPUT        =   s_atinn-low.
  s_atinn-sign = 'I'.
  s_atinn-option = 'EQ'.
*  s_atinn-low = 'STN_CLASS_CODE'.
  APPEND s_atinn.
  s_atwrt-sign = 'I'.
  s_atwrt-option = 'EQ'.
  s_atwrt-low = p_stncls.
  APPEND s_atwrt.

ENDIF.
IF p_stnpur is not INITIAL.
CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
  EXPORTING
    input         = gc_stn_purpose_code
 IMPORTING
   OUTPUT        =   s_atinn-low.
  s_atinn-sign = 'I'.
  s_atinn-option = 'EQ'.
*  s_atinn-low = 'STN_PURPOSE_CODE'.
  APPEND s_atinn.
  s_atwrt-sign = 'I'.
  s_atwrt-option = 'EQ'.
  s_atwrt-low = p_stnpur.
  APPEND s_atwrt.

ENDIF.
  PERFORM validate_stncls_pur.
  PERFORM get_data.
  PERFORM get_station_data.
  PERFORM display_alv.
*&---------------------------------------------------------------------*
*&      Form  F4_STN_CLASS_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_stn_class_code .

  DATA: ls_cabn TYPE cabn,
        lt_cawn TYPE TABLE OF cawn,
        ls_cawn TYPE cawn,
        lt_cawnt TYPE TABLE OF cawnt,
        ls_cawnt TYPE cawnt,
        lt_return TYPE TABLE OF ddshretval.

  CLEAR: gt_f4_char.

  SELECT SINGLE * FROM cabn INTO ls_cabn WHERE atnam = gc_stn_class_code.
  SELECT * FROM cawn INTO TABLE lt_cawn WHERE atinn = ls_cabn-atinn.
  IF lt_cawn[] IS NOT INITIAL.
    SELECT * FROM cawnt INTO TABLE lt_cawnt
      FOR ALL ENTRIES IN lt_cawn
      WHERE atinn = lt_cawn-atinn
        AND atzhl = lt_cawn-atzhl
        AND spras = sy-langu
        AND adzhl = lt_cawn-adzhl.
  ENDIF.
  LOOP AT lt_cawn INTO ls_cawn.
    READ TABLE lt_cawnt INTO ls_cawnt WITH KEY atinn = ls_cawn-atinn
                                               atzhl = ls_cawn-atzhl
                                               spras = sy-langu
                                               adzhl = ls_cawn-adzhl.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    gs_f4_char-atwrt = ls_cawn-atwrt.
    gs_f4_char-atwtb = ls_cawnt-atwtb.
    APPEND gs_f4_char TO gt_f4_char.
  ENDLOOP.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ATWRT'
      dynpprog        = sy-repid
      dynpnr          = '1000'
      dynprofield     = 'P_STNCLS'
      value_org       = 'S'
    TABLES
      value_tab       = gt_f4_char
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.                    " F4_STN_CLASS_CODE
*&---------------------------------------------------------------------*
*&      Form  F4_STN_PURPOSE_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_stn_purpose_code .

  DATA: ls_cabn TYPE cabn,
        lt_cawn TYPE TABLE OF cawn,
        ls_cawn TYPE cawn,
        lt_cawnt TYPE TABLE OF cawnt,
        ls_cawnt TYPE cawnt,
        lt_return TYPE TABLE OF ddshretval.

  CLEAR: gt_f4_char.

  SELECT SINGLE * FROM cabn INTO ls_cabn WHERE atnam = gc_stn_purpose_code.
  SELECT * FROM cawn INTO TABLE lt_cawn WHERE atinn = ls_cabn-atinn.
  IF lt_cawn[] IS NOT INITIAL.
    SELECT * FROM cawnt INTO TABLE lt_cawnt
      FOR ALL ENTRIES IN lt_cawn
      WHERE atinn = lt_cawn-atinn
        AND atzhl = lt_cawn-atzhl
        AND spras = sy-langu
        AND adzhl = lt_cawn-adzhl.
  ENDIF.
  LOOP AT lt_cawn INTO ls_cawn.
    READ TABLE lt_cawnt INTO ls_cawnt WITH KEY atinn = ls_cawn-atinn
                                               atzhl = ls_cawn-atzhl
                                               spras = sy-langu
                                               adzhl = ls_cawn-adzhl.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    gs_f4_char-atwrt = ls_cawn-atwrt.
    gs_f4_char-atwtb = ls_cawnt-atwtb.
    APPEND gs_f4_char TO gt_f4_char.
  ENDLOOP.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ATWRT'
      dynpprog        = sy-repid
      dynpnr          = '1000'
      dynprofield     = 'P_STNPUR'
      value_org       = 'S'
    TABLES
      value_tab       = gt_f4_char
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.                    " F4_STN_PURPOSE_CODE
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_STNCLS_PUR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM validate_stncls_pur .
  DATA: ls_cabn TYPE cabn,
        lt_cawn TYPE TABLE OF cawn,
        ls_cawn TYPE cawn,
        lt_cawnt TYPE TABLE OF cawnt,
        ls_cawnt TYPE cawnt,
        lt_return TYPE TABLE OF ddshretval.

  CLEAR: gt_f4_char.
  IF p_stncls IS NOT INITIAL.
    SELECT SINGLE * FROM cabn INTO ls_cabn WHERE atnam = gc_stn_class_code.
    SELECT SINGLE * FROM cawn INTO ls_cawn WHERE atinn = ls_cabn-atinn
                                             AND atwrt = p_stncls.
    IF sy-subrc <> 0.
      WRITE : / 'Wrong Station Class....'.
      STOP.
    ENDIF.
  ENDIF.
  IF p_stnpur IS NOT INITIAL.
    CLEAR ls_cabn.
    SELECT SINGLE * FROM cabn INTO ls_cabn WHERE atnam = gc_stn_purpose_code.
    SELECT SINGLE * FROM cawn INTO ls_cawn WHERE atinn = ls_cabn-atinn
                                             AND atwrt = p_stnpur.
    IF sy-subrc <> 0.
      WRITE : / 'Wrong Station Purpose....'.
      STOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " VALIDATE_STNCLS_PUR
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_DATA .

TYPES: BEGIN OF ty_objek,
        objek TYPE ausp-objek,
       END OF ty_objek.

CONSTANTS: lc_st_rg TYPE iflo-eqart VALUE 'ST_RG'.
data: lv_cuobj TYPE inob-cuobj,
      lt_objek TYPE TABLE OF ty_objek,
      ls_objek TYPE ty_objek,
      lt_ausp TYPE TABLE OF ausp.

SELECT * from iflo INTO TABLE gt_iflo  WHERE tplnr in s_tplnr
                                         AND spras = sy-langu
                                         AND stort in s_stort
                                         AND eqart = lc_st_rg.
LOOP AT gt_iflo INTO gs_iflo.
  ls_objek-objek = gs_iflo-tplnr.
  APPEND ls_objek TO lt_objek.
ENDLOOP.
SELECT * FROM inob
         INTO TABLE gt_inob
         FOR ALL ENTRIES IN lt_objek
         WHERE obtab = 'IFLOT'
           AND objek = lt_objek-objek
           AND klart = '003'.
SORT gt_inob BY objek.
LOOP AT gt_inob INTO gs_inob.
  CLEAR lv_cuobj.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_inob-cuobj
    IMPORTING
      output = lv_cuobj.

  ls_objek-objek = lv_cuobj.

  APPEND ls_objek-objek TO lt_objek.
ENDLOOP.
* Get Characteristic Values
*if p_stncls is not INITIAL or
*   p_stnpur is not INITIAL.
*  SELECT * FROM ausp
*         INTO TABLE gt_ausp
*         FOR ALL ENTRIES IN lt_objek
*         WHERE objek = lt_objek-objek
*           AND ( atinn = gc_stn_class_code or
*                 atinn = gc_stn_purpose_code )
*           AND ( atwrt = p_stncls or
*                 atwrt = p_stnpur )
*           AND mafid = 'O'
*           AND klart = '003'
*           AND adzhl = space.
*else.
*IF s_atinn[] is not INITIAL or
*   s_atwrt[] is not INITIAL.
*   SELECT * FROM ausp
*         INTO TABLE lt_ausp
*         FOR ALL ENTRIES IN lt_objek
*         WHERE objek = lt_objek-objek
*           AND atinn in s_atinn
*           AND atwrt in s_atwrt
*           AND mafid = 'O'
*           AND klart = '003'
*           AND adzhl = space.
*ENDIF.
  SELECT * FROM ausp
         INTO TABLE gt_ausp
         FOR ALL ENTRIES IN lt_objek
         WHERE objek = lt_objek-objek
*           AND atinn in s_atinn
*           AND atwrt in s_atwrt
           AND mafid = 'O'
           AND klart = '003'
           AND adzhl = space.

*endif.
*IF lt_ausp[] is not INITIAL.
*   append LINES OF lt_ausp to gt_ausp.
*ENDIF.
SORT gt_ausp BY objek atinn klart.

* Get Characteristic internal numbers
  SELECT * FROM cabn INTO TABLE gt_cabn
                   WHERE atnam IN ('STN_CLASS_CODE',
                                   'STN_PURPOSE_CODE',
                                    'DESIGN_CODE1',
                                    'DESIGN_CODE2',
                                    'DESIGN_CODE3',
                                    'DESIGN_CODE4',
                                    'STN_STAGE_1',
                                    'INLET_MOP_1',
                                    'INLET_MIN_1',
                                    'OUTLET_MOP_1',
                                    'OUTLET_MIN_1',
                                    'MAX_PRESS_1',
                                    'CAPACITY_QUANTITY_1',
                                    'DSGNCAP_REVSD_DT',
                                    'STN_STAGE_2',
                                    'INLET_MOP_2',
                                    'INLET_MIN_2',
                                    'OUTLET_MOP_2',
                                    'OUTLET_MIN_2',
                                    'MAX_PRESS_2',
                                    'CAPACITY_QUANTITY_2',
                                    'STN_STAGE_3',
                                    'INLET_MOP_3',
                                    'INLET_MIN_3',
                                    'OUTLET_MOP_3',
                                    'OUTLET_MIN_3',
                                    'MAX_PRESS_3',
                                    'CAPACITY_QUANTITY_3',
                                    'STN_STAGE_4',
                                     'INLET_MOP_4',
                                     'INLET_MIN_4',
                                     'OUTLET_MOP_4',
                                     'OUTLET_MIN_4',
                                     'MAX_PRESS_4',
                                     'CAPACITY_QUANTITY_4',
                                    'STN_STAGE_5',
                                    'INLET_MOP_5',
                                    'INLET_MIN_5',
                                    'OUTLET_MOP_5',
                                    'OUTLET_MIN_5',
                                    'MAX_PRESS_5',
                                    'CAPACITY_QUANTITY_5' ).

   SORT gt_cabn BY atnam.
  if gt_cabn[] is not INITIAL.
     SELECT * FROM cawn INTO TABLE gt_cawn FOR ALL ENTRIES IN gt_cabn
       WHERE atinn = gt_cabn-atinn.
  endif.
  IF gt_cawn[] IS NOT INITIAL.
    SELECT * FROM cawnt INTO TABLE gt_cawnt
      FOR ALL ENTRIES IN gt_cawn
      WHERE atinn = gt_cawn-atinn
        AND atzhl = gt_cawn-atzhl
        AND spras = sy-langu
        AND adzhl = gt_cawn-adzhl.
  ENDIF.
  ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_CHAR_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_OBJEK  text
*      -->P_0843   text
*      -->P_0844   text
*      -->P_0      text
*      <--P_LWA_OP_NON_REG_RATED_CAP  text
*----------------------------------------------------------------------*
FORM GET_CHAR_VALUE  USING    p_objek TYPE ausp-objek
                                        p_atnam TYPE cabn-atnam
                                        p_klart TYPE ausp-klart
                                        p_dec   TYPE i
                               CHANGING p_value TYPE any.

  DATA: lv_char10 TYPE char10,
        lv_objek  TYPE ausp-objek.

  CLEAR : gs_cabn,
          gs_ausp,
          gs_inob.

  lv_objek = p_objek.

  CLEAR p_value.
  READ TABLE gt_cabn INTO gs_cabn WITH KEY atnam = p_atnam.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
  READ TABLE gt_inob INTO gs_inob
                      WITH KEY objek = lv_objek.
 lv_objek = gs_inob-cuobj.
  READ TABLE gt_ausp INTO gs_ausp WITH KEY objek = lv_objek
                                           atinn = gs_cabn-atinn
                                           klart = p_klart
                                                    BINARY SEARCH.
  IF sy-subrc <> 0 .
    RETURN.
  ENDIF.
  CASE gs_cabn-atfor.
    WHEN 'DATE'.
      WRITE gs_ausp-atflv TO lv_char10 EXPONENT 0 DECIMALS 0.
      CONDENSE lv_char10.
      WRITE lv_char10 TO p_value.
    WHEN 'NUM'.
      WRITE gs_ausp-atflv TO lv_char10 EXPONENT 0 DECIMALS p_dec.
      CONDENSE lv_char10.
      WRITE lv_char10 TO p_value.
    WHEN 'CHAR'.
      p_value = gs_ausp-atwrt.
  ENDCASE.

ENDFORM.                    " GET_CHAR_VALUE
*&---------------------------------------------------------------------*
*&      Form  GET_STATION_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_STATION_DATA .

data: lv_atwrt type ausp-atwrt,
      lv_objek TYPE ausp-objek,
      ls_iflo TYPE iflo,
      lv_append TYPE xfeld,
      lv_check type xfeld.



LOOP AT gt_iflo INTO ls_iflo.
  CLEAR: gs_output,
         lv_append.
"-----------------------------------
  gs_output-stort = ls_iflo-stort. "Station id
  gs_output-pltxt = ls_iflo-pltxt. "Station name
  gs_output-tplnr = ls_iflo-tplnr. "functional location
  gs_output-dname = ls_iflo-tplnr(4).    "District Name
  gs_output-bname = ls_iflo-tplnr+5(2).  "Branch Name
  lv_objek = ls_iflo-tplnr.
"STN_CLASS_CODE'
"STN_PURPOSE_CODE
  "Station class description
  clear lv_atwrt.
  PERFORM get_char_desc  USING lv_objek
                               'STN_CLASS_CODE'
                               '003'
                               0
                         CHANGING gs_output-sclassd
                                  lv_atwrt.
  IF p_stncls is NOT INITIAL.
     IF lv_atwrt <> p_stncls.
        CONTINUE.
     ENDIF.
  ENDIF.
  "station purpose description
  clear lv_atwrt.
  PERFORM get_char_desc  USING lv_objek
                               'STN_PURPOSE_CODE'
                               '003'
                               0
                         CHANGING gs_output-spurpos
                                  lv_atwrt.
  IF p_stnpur is NOT INITIAL.
     IF lv_atwrt <> p_stnpur.
        CONTINUE.
     ENDIF.
  ENDIF.
  PERFORM get_char_value USING lv_objek
                                     'DESIGN_CODE1'
                                     '003'
                                     0
                                   CHANGING gs_output-dsgncode1.

  PERFORM get_char_value USING lv_objek
                                     'DESIGN_CODE2'
                                     '003'
                                     0
                                   CHANGING gs_output-dsgncode2.

  PERFORM get_char_value USING lv_objek
                                     'DESIGN_CODE3'
                                     '003'
                                     0
                                   CHANGING gs_output-dsgncode3.
*
*  PERFORM get_char_value USING lv_objek
*                                     'DESIGN_CODE4'
*                                     '003'
*                                     0
*                                   CHANGING gs_output-dsgncode4.
PERFORM get_char_value USING lv_objek
                                     'DSGNCAP_REVSD_DT'
                                     '003'
                                     0
                                   CHANGING gs_output-dsgncap.
"--------------------------------------------------------------------------
  PERFORM get_char_value USING lv_objek
                                     'STN_STAGE_1'
                                     '003'
                                     0
                                   CHANGING gs_output-stnstage.

  PERFORM get_char_value USING lv_objek
                                     'INLET_MOP_1'
                                     '003'
                                     2
                                   CHANGING gs_output-inletmaop.

  PERFORM get_char_value USING lv_objek
                                     'INLET_MIN_1'
                                     '003'
                                     2
                                   CHANGING gs_output-inletmin.

  PERFORM get_char_value USING lv_objek
                                     'OUTLET_MOP_1'
                                     '003'
                                     2
                                   CHANGING gs_output-outletmaop.

  PERFORM get_char_value USING lv_objek
                                     'OUTLET_MIN_1'
                                     '003'
                                     2
                                   CHANGING gs_output-outletmin.

  PERFORM get_char_value USING lv_objek
                                     'MAX_PRESS_1'
                                     '003'
                                     2
                                   CHANGING gs_output-maxpress.

  PERFORM get_char_value USING lv_objek
                                     'CAPACITY_QUANTITY_1'
                                     '003'
                                     0
                                   CHANGING gs_output-capQty.

 "------------------------------------
 IF gs_output-stnstage is not INITIAL or
    gs_output-inletmaop is not INITIAL or
    gs_output-inletmin is not INITIAL or
    gs_output-outletmaop is not INITIAL or
    gs_output-outletmin is not INITIAL or
    gs_output-maxpress is not INITIAL or
    gs_output-capQty is not INITIAL.
    APPEND gs_output to gt_output.
    lv_append = 'X'.
ENDIF.
  CLEAR:  gs_output-stnstage,
          gs_output-inletmaop,
          gs_output-inletmin,
          gs_output-outletmaop,
          gs_output-outletmin,
          gs_output-maxpress,
          gs_output-capQty.
"----------------------------------
  PERFORM get_char_value USING lv_objek
                                     'STN_STAGE_2'
                                     '003'
                                     0
                                   CHANGING gs_output-stnstage.

  PERFORM get_char_value USING lv_objek
                                     'INLET_MOP_2'
                                     '003'
                                     2
                                   CHANGING gs_output-inletmaop.

  PERFORM get_char_value USING lv_objek
                                     'INLET_MIN_2'
                                     '003'
                                     2
                                   CHANGING gs_output-inletmin.

  PERFORM get_char_value USING lv_objek
                                     'OUTLET_MOP_2'
                                     '003'
                                     2
                                   CHANGING gs_output-outletmaop.

  PERFORM get_char_value USING lv_objek
                                     'OUTLET_MIN_2'
                                     '003'
                                     2
                                   CHANGING gs_output-outletmin.

  PERFORM get_char_value USING lv_objek
                                     'MAX_PRESS_2'
                                     '003'
                                     2
                                   CHANGING gs_output-maxpress.

  PERFORM get_char_value USING lv_objek
                                     'CAPACITY_QUANTITY_2'
                                     '003'
                                     0
                                   CHANGING gs_output-capQty.
  "------------------------------------
 IF gs_output-stnstage is not INITIAL or
    gs_output-inletmaop is not INITIAL or
    gs_output-inletmin is not INITIAL or
    gs_output-outletmaop is not INITIAL or
    gs_output-outletmin is not INITIAL or
    gs_output-maxpress is not INITIAL or
    gs_output-capQty is not INITIAL.
    APPEND gs_output to gt_output.
    lv_append = 'X'.
ENDIF.
   CLEAR:  gs_output-stnstage,
          gs_output-inletmaop,
          gs_output-inletmin,
          gs_output-outletmaop,
          gs_output-outletmin,
          gs_output-maxpress,
          gs_output-capQty.
  "----------------------------------

  PERFORM get_char_value USING lv_objek
                                     'STN_STAGE_3'
                                     '003'
                                     0
                                   CHANGING gs_output-stnstage.

  PERFORM get_char_value USING lv_objek
                                     'INLET_MOP_3'
                                     '003'
                                     2
                                   CHANGING gs_output-inletmaop.

  PERFORM get_char_value USING lv_objek
                                     'INLET_MIN_3'
                                     '003'
                                     2
                                   CHANGING gs_output-inletmin.

  PERFORM get_char_value USING lv_objek
                                     'OUTLET_MOP_3'
                                     '003'
                                     2
                                   CHANGING gs_output-outletmaop.

  PERFORM get_char_value USING lv_objek
                                     'OUTLET_MIN_3'
                                     '003'
                                     2
                                   CHANGING gs_output-outletmin.

  PERFORM get_char_value USING lv_objek
                                     'MAX_PRESS_3'
                                     '003'
                                     2
                                   CHANGING gs_output-maxpress.

  PERFORM get_char_value USING lv_objek
                                     'CAPACITY_QUANTITY_3'
                                     '003'
                                     0
                                   CHANGING gs_output-capQty.
  "------------------------------------
   IF gs_output-stnstage is not INITIAL or
    gs_output-inletmaop is not INITIAL or
    gs_output-inletmin is not INITIAL or
    gs_output-outletmaop is not INITIAL or
    gs_output-outletmin is not INITIAL or
    gs_output-maxpress is not INITIAL or
    gs_output-capQty is not INITIAL.
    APPEND gs_output to gt_output.
    lv_append = 'X'.
ENDIF.
   CLEAR:  gs_output-stnstage,
          gs_output-inletmaop,
          gs_output-inletmin,
          gs_output-outletmaop,
          gs_output-outletmin,
          gs_output-maxpress,
          gs_output-capQty.
  "----------------------------------
  PERFORM get_char_value USING lv_objek
                                     'STN_STAGE_4'
                                     '003'
                                     0
                                   CHANGING gs_output-stnstage.

  PERFORM get_char_value USING lv_objek
                                     'INLET_MOP_4'
                                     '003'
                                     2
                                   CHANGING gs_output-inletmaop.

  PERFORM get_char_value USING lv_objek
                                     'INLET_MIN_4'
                                     '003'
                                     2
                                   CHANGING gs_output-inletmin.

  PERFORM get_char_value USING lv_objek
                                     'OUTLET_MOP_4'
                                     '003'
                                     2
                                   CHANGING gs_output-outletmaop.

  PERFORM get_char_value USING lv_objek
                                     'OUTLET_MIN_4'
                                     '003'
                                     2
                                   CHANGING gs_output-outletmin.

  PERFORM get_char_value USING lv_objek
                                     'MAX_PRESS_4'
                                     '003'
                                     2
                                   CHANGING gs_output-maxpress.

  PERFORM get_char_value USING lv_objek
                                     'CAPACITY_QUANTITY_4'
                                     '003'
                                     0
                                   CHANGING gs_output-capQty.
  "------------------------------------
   IF gs_output-stnstage is not INITIAL or
    gs_output-inletmaop is not INITIAL or
    gs_output-inletmin is not INITIAL or
    gs_output-outletmaop is not INITIAL or
    gs_output-outletmin is not INITIAL or
    gs_output-maxpress is not INITIAL or
    gs_output-capQty is not INITIAL.
    APPEND gs_output to gt_output.
    lv_append = 'X'.
  ENDIF.
   CLEAR:  gs_output-stnstage,
          gs_output-inletmaop,
          gs_output-inletmin,
          gs_output-outletmaop,
          gs_output-outletmin,
          gs_output-maxpress,
          gs_output-capQty.
  "----------------------------------
  PERFORM get_char_value USING lv_objek
                                     'STN_STAGE_5'
                                     '003'
                                     0
                                   CHANGING gs_output-stnstage.

  PERFORM get_char_value USING lv_objek
                                     'INLET_MOP_5'
                                     '003'
                                     2
                                   CHANGING gs_output-inletmaop.

  PERFORM get_char_value USING lv_objek
                                     'INLET_MIN_5'
                                     '003'
                                     2
                                   CHANGING gs_output-inletmin.

  PERFORM get_char_value USING lv_objek
                                     'OUTLET_MOP_5'
                                     '003'
                                     2
                                   CHANGING gs_output-outletmaop.

  PERFORM get_char_value USING lv_objek
                                     'OUTLET_MIN_5'
                                     '003'
                                     2
                                   CHANGING gs_output-outletmin.

  PERFORM get_char_value USING lv_objek
                                     'MAX_PRESS_5'
                                     '003'
                                     2
                                   CHANGING gs_output-maxpress.

  PERFORM get_char_value USING lv_objek
                                     'CAPACITY_QUANTITY_5'
                                     '003'
                                     0
                                   CHANGING gs_output-capQty.
  "------------------------------------
   IF gs_output-stnstage is not INITIAL or
    gs_output-inletmaop is not INITIAL or
    gs_output-inletmin is not INITIAL or
    gs_output-outletmaop is not INITIAL or
    gs_output-outletmin is not INITIAL or
    gs_output-maxpress is not INITIAL or
    gs_output-capQty is not INITIAL.
    APPEND gs_output to gt_output.
    lv_append = 'X'.
 ENDIF.
  "----------------------------------
 IF lv_append is INITIAL.
    append gs_output to gt_output.
 ENDIF.
ENDLOOP.

ENDFORM.                    " GET_STATION_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_CHAR_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_OBJEK  text
*      -->P_1179   text
*      -->P_1180   text
*      -->P_0      text
*      <--P_GS_OUTPUT_SCLASSD  text
*----------------------------------------------------------------------*
FORM GET_CHAR_DESC  USING    p_objek TYPE ausp-objek
                                        p_atnam TYPE cabn-atnam
                                        p_klart TYPE ausp-klart
                                        p_dec   TYPE i
                               CHANGING p_value TYPE cawnt-ATWTB
                                        p_atwrt type ausp-atwrt.

  DATA: lv_char10 TYPE char10,
        lv_objek  TYPE ausp-objek.

  CLEAR : gs_cabn,
          gs_ausp,
          gs_inob,
          gs_cawn,
          gs_Cawnt.

  lv_objek = p_objek.

  CLEAR p_value.
  READ TABLE gt_cabn INTO gs_cabn WITH KEY atnam = p_atnam.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
  READ TABLE gt_inob INTO gs_inob
                      WITH KEY objek = lv_objek.
 lv_objek = gs_inob-cuobj.
  READ TABLE gt_ausp INTO gs_ausp WITH KEY objek = lv_objek
                                           atinn = gs_cabn-atinn
                                           klart = p_klart
                                           BINARY SEARCH.
  IF sy-subrc <> 0 .
    RETURN.
  ENDIF.
  READ TABLE gt_Cawn INTO gs_cawn with key atinn = gs_Cabn-atinn
                                           atwrt = gs_ausp-atwrt.
  READ TABLE gt_cawnt INTO gs_cawnt WITH KEY atinn = gs_cawn-atinn
                                             atzhl = gs_cawn-atzhl
                                             spras = sy-langu
                                             adzhl = gs_cawn-adzhl.
 p_value = gs_cawnt-ATWTB.
 p_atwrt = gs_ausp-atwrt.

ENDFORM.                    " GET_CHAR_DESC
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV .
 DATA:   ls_key         TYPE salv_s_layout_key,
          lo_table       TYPE REF TO cl_salv_table,
          lo_layout      TYPE REF TO cl_salv_layout,
          lo_functions   TYPE REF TO cl_salv_functions,
          lo_display     TYPE REF TO cl_salv_display_settings,
          lo_columns     TYPE REF TO cl_salv_columns_table,
          lo_column      TYPE REF TO cl_salv_column_table,  "#EC NEEDED
          lo_content     TYPE REF TO cl_salv_form_element,
          lo_grid        TYPE REF TO cl_salv_form_layout_grid,
          lo_events_salv TYPE REF TO cl_salv_events_table,
          lo_event       TYPE REF TO lcl_event_handler.

  TRY.
      CALL METHOD cl_salv_table=>factory
*        EXPORTING
*    list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*          r_container    = lr_con1
*          container_name = 'ALV_CON1'
        IMPORTING
          r_salv_table   = lo_table
        CHANGING
          t_table        = gt_output.
    CATCH cx_salv_msg .                                 "#EC NO_HANDLER
  ENDTRY.
*Function settings
  lo_functions = lo_table->get_functions( ).
  lo_functions->set_all( abap_true ).
*Display Setting
  lo_display = lo_table->get_display_settings( ).

  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
*Event
  lo_events_salv = lo_table->get_event( ).
  CREATE OBJECT lo_event.
  SET HANDLER: lo_event->hotspot_click
               FOR lo_events_salv.
*Set layout
  lo_layout = lo_table->get_layout( ).
  ls_key-report = sy-repid.
  lo_layout->set_key( ls_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*  CALL METHOD lo_layout->set_initial_layout
*    EXPORTING
*      value = p_vari.
*Get columns
  CALL METHOD lo_table->get_columns
    RECEIVING
      value = lo_columns.
*****Change ALV Fields  - title etc.
  PERFORM alv_fields USING lo_columns lo_column.
******Display ALV
  CALL METHOD lo_table->display.

ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_FIELDS  USING    io_columns TYPE REF TO cl_salv_columns_table
                          io_column  TYPE REF TO cl_salv_column_table.

data: lv_short_text TYPE SCRTEXT_S,
      lv_med_text type SCRTEXT_M,
      lv_long_text TYPE SCRTEXT_L.

******hot spot
*  TRY.
*      io_column ?= io_columns->get_column( 'ANLN1' ).
*      CALL METHOD io_column->set_cell_type
*        EXPORTING
*      value = if_salv_c_cell_type=>hotspot.
*    CATCH cx_salv_not_found .
*  ENDTRY.
*
*  TRY .
*      io_column ?= io_columns->get_column( 'KNAFA' ).
*      CALL METHOD io_column->set_alignment
*      EXPORTING
*        value  = IF_SALV_C_ALIGNMENT=>RIGHT.
*
*  CATCH cx_salv_not_found .
*
*  ENDTRY.
*  "----------------------------------------------
  DATA: lv_column     TYPE lvc_fname.

  LOOP AT gt_comp_tab.
    CLEAR: lv_long_text,
           lv_short_text,
           lv_med_text.
    lv_column = gt_comp_tab-compname.
    CASE lv_column.
      WHEN 'STORT'.
        lv_long_text  = 'Station ID'.
        lv_short_text = 'StN ID'.
        lv_med_text   = 'Station ID'.
      WHEN 'PLTXT'.
        lv_long_text  = 'Station Name'.
        lv_short_text = 'Stnname'.
        lv_med_text   = 'Station Name'.
      WHEN 'DNAME'.
        lv_long_text  = 'District Name'.
        lv_short_text = 'DistName'.
        lv_med_text   = 'District Name'.
      WHEN 'BNAME'.
        lv_long_text  = 'Branch Name'.
        lv_short_text = 'BrchName'.
        lv_med_text   = 'Branch Name'.
      WHEN 'SCLASSD'.
        lv_long_text  = 'Station Class Description'.
        lv_short_text = 'StnClsDesc'.
        lv_med_text   = 'StnCls Desc'.
      WHEN 'SPURPOS'.
        lv_long_text  = 'Station Purpose Description'.
        lv_short_text = 'StnPurDesc'.
        lv_med_text   = 'StnPur Desc'.
      WHEN 'DSGNCODE1'.
        lv_long_text  = 'Stn Design Code1'.
        lv_short_text = 'StnDsgCode1'.
        lv_med_text   = 'StnDsgCode1'.
      WHEN 'DSGNCODE2'.
        lv_long_text  = 'Stn Design Code2'.
        lv_short_text = 'StnDsgCode2'.
        lv_med_text   = 'StnDsgCode2'.
      WHEN 'DSGNCODE3'.
        lv_long_text  = 'Stn Design Code3'.
        lv_short_text = 'StnDsgCode3'.
        lv_med_text   = 'StnDsgCode3'.
      WHEN 'STNSTAGE'.
        lv_long_text  = 'Stn Stage'.
        lv_short_text = 'StnStage'.
        lv_med_text   = 'Stn Stage'.
      WHEN 'INLETMAOP'.
        lv_long_text  = 'Inlet Maop KPa'.
        lv_short_text = 'InMaopKPa'.
        lv_med_text   = 'InletMaopKPa'.
      WHEN 'INLETMIN'.
        lv_long_text  = 'Inlet Min KPa'.
        lv_short_text = 'InMinKPa'.
        lv_med_text   = 'InletMinKPa'.
      WHEN 'OUTLETMAOP'.
        lv_long_text  = 'Outlet Maop'.
        lv_short_text = 'OutMaop'.
        lv_med_text   = 'OutletMaop'.
      WHEN 'OUTLETMIN'.
        lv_long_text  = 'Outlet Min'.
        lv_short_text = 'OutletMin'.
        lv_med_text   = 'Outlet Min'.
      WHEN 'MAXPRESS'.
        lv_long_text  = 'MaxPress'.
        lv_short_text = 'MaxPress'.
        lv_med_text   = 'MaxPress'.
      WHEN 'CAPQTY'.
        lv_long_text  = 'Capacity Qty'.
        lv_short_text = 'CapQty'.
        lv_med_text   = 'Cap.Qty'.
      WHEN 'DSGNCAP'.
        lv_long_text  = 'Design Cap Status'.
        lv_short_text = 'DsgCapStat'.
        lv_med_text   = 'DsgnCapStatus'.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
    TRY.

       io_column ?= io_columns->get_column( lv_column ).
       CALL METHOD io_column->set_long_text
         EXPORTING
           value = lv_long_text.
       CALL METHOD io_column->set_short_text
         EXPORTING
           value = lv_short_text.
       CALL METHOD io_column->set_medium_text
         EXPORTING
           value = lv_med_text.

    CATCH cx_salv_not_found .
    ENDTRY.

  ENDLOOP.

*****hot spot on Functional Location
     TRY.
        io_column ?= io_columns->get_column( 'TPLNR' ).
      CATCH cx_salv_not_found .
    ENDTRY.
    CALL METHOD io_column->set_cell_type
      EXPORTING
        value = if_salv_c_cell_type=>hotspot.

ENDFORM.                    " ALV_FIELDS
*&---------------------------------------------------------------------*
*&      Form  HANDLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROW  text
*      -->P_COLUMN  text
*----------------------------------------------------------------------*
FORM HANDLE_CLICK  USING iv_row TYPE salv_de_row
                          iv_column TYPE salv_de_column.

  DATA: ls_output TYPE ty_output.
*
  READ TABLE gt_output INTO ls_output INDEX iv_row.
  CHECK sy-subrc = 0.
  CASE iv_column.
      WHEN 'TPLNR'.
       SET PARAMETER ID 'IFL' FIELD ls_output-tplnr.
       CALL TRANSACTION 'IL03' AND SKIP FIRST SCREEN.
  ENDCASE.
ENDFORM.                    " HANDLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  GET_COMPONENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_COMPONENTS .

CALL FUNCTION 'GET_COMPONENT_LIST'
    EXPORTING
      program    = sy-repid
      fieldname  = 'GT_OUTPUT1'
    TABLES
      components = gt_comp_tab[].

ENDFORM.                    " GET_COMPONENTS
