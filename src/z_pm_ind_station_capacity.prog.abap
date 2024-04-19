*&---------------------------------------------------------------------*
*& Report  Z_PM_IND_STATION_CAPACITY
* Author             : Eldhose Mathew
* Date               : 11/28/2014
* Technical Contact  : Eldhose Mathew
* Business Contact   : Dave Grodkiewicz
* Purpose            : This report is a dashboard for Stations. It
*                      provides a summary of Regulator, Control Valves,
*                      and Non-regulator details of equipments
*                      associated with a station
*----------------------------------------------------------------------
*                      Modification Log
*
* Changed On Changed By Ticket   Description
* 2015/06/08 gymana  SDP85560 Fix bug that was causing some
*                             stations to be missed in the rpt
* 2015/06/09 gymana  SDP86055 Fix Orifice column and add Liner
*                             column in report
* 2015/06/25 gymana  SDP87326 - Fix code for body size not displaying
*                             - Add isolating valve size column
*                             - in Regulator/Non-Regulator sections,
*                               sort by Num at station
*                             - increase size of Regulator and Non-
*                               regulator sections
* 2015/10/06 gymana  SDP86866 - Clear out Orifice size field after
*                               each record is processed. Old size
*                               was being carried over into next record
* 2016/08/23 gymana  ACR2106 - Set body size field to 2 decimal places
* 2016/11/11 gymana  ACR2337 - Set Stn Cap Detail Max Pressure field
*                              to 2 dec places. Change logic to get set
*                              press. measurement point from FLOC lvl 5
* 2017/08/22 panusuri ACR4558 - Add new Equipment and fields to report.
*&---------------------------------------------------------------------

REPORT  z_pm_ind_station_capacity.

**********************************************************
* Tpye Declarations
**********************************************************
TYPES: BEGIN OF gty_equi,
        equnr    TYPE v_equi-equnr,
        eqktx    TYPE v_equi-eqktx,
        tplnr    TYPE v_equi-tplnr,
        typbz    TYPE v_equi-typbz,
        eqart    TYPE v_equi-eqart,
        objnr    TYPE v_equi-objnr,
        floc_obj TYPE iflo-objnr,
      END OF gty_equi,

      BEGIN OF gty_op_summary,
        premise_no TYPE iflo-msgrp,
        cap_ver_rq TYPE xflag,
        cap_rev_dt TYPE sy-datum,
        st_dsn_cd1 TYPE atwrt,
        st_dsn_cd2 TYPE atwrt,
        st_dsn_cd3 TYPE atwrt,
        st_dsn_cd4 TYPE atwrt,
      END OF gty_op_summary,

      BEGIN OF gty_op_capacity,
        stn_stage TYPE atwrt,
        inlet_mop TYPE atwrt,
        inlet_min TYPE atwrt,
        otlet_mop TYPE atwrt,
        otlet_min TYPE atwrt,
        max_press TYPE atwrt,
        cap_quant TYPE atwrt,
       END OF gty_op_capacity,

       BEGIN OF gty_op_regular,
         num_statn TYPE char15,
         equip_num TYPE v_equi-equnr,
         equip_txt TYPE v_equi-eqktx,
         equip_typ TYPE v_equi-eqart,
         equip_mod TYPE v_equi-typbz,
         body_size TYPE atwrt,
*         orif_size TYPE atwrt,         <== Delete D30K932249
         orif_size TYPE n,            " <== Insert D30K932249
         liner     TYPE atwrt,                              "SDP86055
         set_press TYPE p DECIMALS 2,
         stat_set_press TYPE atwrt,"(+)PANUSURI Ticket ACR-4558
         inlet_mop TYPE atwrt,
         inlet_min TYPE atwrt,
         otlet_mop TYPE atwrt,
         otlet_min TYPE atwrt,
         desgn_flw TYPE atwrt,
         fail_open TYPE atwrt,
         cap_stats TYPE atwrt,
       END OF gty_op_regular,

     BEGIN OF gty_op_non_reg,
         num_statn TYPE char15,
         equip_num TYPE v_equi-equnr,
         equip_txt TYPE v_equi-eqktx,
         equip_typ TYPE v_equi-eqart,
         locatereg TYPE atwrt,
         eq_typ_tx TYPE t370k_t-eartx,
         igrp_code TYPE atwrt,
         equip_mod TYPE v_equi-typbz,
         prot_eqno TYPE atwrt,
         body_size TYPE atwrt,
         iso_valve TYPE atwrt,                              "SDP87326
         set_press TYPE p DECIMALS 2,
         stat_set_press TYPE atwrt,"(+)PANUSURI Ticket ACR-4558
         rated_cap TYPE atwrt,
         press_rat TYPE atwrt,
         cap_stats TYPE atwrt,
         dsgn_flow_cap  TYPE atwrt, "(+)PANUSURI Ticket ACR-4558
         press_dsgn_rat TYPE atwrt, "(+)PANUSURI Ticket ACR-4558
     END OF gty_op_non_reg,

     BEGIN OF gty_eqart,
         eqart TYPE t370k_t-eqart,
         eartx TYPE t370k_t-eartx,
     END OF gty_eqart,

      BEGIN OF gty_objek,
        objek TYPE ausp-objek,
      END OF gty_objek,

      BEGIN OF gty_ausp,
        objek TYPE ausp-objek,
        atinn TYPE ausp-atinn,
        klart TYPE ausp-klart,
        atwrt TYPE ausp-atwrt,
        atflv TYPE ausp-atflv,
      END OF gty_ausp,

      BEGIN OF gty_imptt,
          point TYPE imptt-point,
          mpobj TYPE imptt-mpobj,
          psort TYPE imptt-psort,
          pttxt TYPE imptt-pttxt,
          mrngu TYPE imptt-mrngu,
      END OF gty_imptt,

      BEGIN OF gty_imrg,
        mdocm TYPE imrg-mdocm,
        point TYPE imrg-point,
        idate TYPE imrg-idate,
        itime TYPE imrg-itime,
        recdv TYPE imrg-recdv,
        recdu TYPE imrg-recdu,
        vlcod TYPE imrg-vlcod,
      END OF gty_imrg,

      BEGIN OF gty_inob,
        cuobj TYPE inob-cuobj,
        objek TYPE inob-objek,
        klart TYPE inob-klart,
      END OF gty_inob,

      BEGIN OF gty_refs,
        tab TYPE REF TO data,
        ref TYPE REF TO cl_salv_events_table,
      END OF gty_refs,

      BEGIN OF gty_long_text,
        long_text TYPE tdline,
      END OF gty_long_text,

      BEGIN OF gty_cabn,
        atinn TYPE cabn-atinn,
        atnam TYPE cabn-atnam,
        atfor TYPE cabn-atfor,
      END OF gty_cabn.

**********************************************************
* Data Declarations (Global)
**********************************************************
DATA: BEGIN OF gs_iflo,
       tplnr TYPE iflo-tplnr,
       msgrp TYPE iflo-msgrp,
      END OF gs_iflo,

      gv_ktext  TYPE t499s-ktext,
      gv_okcode TYPE sy-ucomm,
      gv_title  TYPE char100,

      git_imptt TYPE STANDARD TABLE OF gty_imptt,
      git_imrg  TYPE STANDARD TABLE OF gty_imrg,
      git_equi  TYPE STANDARD TABLE OF gty_equi,
      git_objek TYPE STANDARD TABLE OF gty_objek,
      git_ausp  TYPE STANDARD TABLE OF gty_ausp,
      git_inob  TYPE STANDARD TABLE OF gty_inob,
      git_cabn  TYPE STANDARD TABLE OF gty_cabn,
      git_eqart TYPE STANDARD TABLE OF gty_eqart,
      git_refs  TYPE STANDARD TABLE OF gty_refs,

      git_op_summary  TYPE STANDARD TABLE OF gty_op_summary,
      git_op_capacity TYPE STANDARD TABLE OF gty_op_capacity,
      git_op_regular  TYPE STANDARD TABLE OF gty_op_regular,
      git_op_non_reg  TYPE STANDARD TABLE OF gty_op_non_reg,
      git_long_text   TYPE STANDARD TABLE OF gty_long_text,

      gr_dock_cont TYPE REF TO cl_gui_docking_container,
      gr_splt_cont TYPE REF TO cl_gui_splitter_container,
      gr_grid1     TYPE REF TO cl_gui_container,
      gr_grid2     TYPE REF TO cl_gui_container,
      gr_grid3     TYPE REF TO cl_gui_container,
      gr_grid4     TYPE REF TO cl_gui_container,
      gr_grid5     TYPE REF TO cl_gui_container,
      gr_salv1     TYPE REF TO cl_salv_table,
      gr_salv2     TYPE REF TO cl_salv_table,
      gr_salv3     TYPE REF TO cl_salv_table,
      gr_salv4     TYPE REF TO cl_salv_table,

      gr_text_edit TYPE REF TO cl_gui_textedit,

      gr_alv_functions1 TYPE REF TO cl_salv_functions_list ##needed,
      gr_columns1       TYPE REF TO cl_salv_columns ##needed,
      gr_column1        TYPE REF TO cl_salv_column ##needed,
      gr_display1       TYPE REF TO cl_salv_display_settings,

      gr_alv_functions2 TYPE REF TO cl_salv_functions_list ##needed,
      gr_columns2       TYPE REF TO cl_salv_columns ##needed,
      gr_column2        TYPE REF TO cl_salv_column ##needed,
      gr_display2       TYPE REF TO cl_salv_display_settings,

      gr_alv_functions3 TYPE REF TO cl_salv_functions_list ##needed,
      gr_columns3       TYPE REF TO cl_salv_columns ##needed,
      gr_column3        TYPE REF TO cl_salv_column ##needed,
      gr_display3       TYPE REF TO cl_salv_display_settings,
      gr_layout3        TYPE REF TO cl_salv_layout,

      gr_alv_functions4 TYPE REF TO cl_salv_functions_list ##needed,
      gr_columns4       TYPE REF TO cl_salv_columns ##needed,
      gr_column4        TYPE REF TO cl_salv_column ##needed,
      gr_display4       TYPE REF TO cl_salv_display_settings,
      gr_layout4        TYPE REF TO cl_salv_layout.

*----------------------------------------------------------------------*
* CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
CLASS event_class DEFINITION FINAL.
* Handling hotspot click
  PUBLIC SECTION.

    CLASS-METHODS:
    handle_double_click
    FOR EVENT double_click OF cl_salv_events_table IMPORTING row "#EC NEEDED
                                                             column
                                                             sender.

ENDCLASS. "lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
* CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS event_class IMPLEMENTATION.
  METHOD handle_double_click.

    DATA: lwa_ref TYPE gty_refs,
          lv_tcode TYPE sy-tcode.

    FIELD-SYMBOLS: <lfs_tab>   TYPE INDEX TABLE,
                   <lfs_line>  TYPE any,
                   <lfs_equnr> TYPE equnr,
                   <lfs_equi>  TYPE gty_equi.

    READ TABLE git_refs INTO lwa_ref WITH KEY ref = sender.
    IF sy-subrc EQ 0.
      ASSIGN lwa_ref-tab->* TO <lfs_tab>.
      IF sy-subrc EQ 0.
        READ TABLE <lfs_tab> ASSIGNING <lfs_line> INDEX row.
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'EQUIP_NUM' OF STRUCTURE <lfs_line> TO <lfs_equnr>.
          IF sy-subrc EQ 0.
            READ TABLE git_equi ASSIGNING <lfs_equi> WITH KEY equnr = <lfs_equnr>
                                                     BINARY SEARCH.
            IF sy-subrc EQ 0.
              SET PARAMETER ID 'IFL' FIELD <lfs_equi>-tplnr.
              lv_tcode = 'IL02'.
              AUTHORITY-CHECK OBJECT 'I_TCODE'
                              ID 'TCD' FIELD lv_tcode.
              IF sy-subrc NE 0.
                lv_tcode = 'IL03'.
              ENDIF.
            ENDIF.
*            SET PARAMETER ID 'EQN' FIELD <lfs_equnr>.
*            lv_tcode = 'IE02'.
*            AUTHORITY-CHECK OBJECT 'I_TCODE'
*                            ID 'TCD' FIELD lv_tcode.
*            IF sy-subrc NE 0.
*              lv_tcode = 'IE03'.
*            ENDIF.
          ELSE.
            SET PARAMETER ID 'IFL' FIELD gs_iflo-tplnr.
            lv_tcode = 'IL02'.
            AUTHORITY-CHECK OBJECT 'I_TCODE'
                            ID 'TCD' FIELD lv_tcode.
            IF sy-subrc NE 0.
              lv_tcode = 'IL03'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_tcode IS NOT INITIAL.
      CALL TRANSACTION lv_tcode AND SKIP FIRST SCREEN.
    ENDIF.

  ENDMETHOD.                    "handle_double_click

ENDCLASS. "lcl_event_receiver IMPLEMENTATION

**********************************************************
* Selection Screen
**********************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-000.
PARAMETER: p_stn TYPE iloa-stort OBLIGATORY. "station
SELECTION-SCREEN END OF BLOCK b1.

**********************************************************
* AT SELECTION SCREEN ON
**********************************************************
AT SELECTION-SCREEN ON p_stn.

*Validate Station
  PERFORM validate_station.

**********************************************************
* Start of Selection
**********************************************************
START-OF-SELECTION.

* Get Data
  PERFORM get_data.

**********************************************************
* End of Selection
**********************************************************
END-OF-SELECTION.

* Prepare Output table
  PERFORM prepare_output.

* ALV display
  PERFORM call_alv_screen.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get Data
*----------------------------------------------------------------------*
FORM get_data .

  FIELD-SYMBOLS: <fs_inob> TYPE gty_inob.

  DATA: lwa_equi  TYPE gty_equi,
        lwa_objek TYPE gty_objek,
        lwa_objnr TYPE iflo-objnr.                          "ACR2337

* Get Functional Location Assignment
  SELECT SINGLE tplnr
                msgrp
                FROM iflo
                INTO gs_iflo
                WHERE spras = sy-langu AND
                      stort = p_stn AND                     "SDP85560
                      eqart = 'ST_RG'.                      "SDP85560

* Get Station Text
  SELECT SINGLE ktext
                FROM t499s
                INTO gv_ktext
                WHERE stand = p_stn.

* Get Equipments
  SELECT  a~equnr a~eqktx a~tplnr a~typbz a~eqart           "ACR2337
          a~objnr b~objnr AS floc_obj                       "ACR2337
    FROM v_equi AS a INNER JOIN iflo AS b                   "ACR2337
      ON a~tplnr EQ b~tplnr                                 "ACR2337
    INTO CORRESPONDING FIELDS OF TABLE git_equi             "ACR2337
   WHERE a~stort = p_stn AND                                "ACR2337
         datbi GT sy-datum.                                 "ACR2337

* Get Object number from Equipment to find FLOC which will be
* used to find SET Pressure measurement pt to pt.
*  SELECT SINGLE objnr                                         "ACR2337
*                FROM iflo                                     "ACR2337
*                INTO lwa_objnr                                "ACR2337
*                FOR ALL ENTRIES IN git_equi
*                WHERE tplnr = git_equi-tplnr.                 "ACR2337

* Get Equipment types
  IF git_equi IS NOT INITIAL.
    SELECT eqart
           eartx
           FROM t370k_t
           INTO TABLE git_eqart
           FOR ALL ENTRIES IN git_equi
           WHERE spras = sy-langu AND
                 eqart = git_equi-eqart.

    IF sy-subrc EQ 0.
      SORT git_eqart BY eqart.
    ENDIF.

  ENDIF.

* Get Characteristic internal numbers
  SELECT atinn
         atnam
         atfor
         FROM cabn
         INTO TABLE git_cabn
         WHERE atnam IN ('DSGN_CAP_STATUS',
                         'DSGNCAP_REVSD_DT',
                         'LOCATION_TO_REGS',
                         'DESIGN_CODE1',
                         'DESIGN_CODE2',
                         'DESIGN_CODE3',
                         'DESIGN_CODE4',
                         'STN_STAGE_1',
                         'STN_STAGE_2',
                         'STN_STAGE_3',
                         'STN_STAGE_4',
                         'STN_STAGE_5',
                         'INLET_MOP_1',
                         'INLET_MOP_2',
                         'INLET_MOP_3',
                         'INLET_MOP_4',
                         'INLET_MOP_5',
                         'INLET_MIN_1',
                         'INLET_MIN_2',
                         'INLET_MIN_3',
                         'INLET_MIN_4',
                         'INLET_MIN_5',
                         'OUTLET_MOP_1',
                         'OUTLET_MOP_2',
                         'OUTLET_MOP_3',
                         'OUTLET_MOP_4',
                         'OUTLET_MOP_5',
                         'OUTLET_MIN_1',
                         'OUTLET_MIN_2',
                         'OUTLET_MIN_3',
                         'OUTLET_MIN_4',
                         'OUTLET_MIN_5',
                         'MAX_PRESS_1',
                         'MAX_PRESS_2',
                         'MAX_PRESS_3',
                         'MAX_PRESS_4',
                         'MAX_PRESS_5',
                         'CAPACITY_QUANTITY_1',
                         'CAPACITY_QUANTITY_2',
                         'CAPACITY_QUANTITY_3',
                         'CAPACITY_QUANTITY_4',
                         'CAPACITY_QUANTITY_5',
                         'BODY_SIZE',
                         'BODY_SIZE_IN',
                         'BODY_INLET_SIZE_NPS',
                         'CONNECTION_SIZE_NPS',             "SDP87326
                         'DSGN_MAX_IN_PRESS',
                         'DSGN_MIN_IN_PRESS',
                         'DSGN_MAX_OUT_PRESS',
                         'DSGN_MIN_OUT_PRESS',
                         'DSGNFLOW_CAPACITY',
                         'DSGNFAILOPEN_CAPT',
                         'DSGNPRESSURE_RATING',
                         'ORIFICE_SIZE',
                         'ORIFICE_SIZE_UOM',
                         'LINER',                           "SDP86055
                         'INLET_MAX_PRESS',
                         'INLET_UOM',
                         'INLET_MIN_PRES',
                         'ISOLATING_VALVE_SIZE',            "SDP87326
                         'OUTLET_MAX_PRESS',
                         'OUTLET_UOM',
                         'OUTLET_MIN_PRESS',
                         'DESIGN_FLOW',
                         'FAIL_OPEN',
                         'DSGN_CAPACITY_STATUS',
                         'IGRP_CODE',
                         'PROTECTED_EQUIPMENT_NUM',
                         'RATED_CAPACITY',
                         'VALVE_SIZE',
                         'STATION_SET_PRESSURE',       "(+)PANUSURI ACR-4558
                         'HE_DESIGN_FLOW_CAPACITY',    "(+)PANUSURI ACR-4558
                         'HE_PRESSURE_DESIGN_RATING'). "(+)PANUSURI ACR-4558

  IF sy-subrc EQ 0.
    SORT git_cabn BY atnam.
  ENDIF.

* Prepare Object Ids to retrieve characteristics
  LOOP AT git_equi INTO lwa_equi.
    lwa_objek-objek = lwa_equi-equnr.
    APPEND lwa_objek TO git_objek.
    lwa_objek-objek = lwa_equi-tplnr.
    APPEND lwa_objek TO git_objek.
  ENDLOOP.
  lwa_objek-objek = gs_iflo-tplnr.
  APPEND lwa_objek TO git_objek.

* Get Object IDs for characteristics
  SELECT cuobj
         objek
         klart
         FROM inob
         INTO TABLE git_inob
         FOR ALL ENTRIES IN git_objek
         WHERE ( obtab = 'EQUI' OR obtab = 'IFLOT' ) AND
               objek = git_objek-objek AND
               ( klart = '002' OR klart = '003' ).

  IF sy-subrc EQ 0.
    SORT git_inob BY objek.
    LOOP AT git_inob ASSIGNING <fs_inob>.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_inob>-cuobj
        IMPORTING
          output = <fs_inob>-cuobj.

      lwa_objek-objek = <fs_inob>-cuobj.
      APPEND lwa_objek TO git_objek.
    ENDLOOP.
  ENDIF.

* Get Characteristic Values
  SELECT objek
         atinn
         klart
         atwrt
         atflv
         FROM ausp
         INTO TABLE git_ausp
         FOR ALL ENTRIES IN git_objek
         WHERE objek = git_objek-objek AND
               mafid = 'O' AND
               ( klart = '002' OR klart = '003' ) AND
               adzhl = space.

  IF sy-subrc EQ 0.
    SORT git_ausp BY objek atinn klart.
  ENDIF.

* Get Measurement points for Equipment
  SELECT point
         mpobj
         psort
         pttxt
         mrngu
         FROM imptt
         INTO TABLE git_imptt
         FOR ALL ENTRIES IN git_equi
         WHERE mpobj = git_equi-floc_obj.                   "ACR2337

  IF sy-subrc EQ 0.
    SORT git_imptt BY mpobj psort.
  ENDIF.

* Get Measurement documents with Readings
  IF git_imptt IS NOT INITIAL.
    SELECT mdocm
           point
           idate
           itime
           recdv
           recdu
           vlcod
           FROM imrg
           INTO TABLE git_imrg
           FOR ALL ENTRIES IN git_imptt
           WHERE point EQ git_imptt-point.

    IF sy-subrc EQ 0.
      SORT git_imrg BY point ASCENDING idate DESCENDING itime DESCENDING.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PREPARE_OUTPUT
*&---------------------------------------------------------------------*
*       Prepare Output
*----------------------------------------------------------------------*
FORM prepare_output .

  DATA: lwa_op_regular TYPE gty_op_regular,
        lwa_op_non_reg TYPE gty_op_non_reg,
        lwa_equi       TYPE gty_equi,
        lwa_eqart      TYPE gty_eqart,

        lv_date        TYPE sy-datum,
        lv_press_uom   TYPE imrg-vlcod,
        lv_set_press   TYPE p DECIMALS 2,
        lit_tab        TYPE STANDARD TABLE OF string,
        lwa_tab        TYPE string,
        lv_orif_uom    TYPE atwrt,
        lv_orif_size   TYPE p DECIMALS 1,
        lv_lines       TYPE int4,
        lv_objek       TYPE ausp-objek.

* Prepare Station Summary data
  PERFORM prepare_summary_output.

* Prepare Station Capacity details
  PERFORM prepare_capacity_output.

  SORT git_equi BY equnr.
* Prepare Regulator and Non-regulator details
  LOOP AT git_equi INTO lwa_equi.

    REFRESH: lit_tab.
    CLEAR: lv_orif_size, lv_orif_uom.                       "SDP86866
    SPLIT lwa_equi-tplnr AT '-' INTO TABLE lit_tab.
    DESCRIBE TABLE lit_tab LINES lv_lines.
    READ TABLE lit_tab INTO lwa_tab INDEX lv_lines.

    lwa_op_regular-num_statn = lwa_tab.
    lwa_op_regular-equip_num = lwa_equi-equnr.
    lwa_op_regular-equip_txt = lwa_equi-eqktx.
    lwa_op_regular-equip_mod = lwa_equi-typbz.
    lwa_op_regular-equip_typ = lwa_equi-eqart.

    lwa_op_non_reg-num_statn = lwa_tab.
    lwa_op_non_reg-equip_num = lwa_equi-equnr.
    lwa_op_non_reg-equip_txt = lwa_equi-eqktx.
    lwa_op_non_reg-equip_typ = lwa_equi-eqart.
    lwa_op_non_reg-equip_mod = lwa_equi-typbz.

    READ TABLE git_eqart INTO lwa_eqart WITH KEY eqart = lwa_equi-eqart
                                        BINARY SEARCH.
    IF sy-subrc EQ 0.
      lwa_op_non_reg-eq_typ_tx = lwa_eqart-eartx.
    ENDIF.

    lv_objek = lwa_equi-equnr.
    PERFORM get_characteristic_value USING lv_objek
                                       'IGRP_CODE'
                                       '002'
                                       0
                                     CHANGING lwa_op_non_reg-igrp_code.

    lv_objek = lwa_equi-tplnr.
    PERFORM get_characteristic_value USING lv_objek
                                       'PROTECTED_EQUIPMENT_NUM'
                                       '003'
                                       0
                                     CHANGING lwa_op_non_reg-prot_eqno.

    lv_objek = lwa_equi-tplnr.
    PERFORM get_characteristic_value USING lv_objek
                                       'LOCATION_TO_REGS'
                                       '003'
                                       0
                                     CHANGING lwa_op_non_reg-locatereg.

    lv_objek = lwa_equi-tplnr.
    PERFORM get_characteristic_value USING lv_objek
                                       'DSGNFLOW_CAPACITY'
                                       '003'
                                       0
                                     CHANGING lwa_op_non_reg-rated_cap.

    lv_objek = lwa_equi-equnr.
    CASE  lwa_equi-eqart.
      WHEN 'SC_VA_CT' OR
           'MD_MT_DI' OR
           'SC_VA_RG'.

        PERFORM get_characteristic_value USING lv_objek
                                           'BODY_SIZE_IN'
                                           '002'
                                           2                "ACR2106
                                         CHANGING lwa_op_regular-body_size.
        lwa_op_non_reg-body_size = lwa_op_regular-body_size.

*       BOI by PANUSURI Ticket ACR-4558
        lv_objek = lwa_equi-tplnr.
        PERFORM get_characteristic_value USING lv_objek
                                           'STATION_SET_PRESSURE'
                                           '003'
                                           2
                                         CHANGING lwa_op_regular-stat_set_press.
*       EOI by PANUSURI Ticket ACR-4558

*      WHEN 'SC_VA_RG'.
*        PERFORM get_characteristic_value USING lv_objek
*                                           'BODY_INLET_SIZE_NPS'
*                                           '002'
*                                           2                      "ACR2106
*                                         CHANGING lwa_op_regular-body_size.
* Start SDP87326 gymana add 'Connection Size NPS'
*                       and 'Isolating Valve Size'
      WHEN 'ME_FS'  OR
           'ME_FS_SE' OR
           'ME_FS_ST'.
        PERFORM get_characteristic_value USING lv_objek
                                           'CONNECTION_SIZE_NPS'
                                           '002'
                                           2                "ACR2106
                                         CHANGING lwa_op_non_reg-body_size.

      WHEN 'SC_VA_SC'.
        PERFORM get_characteristic_value USING lv_objek
                                           'ISOLATING_VALVE_SIZE'
                                           '002'
                                           2
                                         CHANGING lwa_op_non_reg-iso_valve.
        PERFORM get_characteristic_value USING lv_objek
                                           'VALVE_SIZE'   "'BODY_SIZE'
                                           '002'
                                           2                "ACR2106
                                         CHANGING lwa_op_non_reg-body_size.
* End SDP87326 gymana add 'Connection Size NPS' 'Isolating Valve Size'
*       BOI by PANUSURI Ticket ACR-4558
        lv_objek = lwa_equi-tplnr.
        PERFORM get_characteristic_value USING lv_objek
                                           'STATION_SET_PRESSURE'
                                           '003'
                                           2
                                         CHANGING lwa_op_non_reg-stat_set_press.
      WHEN 'ME_HB_DF' OR
           'ME_HB_CA' OR
           'ME_HB_EH' OR
           'ME_HB_FB' OR
           'ME_HB_IF' OR
           'ME_HB'.
        lv_objek = lwa_equi-tplnr.
        PERFORM get_characteristic_value USING lv_objek
                                           'HE_DESIGN_FLOW_CAPACITY'
                                           '003'
                                           0
                                         CHANGING lwa_op_non_reg-dsgn_flow_cap.
        lv_objek = lwa_equi-tplnr.
        PERFORM get_characteristic_value USING lv_objek
                                           'HE_PRESSURE_DESIGN_RATING'
                                           '003'
                                           0
                                         CHANGING lwa_op_non_reg-press_dsgn_rat.
*EOI PANUSURI Ticket ACR-4558
      WHEN OTHERS.
        PERFORM get_characteristic_value USING lv_objek
                                           'BODY_SIZE'
                                           '002'
                                           2                "ACR2106
                                         CHANGING lwa_op_non_reg-body_size.
    ENDCASE.

    lv_objek = lwa_equi-equnr.
    PERFORM get_characteristic_value USING lv_objek
                                       'ORIFICE_SIZE'
                                       '002'
                                       3
                                     CHANGING lwa_op_regular-orif_size.

    lv_objek = lwa_equi-equnr.
    PERFORM get_characteristic_value USING lv_objek
                                       'ORIFICE_SIZE_UOM'
                                       '002'
                                       0
                                     CHANGING lv_orif_uom.

    IF lv_orif_uom EQ '"'.
      lv_orif_size = lwa_op_regular-orif_size * 254 / 10.
    ELSEIF lv_orif_uom EQ 'MM'.
      lv_orif_size = lwa_op_regular-orif_size.
    ENDIF.
    WRITE lv_orif_size TO lwa_op_regular-orif_size.
    CONDENSE lwa_op_regular-orif_size.

* Start SDP86055 gymana add 'Liner'
    lv_objek = lwa_equi-equnr.
    PERFORM get_characteristic_value USING lv_objek
                                       'LINER'
                                       '002'
                                       0
                                     CHANGING lwa_op_regular-liner.
* End SDP86055 gymana add 'Liner'

    lv_objek = lwa_equi-tplnr.
    PERFORM get_characteristic_value USING lv_objek
                                       'DSGN_MAX_IN_PRESS'
                                       '003'
                                       2
                                     CHANGING lwa_op_regular-inlet_mop.

    lv_objek = lwa_equi-tplnr.
    PERFORM get_characteristic_value USING lv_objek
                                       'DSGN_MIN_IN_PRESS'
                                       '003'
                                       2
                                     CHANGING lwa_op_regular-inlet_min.

    lv_objek = lwa_equi-tplnr.
    PERFORM get_characteristic_value USING lv_objek
                                       'DSGN_MAX_OUT_PRESS'
                                       '003'
                                       2
                                     CHANGING lwa_op_regular-otlet_mop.

    lv_objek = lwa_equi-tplnr.
    PERFORM get_characteristic_value USING lv_objek
                                       'DSGN_MIN_OUT_PRESS'
                                       '003'
                                       2
                                     CHANGING lwa_op_regular-otlet_min.

    lv_objek = lwa_equi-tplnr.
    PERFORM get_characteristic_value USING lv_objek
                                       'DSGNFLOW_CAPACITY'
                                       '003'
                                       0
                                     CHANGING lwa_op_regular-desgn_flw.

    lv_objek = lwa_equi-tplnr.
    PERFORM get_characteristic_value USING lv_objek
                                       'DSGNFAILOPEN_CAPT'
                                       '003'
                                       0
                                     CHANGING lwa_op_regular-fail_open.

    lv_objek = lwa_equi-tplnr.
    PERFORM get_characteristic_value USING lv_objek
                                       'DSGNPRESSURE_RATING'
                                       '003'
                                       0
                                     CHANGING lwa_op_non_reg-press_rat.

    lv_objek = lwa_equi-tplnr.
    PERFORM get_characteristic_value USING lv_objek
                                       'DSGN_CAPACITY_STATUS'
                                       '003'
                                       0
                                     CHANGING lwa_op_regular-cap_stats.

    lwa_op_non_reg-cap_stats = lwa_op_regular-cap_stats.

* Get mesaurement doc. for liquid left
    PERFORM get_last_measure_reading USING lwa_equi-floc_obj
                                      'SET_PRESSURE_LEFT'
                                       space
                                     CHANGING lwa_op_regular-set_press
                                              lv_date.

* Get mesaurement doc. for liquid left
    PERFORM get_last_measure_reading USING lwa_equi-floc_obj
                                      'UOM_PRESSURE'
                                       abap_true
                                     CHANGING lv_press_uom
                                              lv_date.

    CONDENSE lv_press_uom.
    TRANSLATE lv_press_uom TO UPPER CASE.
    IF lv_press_uom EQ 'PSI'.
      lv_set_press = lwa_op_regular-set_press * 6895 / 1000.
    ELSEIF lv_press_uom EQ 'KPA'.
      lv_set_press = lwa_op_regular-set_press.
    ENDIF.
    lwa_op_regular-set_press = lv_set_press.

    lwa_op_non_reg-set_press = lwa_op_regular-set_press.

    CASE lwa_equi-eqart.
      WHEN 'SC_VA_RG' OR
           'SC_VA_CT'.
        APPEND lwa_op_regular TO git_op_regular.
      WHEN 'MD_MT'    OR
           'MD_MT_DI' OR
           'MD_MT_OR' OR
           'MD_MT_RO' OR
           'MD_MT_TU' OR
           'MD_MT_UT' OR
           'ME_FS'    OR
           'SC_VA_SC' OR
           'ME_FS_ST' OR
           'ME_FS_SE' OR
*BOI PANUSURI Ticket ACR-4558
           'ME_HB_DF' OR
           'ME_HB_CA' OR
           'ME_HB_EH' OR
           'ME_HB_FB' OR
           'ME_HB_IF' OR
           'ME_HB'.
*EOI PANUSURI Ticket ACR-4558
        APPEND lwa_op_non_reg TO git_op_non_reg.
    ENDCASE.
    CLEAR lwa_op_regular.
    CLEAR lwa_op_non_reg.
  ENDLOOP.

*  SORT git_op_regular BY equip_num.                           "SDP87326
*  SORT git_op_non_reg BY equip_num.                           "SDP87326

  SORT git_op_regular BY num_statn.                         "SDP87326
  SORT git_op_non_reg BY num_statn.                         "SDP87326

ENDFORM.                    " PREPARE_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_SCREEN
*&---------------------------------------------------------------------*
*       Display ALV
*----------------------------------------------------------------------*
FORM call_alv_screen .

  CALL SCREEN 100.

ENDFORM.                    " CALL_ALV_SCREEN
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_STATION
*&---------------------------------------------------------------------*
*       Validate Station
*----------------------------------------------------------------------*
FORM validate_station .

  DATA: lv_station TYPE iloa-stort.

  SELECT SINGLE stand FROM t499s
         INTO lv_station
         WHERE stand = p_stn.

  IF sy-subrc NE 0.
    MESSAGE 'Invalid Station. Please check your entry'(001) TYPE 'E'.
  ENDIF.

ENDFORM.                    " VALIDATE_STATION
*&---------------------------------------------------------------------*
*&      Form  GET_CHARACTERISTIC_VALUE
*&---------------------------------------------------------------------*
*       Get Characteristic Value
*----------------------------------------------------------------------*
FORM get_characteristic_value  USING    p_objek TYPE ausp-objek
                                        p_atnam TYPE cabn-atnam
                                        p_klart TYPE ausp-klart
                                        p_dec   TYPE i
                               CHANGING p_value TYPE any.

  FIELD-SYMBOLS: <lfs_cabn> TYPE gty_cabn,
                 <lfs_inob> TYPE gty_inob,
                 <lfs_ausp> TYPE gty_ausp.

  DATA: lv_char10 TYPE char10,
        lv_objek  TYPE ausp-objek.

  lv_objek = p_objek.

  CLEAR p_value.
  READ TABLE git_cabn ASSIGNING <lfs_cabn> WITH KEY atnam = p_atnam
                                           BINARY SEARCH.
  IF sy-subrc NE 0 OR <lfs_cabn> IS NOT ASSIGNED.
    RETURN.
  ENDIF.

  READ TABLE git_inob ASSIGNING <lfs_inob>
                      WITH KEY objek = lv_objek
                      BINARY SEARCH.

  IF sy-subrc EQ 0.
    lv_objek = <lfs_inob>-cuobj.
  ENDIF.

  READ TABLE git_ausp ASSIGNING <lfs_ausp> WITH KEY objek = lv_objek
                                                    atinn = <lfs_cabn>-atinn
                                                    klart = p_klart
                                                    BINARY SEARCH.
  IF sy-subrc NE 0 OR <lfs_ausp> IS NOT ASSIGNED.
    RETURN.
  ENDIF.

  CASE <lfs_cabn>-atfor.
    WHEN 'DATE'.
      WRITE <lfs_ausp>-atflv TO lv_char10 EXPONENT 0 DECIMALS 0.
      CONDENSE lv_char10.
      WRITE lv_char10 TO p_value.
    WHEN 'NUM'.
      WRITE <lfs_ausp>-atflv TO lv_char10 EXPONENT 0 DECIMALS p_dec.
      CONDENSE lv_char10.
      WRITE lv_char10 TO p_value.
    WHEN 'CHAR'.
      p_value = <lfs_ausp>-atwrt.
  ENDCASE.

ENDFORM.                    " GET_CHARACTERISTIC_VALUE
*&---------------------------------------------------------------------*
*&      Form  PREPARE_SUMMARY_OUTPUT
*&---------------------------------------------------------------------*
*       Prepare Summary
*----------------------------------------------------------------------*
FORM prepare_summary_output .

  DATA: lv_objek TYPE ausp-objek,
        lwa_op_summary  TYPE gty_op_summary.

  lwa_op_summary-premise_no =  gs_iflo-msgrp.

  lv_objek = gs_iflo-tplnr.
  PERFORM get_characteristic_value USING lv_objek
                                     'DSGN_CAP_STATUS'
                                     '003'
                                     0
                                   CHANGING lwa_op_summary-cap_ver_rq.

  PERFORM get_characteristic_value USING lv_objek
                                     'DSGNCAP_REVSD_DT'
                                     '003'
                                     0
                                   CHANGING lwa_op_summary-cap_rev_dt.

  PERFORM get_characteristic_value USING lv_objek
                                     'DESIGN_CODE1'
                                     '003'
                                     0
                                   CHANGING lwa_op_summary-st_dsn_cd1.

  PERFORM get_characteristic_value USING lv_objek
                                     'DESIGN_CODE2'
                                     '003'
                                     0
                                   CHANGING lwa_op_summary-st_dsn_cd2.

  PERFORM get_characteristic_value USING lv_objek
                                     'DESIGN_CODE3'
                                     '003'
                                     0
                                   CHANGING lwa_op_summary-st_dsn_cd3.

  PERFORM get_characteristic_value USING lv_objek
                                     'DESIGN_CODE4'
                                     '003'
                                     0
                                   CHANGING lwa_op_summary-st_dsn_cd4.

  IF lwa_op_summary IS NOT INITIAL.
    APPEND lwa_op_summary TO git_op_summary.
    CLEAR lwa_op_summary.
  ENDIF.

ENDFORM.                    " PREPARE_SUMMARY_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  PREPARE_CAPACITY_OUTPUT
*&---------------------------------------------------------------------*
*       Prepare Capacity Output
*----------------------------------------------------------------------*
FORM prepare_capacity_output .

  DATA: lwa_op_capacity TYPE gty_op_capacity,
        lv_objek TYPE ausp-objek.

  lv_objek = gs_iflo-tplnr.
  PERFORM get_characteristic_value USING lv_objek
                                     'STN_STAGE_1'
                                     '003'
                                     0
                                   CHANGING lwa_op_capacity-stn_stage.

  PERFORM get_characteristic_value USING lv_objek
                                     'INLET_MOP_1'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-inlet_mop.

  PERFORM get_characteristic_value USING lv_objek
                                     'INLET_MIN_1'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-inlet_min.

  PERFORM get_characteristic_value USING lv_objek
                                     'OUTLET_MOP_1'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-otlet_mop.

  PERFORM get_characteristic_value USING lv_objek
                                     'OUTLET_MIN_1'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-otlet_min.

  PERFORM get_characteristic_value USING lv_objek
                                     'MAX_PRESS_1'
                                     '003'
                                     2                      "ACR-2337
                                   CHANGING lwa_op_capacity-max_press.

  PERFORM get_characteristic_value USING lv_objek
                                     'CAPACITY_QUANTITY_1'
                                     '003'
                                     0
                                   CHANGING lwa_op_capacity-cap_quant.

  IF lwa_op_capacity IS NOT INITIAL.
    APPEND lwa_op_capacity TO git_op_capacity.
    CLEAR lwa_op_capacity.
  ENDIF.

  PERFORM get_characteristic_value USING lv_objek
                                     'STN_STAGE_2'
                                     '003'
                                     0
                                   CHANGING lwa_op_capacity-stn_stage.

  PERFORM get_characteristic_value USING lv_objek
                                     'INLET_MOP_2'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-inlet_mop.

  PERFORM get_characteristic_value USING lv_objek
                                     'INLET_MIN_2'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-inlet_min.

  PERFORM get_characteristic_value USING lv_objek
                                     'OUTLET_MOP_2'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-otlet_mop.

  PERFORM get_characteristic_value USING lv_objek
                                     'OUTLET_MIN_2'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-otlet_min.

  PERFORM get_characteristic_value USING lv_objek
                                     'MAX_PRESS_2'
                                     '003'
                                     2                      "ACR-2337
                                   CHANGING lwa_op_capacity-max_press.

  PERFORM get_characteristic_value USING lv_objek
                                     'CAPACITY_QUANTITY_2'
                                     '003'
                                     0
                                   CHANGING lwa_op_capacity-cap_quant.

  IF lwa_op_capacity IS NOT INITIAL.
    APPEND lwa_op_capacity TO git_op_capacity.
    CLEAR lwa_op_capacity.
  ENDIF.

  PERFORM get_characteristic_value USING lv_objek
                                     'STN_STAGE_3'
                                     '003'
                                     0
                                   CHANGING lwa_op_capacity-stn_stage.

  PERFORM get_characteristic_value USING lv_objek
                                     'INLET_MOP_3'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-inlet_mop.

  PERFORM get_characteristic_value USING lv_objek
                                     'INLET_MIN_3'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-inlet_min.

  PERFORM get_characteristic_value USING lv_objek
                                     'OUTLET_MOP_3'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-otlet_mop.

  PERFORM get_characteristic_value USING lv_objek
                                     'OUTLET_MIN_3'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-otlet_min.

  PERFORM get_characteristic_value USING lv_objek
                                     'MAX_PRESS_3'
                                     '003'
                                     2                      "ACR-2337
                                   CHANGING lwa_op_capacity-max_press.

  PERFORM get_characteristic_value USING lv_objek
                                     'CAPACITY_QUANTITY_3'
                                     '003'
                                     0
                                   CHANGING lwa_op_capacity-cap_quant.

  IF lwa_op_capacity IS NOT INITIAL.
    APPEND lwa_op_capacity TO git_op_capacity.
    CLEAR lwa_op_capacity.
  ENDIF.

  PERFORM get_characteristic_value USING lv_objek
                                     'STN_STAGE_4'
                                     '003'
                                     0
                                   CHANGING lwa_op_capacity-stn_stage.

  PERFORM get_characteristic_value USING lv_objek
                                     'INLET_MOP_4'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-inlet_mop.

  PERFORM get_characteristic_value USING lv_objek
                                     'INLET_MIN_4'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-inlet_min.

  PERFORM get_characteristic_value USING lv_objek
                                     'OUTLET_MOP_4'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-otlet_mop.

  PERFORM get_characteristic_value USING lv_objek
                                     'OUTLET_MIN_4'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-otlet_min.

  PERFORM get_characteristic_value USING lv_objek
                                     'MAX_PRESS_4'
                                     '003'
                                     2                      "ACR-2337
                                   CHANGING lwa_op_capacity-max_press.

  PERFORM get_characteristic_value USING lv_objek
                                     'CAPACITY_QUANTITY_4'
                                     '003'
                                     0
                                   CHANGING lwa_op_capacity-cap_quant.

  IF lwa_op_capacity IS NOT INITIAL.
    APPEND lwa_op_capacity TO git_op_capacity.
    CLEAR lwa_op_capacity.
  ENDIF.

  PERFORM get_characteristic_value USING lv_objek
                                     'STN_STAGE_5'
                                     '003'
                                     0
                                   CHANGING lwa_op_capacity-stn_stage.

  PERFORM get_characteristic_value USING lv_objek
                                     'INLET_MOP_5'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-inlet_mop.

  PERFORM get_characteristic_value USING lv_objek
                                     'INLET_MIN_5'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-inlet_min.

  PERFORM get_characteristic_value USING lv_objek
                                     'OUTLET_MOP_5'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-otlet_mop.

  PERFORM get_characteristic_value USING lv_objek
                                     'OUTLET_MIN_5'
                                     '003'
                                     2
                                   CHANGING lwa_op_capacity-otlet_min.

  PERFORM get_characteristic_value USING lv_objek
                                     'MAX_PRESS_5'
                                     '003'
                                     2                      "ACR-2337
                                   CHANGING lwa_op_capacity-max_press.

  PERFORM get_characteristic_value USING lv_objek
                                     'CAPACITY_QUANTITY_5'
                                     '003'
                                     0
                                   CHANGING lwa_op_capacity-cap_quant.

  IF lwa_op_capacity IS NOT INITIAL.
    APPEND lwa_op_capacity TO git_op_capacity.
    CLEAR lwa_op_capacity.
  ENDIF.

ENDFORM.                    " PREPARE_CAPACITY_OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  CONCATENATE 'Station Cap Report'(002)                     "ACR2106
              p_stn
              gv_ktext INTO gv_title SEPARATED BY space.

  SET PF-STATUS 'ZINDSTCAP'.
  SET TITLEBAR 'ZINDSTCAP' WITH gv_title.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display OUTPUT.

  IF gr_dock_cont IS INITIAL.

    PERFORM display_alv.

  ENDIF.
  CLEAR gv_okcode.

ENDMODULE.                 " DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'REFRESH'.
      PERFORM refresh_results.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .

  DATA: lwa_ref TYPE gty_refs.

  CREATE OBJECT gr_dock_cont
    EXPORTING
      side                        = cl_gui_docking_container=>dock_at_left
      extension                   = 99999
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CREATE OBJECT gr_splt_cont
    EXPORTING
      parent            = gr_dock_cont
      rows              = 5
      columns           = 1
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD gr_splt_cont->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = gr_grid1.

  CALL METHOD gr_splt_cont->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = gr_grid2.

  CALL METHOD gr_splt_cont->get_container
    EXPORTING
      row       = 3
      column    = 1
    RECEIVING
      container = gr_grid3.

  CALL METHOD gr_splt_cont->get_container
    EXPORTING
      row       = 4
      column    = 1
    RECEIVING
      container = gr_grid4.

  CALL METHOD gr_splt_cont->get_container
    EXPORTING
      row       = 5
      column    = 1
    RECEIVING
      container = gr_grid5.

  CALL METHOD gr_splt_cont->set_row_height
    EXPORTING
      id                = 1
      height            = 7
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD gr_splt_cont->set_row_height
    EXPORTING
      id                = 2
      height            = 12
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD gr_splt_cont->set_row_height
    EXPORTING
      id                = 3
      height            = 25
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD gr_splt_cont->set_row_height
    EXPORTING
      id                = 4
      height            = 25
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD gr_splt_cont->set_row_height
    EXPORTING
      id                = 5
      height            = 9
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Display ALV Summary
  PERFORM display_alv_summary.

* Display ALV CAP Details
  PERFORM display_alv_cap_details.

* Display ALV Regulator details
  PERFORM display_alv_reg_details.

* Display ALV Non Regulator details
  PERFORM display_alv_non_reg_details.

* Display Long text
  PERFORM display_long_text.

  GET REFERENCE OF git_op_summary INTO lwa_ref-tab.
  lwa_ref-ref = gr_salv1->get_event( ).
  APPEND lwa_ref TO git_refs.
  CLEAR lwa_ref.

  GET REFERENCE OF git_op_capacity INTO lwa_ref-tab.
  lwa_ref-ref = gr_salv2->get_event( ).
  APPEND lwa_ref TO git_refs.
  CLEAR lwa_ref.

  GET REFERENCE OF git_op_regular INTO lwa_ref-tab.
  lwa_ref-ref = gr_salv3->get_event( ).
  APPEND lwa_ref TO git_refs.
  CLEAR lwa_ref.

  GET REFERENCE OF git_op_non_reg INTO lwa_ref-tab.
  lwa_ref-ref = gr_salv4->get_event( ).
  APPEND lwa_ref TO git_refs.
  CLEAR lwa_ref.

  SET HANDLER event_class=>handle_double_click FOR ALL INSTANCES. "register the event handler

ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_SUMMARY
*&---------------------------------------------------------------------*
*       Station Capacity Summary
*----------------------------------------------------------------------*
FORM display_alv_summary .

  DATA: lv_ltext TYPE scrtext_l,
        lv_mtext TYPE scrtext_m,
        lv_stext TYPE scrtext_s.

  TRY.

      cl_salv_table=>factory(
          EXPORTING
            r_container    = gr_grid1
          IMPORTING
            r_salv_table   = gr_salv1
          CHANGING
            t_table        = git_op_summary
               ).

      gr_display1 = gr_salv1->get_display_settings( ).
      gr_display1->set_list_header( 'Station Capacity Summary'(004) ).
      gr_columns1 = gr_salv1->get_columns( ).

      lv_ltext = 'Premise Number'(005).
      lv_mtext = 'Premise Number'(005).
      lv_stext = 'Premise'(006).
      gr_column1 = gr_columns1->get_column('PREMISE_NO').
      gr_column1->set_long_text( lv_ltext ).
      gr_column1->set_medium_text( lv_mtext ).
      gr_column1->set_short_text( lv_stext ).
      gr_column1->set_output_length( 15 ).

      lv_ltext = 'Capacity Verification Req.'(007).
      lv_mtext = 'Cap Ver Req'(008).
      lv_stext = 'Cp Ver Rq'(009).
      gr_column1 = gr_columns1->get_column('CAP_VER_RQ').
      gr_column1->set_long_text( lv_ltext ).
      gr_column1->set_medium_text( lv_mtext ).
      gr_column1->set_short_text( lv_stext ).
      gr_column1->set_output_length( 15 ).

      lv_ltext = 'Capacity Revised Date'(010).
      lv_mtext = 'Capacity Revised Date'(010).
      lv_stext = 'Cp Rev Dt'(011).
      gr_column1 = gr_columns1->get_column('CAP_REV_DT').
      gr_column1->set_long_text( lv_ltext ).
      gr_column1->set_medium_text( lv_mtext ).
      gr_column1->set_short_text( lv_stext ).
      gr_column1->set_output_length( 15 ).

      lv_ltext = 'Station Design Code 1'(012).
      lv_mtext = 'St. Design Code 1'(013).
      lv_stext = 'St Dsn Cd1'(014).
      gr_column1 = gr_columns1->get_column('ST_DSN_CD1').
      gr_column1->set_long_text( lv_ltext ).
      gr_column1->set_medium_text( lv_mtext ).
      gr_column1->set_short_text( lv_stext ).
      gr_column1->set_output_length( 15 ).

      lv_ltext = 'Station Design Code 2'(015).
      lv_mtext = 'St. Design Code 2'(016).
      lv_stext = 'St Dsn Cd2'(017).
      gr_column1 = gr_columns1->get_column('ST_DSN_CD2').
      gr_column1->set_long_text( lv_ltext ).
      gr_column1->set_medium_text( lv_mtext ).
      gr_column1->set_short_text( lv_stext ).
      gr_column1->set_output_length( 15 ).

      lv_ltext = 'Station Design Code 3'(018).
      lv_mtext = 'St. Design Code 3'(019).
      lv_stext = 'St Dsn Cd3'(020).
      gr_column1 = gr_columns1->get_column('ST_DSN_CD3').
      gr_column1->set_long_text( lv_ltext ).
      gr_column1->set_medium_text( lv_mtext ).
      gr_column1->set_short_text( lv_stext ).
      gr_column1->set_output_length( 15 ).

      lv_ltext = 'Station Design Code 4'(021).
      lv_mtext = 'St. Design Code 4'(022).
      lv_stext = 'St Dsn Cd4'(023).
      gr_column1 = gr_columns1->get_column('ST_DSN_CD4').
      gr_column1->set_long_text( lv_ltext ).
      gr_column1->set_medium_text( lv_mtext ).
      gr_column1->set_short_text( lv_stext ).
      gr_column1->set_output_length( 15 ).

      CALL METHOD gr_salv1->display.
    CATCH cx_salv_msg.
    CATCH cx_salv_not_found.
  ENDTRY.

ENDFORM.                    " DISPLAY_ALV_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_CAP_DETAILS
*&---------------------------------------------------------------------*
*       Display ALV CAP Details
*----------------------------------------------------------------------*
FORM display_alv_cap_details .

  DATA: lv_ltext TYPE scrtext_l,
        lv_mtext TYPE scrtext_m,
        lv_stext TYPE scrtext_s.

  TRY.
      cl_salv_table=>factory(
          EXPORTING
            r_container    = gr_grid2
          IMPORTING
            r_salv_table   = gr_salv2
          CHANGING
            t_table        = git_op_capacity
               ).

      gr_display2 = gr_salv2->get_display_settings( ).
      gr_display2->set_list_header( 'Station Capacity Details'(065) ).
      gr_columns2 = gr_salv2->get_columns( ).

      lv_ltext = 'Station Stage'(024).
      lv_mtext = 'Station Stage'(024).
      lv_stext = 'StnStage'(070).
      gr_column2 = gr_columns2->get_column('STN_STAGE').
      gr_column2->set_long_text( lv_ltext ).
      gr_column2->set_medium_text( lv_mtext ).
      gr_column2->set_short_text( lv_stext ).
      gr_column2->set_output_length( 15 ).

      lv_ltext = 'Inlet MOP(kPa)'(026).
      lv_mtext = 'Inlet MOP(kPa)'(026).
      lv_stext = 'InMOP(kPa)'(071).
      gr_column2 = gr_columns2->get_column('INLET_MOP').
      gr_column2->set_long_text( lv_ltext ).
      gr_column2->set_medium_text( lv_mtext ).
      gr_column2->set_short_text( lv_stext ).
      gr_column2->set_output_length( 15 ).

      lv_ltext = 'Inlet Min(kPa)'(027).
      lv_mtext = 'Inlet Min(kPa)'(027).
      lv_stext = 'InMin(kPa)'(072).
      gr_column2 = gr_columns2->get_column('INLET_MIN').
      gr_column2->set_long_text( lv_ltext ).
      gr_column2->set_medium_text( lv_mtext ).
      gr_column2->set_short_text( lv_stext ).
      gr_column2->set_output_length( 15 ).

      lv_ltext = 'Outlet MOP(kPa)'(028).
      lv_mtext = 'Outlet MOP(kPa)'(028).
      lv_stext = 'OtMOP(kPa)'(073).
      gr_column2 = gr_columns2->get_column('OTLET_MOP').
      gr_column2->set_long_text( lv_ltext ).
      gr_column2->set_medium_text( lv_mtext ).
      gr_column2->set_short_text( lv_stext ).
      gr_column2->set_output_length( 15 ).

      lv_ltext = 'Outlet Min(kPa)'(029).
      lv_mtext = 'Outlet Min(kPa)'(029).
      lv_stext = 'OtMin(kPa)'(074).
      gr_column2 = gr_columns2->get_column('OTLET_MIN').
      gr_column2->set_long_text( lv_ltext ).
      gr_column2->set_medium_text( lv_mtext ).
      gr_column2->set_short_text( lv_stext ).
      gr_column2->set_output_length( 15 ).

      lv_ltext = 'Max Sus. Pressure(kPa)'(030).
      lv_mtext = 'Max Sus. Pressure(kPa)'(030).
      lv_stext = 'MxSuP(kPa)'(075).
      gr_column2 = gr_columns2->get_column('MAX_PRESS').
      gr_column2->set_long_text( lv_ltext ).
      gr_column2->set_medium_text( lv_mtext ).
      gr_column2->set_short_text( lv_stext ).
      gr_column2->set_output_length( 25 ).

      lv_ltext = 'Stn. Stg. Capacity(sm3/h)'(032).
      lv_mtext = 'Stn. Stg. Capacity(sm3/h)'(032).
      lv_stext = 'StgCp(sm3h)'(076).
      gr_column2 = gr_columns2->get_column('CAP_QUANT').
      gr_column2->set_long_text( lv_ltext ).
      gr_column2->set_medium_text( lv_mtext ).
      gr_column2->set_short_text( lv_stext ).
      gr_column2->set_output_length( 25 ).

      CALL METHOD gr_salv2->display.
    CATCH cx_salv_msg.
    CATCH cx_salv_not_found.
  ENDTRY.

ENDFORM.                    " DISPLAY_ALV_CAP_DETAILS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REG_DETAILS
*&---------------------------------------------------------------------*
*       Station Regular Details
*----------------------------------------------------------------------*
FORM display_alv_reg_details .

  DATA: lv_ltext TYPE scrtext_l,
        lv_mtext TYPE scrtext_m,
        lv_stext TYPE scrtext_s.

  DATA ls_layout_key TYPE salv_s_layout_key.

  TRY.
      cl_salv_table=>factory(
          EXPORTING
            r_container    = gr_grid3
          IMPORTING
            r_salv_table   = gr_salv3
          CHANGING
            t_table        = git_op_regular
               ).

      gr_layout3 = gr_salv3->get_layout( ).
      ls_layout_key-report = sy-repid.
      ls_layout_key-handle = 'REG'.
      gr_layout3->set_key( ls_layout_key ).
      gr_layout3->set_save_restriction( if_salv_c_layout=>restrict_user_dependant ).
      gr_layout3->set_default( if_salv_c_bool_sap=>true ).

      gr_alv_functions3 = gr_salv3->get_functions( ).
      gr_alv_functions3->set_all( abap_true ).
      gr_display3 = gr_salv3->get_display_settings( ).
      gr_display3->set_list_header( 'Station Regulator and Control Valves Details'(066) ).
      gr_columns3 = gr_salv3->get_columns( ).

      lv_ltext = 'Num at Station'(068).
      lv_ltext = 'Num at Station'(068).
      lv_stext = 'Num@Statn'(077).
      gr_column3 = gr_columns3->get_column('NUM_STATN').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_short_text( lv_stext ).
      gr_column3->set_output_length( 13 ).

      lv_ltext = 'Equipment Number'(034).
      lv_mtext = 'Equipment Number'(034).
      lv_stext = 'Equipment'(035).
      gr_column3 = gr_columns3->get_column('EQUIP_NUM').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_short_text( lv_stext ).
      gr_column3->set_output_length( 15 ).

      lv_ltext = 'Equipment Text'(036).
      lv_mtext = 'Equipment Text'(036).
      lv_stext = 'EquipText'(037).
      gr_column3 = gr_columns3->get_column('EQUIP_TXT').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_short_text( lv_stext ).
      gr_column3->set_output_length( 15 ).
      gr_column3->set_visible( value  = if_salv_c_bool_sap=>false ).

      lv_ltext = 'Equipment Type'(052).
      lv_mtext = 'Equipment Type'(052).
      lv_stext = 'EquipType'(053).
      gr_column3 = gr_columns3->get_column('EQUIP_TYP').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_short_text( lv_stext ).
      gr_column3->set_output_length( 13 ).
      gr_column3->set_visible( value  = if_salv_c_bool_sap=>false ).

      lv_ltext = 'Equipment Model'(038).
      lv_mtext = 'Equipment Model'(038).
      lv_stext = 'EquipModel'(039).
      gr_column3 = gr_columns3->get_column('EQUIP_MOD').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_short_text( lv_stext ).
      gr_column3->set_output_length( 15 ).

      lv_ltext = 'Body Size'(040).
      lv_mtext = 'Body Size'(040).
      lv_stext = 'Body Size'(040).
      gr_column3 = gr_columns3->get_column('BODY_SIZE').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_short_text( lv_stext ).
      gr_column3->set_output_length( 13 ).

      lv_ltext = 'Orifice(mm)'(041).
      lv_mtext = 'Orifice(mm)'(041).
      lv_stext = 'Orfice(mm)'(083).
      gr_column3 = gr_columns3->get_column('ORIF_SIZE').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_short_text( lv_stext ).
      gr_column3->set_output_length( 13 ).

      lv_ltext = 'Liner(%)'(087).                           "SDP86055
      lv_mtext = 'Liner(%)'(087).                           "SDP86055
      lv_stext = 'Liner(%)'(087).                           "SDP86055
      gr_column3 = gr_columns3->get_column('LINER').        "SDP86055
      gr_column3->set_long_text( lv_ltext ).                "SDP86055
      gr_column3->set_medium_text( lv_mtext ).              "SDP86055
      gr_column3->set_short_text( lv_stext ).               "SDP86055
      gr_column3->set_output_length( 13 ).                  "SDP86055

      lv_ltext = 'Set Pressure(kPa)'(044).
      lv_mtext = 'Set Pressure(kPa)'(044).
      lv_stext = 'SetPr(kPa)'(079).
      gr_column3 = gr_columns3->get_column('SET_PRESS').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_short_text( lv_stext ).
      gr_column3->set_output_length( 14 ).
*BOI PANUSURI Ticket ACR-4558
      lv_ltext = 'Station Set Press'(025).
      lv_mtext = 'Station Set Press'(025).
      lv_stext = 'Station SetPr'(031).
      gr_column3 = gr_columns3->get_column('STAT_SET_PRESS').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_short_text( lv_stext ).
      gr_column3->set_output_length( 14 ).
*EOI PANUSURI Ticket ACR-4558
      lv_ltext = 'Inlet MOP(kPa)'(026).
      lv_mtext = 'Inlet MOP(kPa)'(026).
      lv_stext = 'InMOP(kPa)'(071).
      gr_column3 = gr_columns3->get_column('INLET_MOP').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_output_length( 13 ).

      lv_ltext = 'Inlet Min(kPa)'(027).
      lv_mtext = 'Inlet Min(kPa)'(027).
      lv_stext = 'InMin(kPa)'(072).
      gr_column3 = gr_columns3->get_column('INLET_MIN').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_short_text( lv_stext ).
      gr_column3->set_output_length( 13 ).

      lv_ltext = 'Outlet MOP(kPa)'(028).
      lv_mtext = 'Outlet MOP(kPa)'(028).
      lv_stext = 'OtMOP(kPa)'(073).
      gr_column3 = gr_columns3->get_column('OTLET_MOP').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_short_text( lv_stext ).
      gr_column3->set_output_length( 13 ).

      lv_ltext = 'Outlet Min(kPa)'(029).
      lv_mtext = 'Outlet Min(kPa)'(029).
      lv_stext = 'OtMIN(kPa)'(084).
      gr_column3 = gr_columns3->get_column('OTLET_MIN').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_short_text( lv_stext ).
      gr_column3->set_output_length( 13 ).

      lv_ltext = 'Design Flow(sm3/h)'(046).
      lv_mtext = 'Design Flow(sm3/h)'(046).
      lv_stext = 'DnFl(sm3h)'(085).
      gr_column3 = gr_columns3->get_column('DESGN_FLW').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_short_text( lv_stext ).
      gr_column3->set_output_length( 13 ).

      lv_ltext = 'Fail Open(sm3/h)'(048).
      lv_mtext = 'Fail Open(sm3/h)'(048).
      lv_stext = 'FlOp(sm3h)'(086).
      gr_column3 = gr_columns3->get_column('FAIL_OPEN').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_short_text( lv_stext ).
      gr_column3->set_output_length( 13 ).

      lv_ltext = 'Capacity Status'(050).
      lv_mtext = 'Capacity Status'(050).
      lv_stext = 'CapStatus'(082).
      gr_column3 = gr_columns3->get_column('CAP_STATS').
      gr_column3->set_long_text( lv_ltext ).
      gr_column3->set_medium_text( lv_mtext ).
      gr_column3->set_short_text( lv_stext ).
      gr_column3->set_output_length( 13 ).

      CALL METHOD gr_salv3->display.
    CATCH cx_salv_msg.
    CATCH cx_salv_not_found.
  ENDTRY.

ENDFORM.                    " DISPLAY_ALV_REG_DETAILS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_NON_REG_DETAILS
*&---------------------------------------------------------------------*
*       Non Regulator Details
*----------------------------------------------------------------------*
FORM display_alv_non_reg_details .

  DATA: lv_ltext TYPE scrtext_l,
        lv_mtext TYPE scrtext_m,
        lv_stext TYPE scrtext_s.

  DATA ls_layout_key TYPE salv_s_layout_key.

  TRY.
      cl_salv_table=>factory(
          EXPORTING
            r_container    = gr_grid4
          IMPORTING
            r_salv_table   = gr_salv4
          CHANGING
            t_table        = git_op_non_reg
               ).

      gr_layout4 = gr_salv4->get_layout( ).
      ls_layout_key-report = sy-repid.
      ls_layout_key-handle = 'NON_REG'.
      gr_layout4->set_key( ls_layout_key ).
      gr_layout4->set_save_restriction( if_salv_c_layout=>restrict_user_dependant ).
      gr_layout4->set_default( if_salv_c_bool_sap=>true ).

      gr_alv_functions4 = gr_salv4->get_functions( ).
      gr_alv_functions4->set_all( abap_true ).
      gr_display4 = gr_salv4->get_display_settings( ).
      gr_display4->set_list_header( 'Non-Regulator Details'(067) ).
      gr_columns4 = gr_salv4->get_columns( ).

      lv_ltext = 'Num at Station'(068).
      lv_ltext = 'Num at Station'(068).
      lv_stext = 'Num@Statn'(077).
      gr_column4 = gr_columns4->get_column('NUM_STATN').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 13 ).

      lv_ltext = 'Equipment Number'(034).
      lv_mtext = 'Equipment Number'(034).
      lv_stext = 'Equipment'(035).
      gr_column4 = gr_columns4->get_column('EQUIP_NUM').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 15 ).

      lv_ltext = 'Equipment Text'(036).
      lv_mtext = 'Equipment Text'(036).
      lv_stext = 'EquipText'(037).
      gr_column4 = gr_columns4->get_column('EQUIP_TXT').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 15 ).

      lv_ltext = 'Equipment Type'(052).
      lv_mtext = 'Equipment Type'(052).
      lv_stext = 'EquipType'(053).
      gr_column4 = gr_columns4->get_column('EQUIP_TYP').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 13 ).
      gr_column4->set_visible( value  = if_salv_c_bool_sap=>false ).

      lv_ltext = 'Location to Reg'(069).
      lv_mtext = 'Location to Reg'(069).
      lv_stext = 'LoctnToReg'(078).
      gr_column4 = gr_columns4->get_column('LOCATEREG').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 13 ).

      lv_ltext = 'Equipment Type text'(054).
      lv_mtext = 'Equipment Type Text'(055).
      lv_stext = 'Type Text'(056).
      gr_column4 = gr_columns4->get_column('EQ_TYP_TX').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 15 ).
      gr_column4->set_visible( value  = if_salv_c_bool_sap=>false ).

      lv_ltext = 'IGRP Code'(057).
      lv_mtext = 'IGRP Code'(057).
      lv_stext = 'IGRP Code'(057).
      gr_column4 = gr_columns4->get_column('IGRP_CODE').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 15 ).
      gr_column4->set_visible( value  = if_salv_c_bool_sap=>false ).

      lv_ltext = 'Equipment Model'(038).
      lv_mtext = 'Equipment Model'(038).
      lv_stext = 'EquipModel'(039).
      gr_column4 = gr_columns4->get_column('EQUIP_MOD').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 15 ).

      lv_ltext = 'Protect Equip. Num'(058).
      lv_mtext = 'Protect Equip. Num'(058).
      lv_stext = 'Prot Eqp No'(059).
      gr_column4 = gr_columns4->get_column('PROT_EQNO').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 15 ).

      lv_ltext = 'Body Size'(040).
      lv_mtext = 'Body Size'(040).
      lv_stext = 'Body Size'(040).
      gr_column4 = gr_columns4->get_column('BODY_SIZE').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 13 ).

      lv_ltext = 'Isolating Valve Size'(088).               "SDP87326
      lv_mtext = 'Isolating Valve Size'(088).               "SDP87326
      lv_stext = 'Iso. Valve Size'(089).                    "SDP87326
      gr_column4 = gr_columns4->get_column('ISO_VALVE').    "SDP87326
      gr_column4->set_long_text( lv_ltext ).                "SDP87326
      gr_column4->set_medium_text( lv_mtext ).              "SDP87326
      gr_column4->set_short_text( lv_stext ).               "SDP87326
      gr_column4->set_output_length( 15 ).                  "SDP87326

      lv_ltext = 'Set Pressure(kPa)'(044).
      lv_mtext = 'Set Pressure(kPa)'(044).
      lv_stext = 'SetPr(kPa)'(079).
      gr_column4 = gr_columns4->get_column('SET_PRESS').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 14 ).

*BOI PANUSURI Ticket ACR-4558
      lv_ltext = 'Station Set Press'(025).
      lv_mtext = 'Station Set Press'(025).
      lv_stext = 'Station SetPr'(031).
      gr_column4 = gr_columns4->get_column('STAT_SET_PRESS').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 14 ).
*EOI PANUSURI Ticket ACR-4558

      lv_ltext = 'Rated Capacity(sm3/h)'(060).
      lv_mtext = 'Rated Capacity(sm3/h)'(060).
      lv_stext = 'RtCp(sm3h)'(080).
      gr_column4 = gr_columns4->get_column('RATED_CAP').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 20 ).

      lv_ltext = 'Pressure@Rating'(063).
      lv_mtext = 'Pressure@Rating'(063).
      lv_stext = 'Press@Rate'(081).
      gr_column4 = gr_columns4->get_column('PRESS_RAT').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 13 ).

      lv_ltext = 'Capacity Status'(050).
      lv_mtext = 'Capacity Status'(050).
      lv_stext = 'CapStatus'(082).
      gr_column4 = gr_columns4->get_column('CAP_STATS').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 13 ).
*BOI PANUSURI Ticket ACR-4558
      lv_ltext = 'HE Design Flow Capacity'(033).
      lv_mtext = 'HE Design Flow Capacity'(033).
      lv_stext = 'DnFlCp'(042).
      gr_column4 = gr_columns4->get_column('DSGN_FLOW_CAP').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 20 ).

      lv_ltext = 'HE Pressure Design Rating'(043).
      lv_mtext = 'HE Pressure Design Rating'(043).
      lv_stext = 'PressDnRate'(045).
      gr_column4 = gr_columns4->get_column('PRESS_DSGN_RAT').
      gr_column4->set_long_text( lv_ltext ).
      gr_column4->set_medium_text( lv_mtext ).
      gr_column4->set_short_text( lv_stext ).
      gr_column4->set_output_length( 20 ).
*EOI PANUSURI Ticket ACR-4558

      CALL METHOD gr_salv4->display.
    CATCH cx_salv_msg.
    CATCH cx_salv_not_found.
  ENDTRY.

ENDFORM.                    " DISPLAY_ALV_NON_REG_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GET_LAST_MEASURE_READING
*&---------------------------------------------------------------------*
*       Get Measurement Reading
*----------------------------------------------------------------------*
FORM get_last_measure_reading  USING    p_objnr TYPE equi-objnr
                                        p_psort TYPE imptt-psort
                                        p_vlcod TYPE char1
                               CHANGING p_value TYPE any
                                        p_idate TYPE sy-datum.

  DATA: wa_imrg  TYPE gty_imrg,
        wa_imptt TYPE gty_imptt.

  READ TABLE git_imptt INTO wa_imptt WITH KEY mpobj = p_objnr
                                              psort = p_psort
                                              BINARY SEARCH.
  IF sy-subrc EQ 0.
    READ TABLE git_imrg INTO wa_imrg WITH KEY point = wa_imptt-point
                                              BINARY SEARCH.
    IF sy-subrc EQ 0 AND p_vlcod IS INITIAL.
      p_value = wa_imrg-recdv.
      p_idate = wa_imrg-idate.
    ELSEIF sy-subrc EQ 0 AND p_vlcod IS NOT INITIAL.
      p_value = wa_imrg-vlcod.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_LAST_MEASURE_READING
*&---------------------------------------------------------------------*
*&      Form  REFRESH_RESULTS
*&---------------------------------------------------------------------*
*       Refresh Results
*----------------------------------------------------------------------*
FORM refresh_results .

  REFRESH: git_op_summary,
           git_op_capacity,
           git_op_regular,
           git_op_non_reg,

           git_imptt,
           git_imrg,
           git_equi,
           git_objek,
           git_ausp,
           git_inob,
           git_cabn,
           git_eqart.

* Get Data
  PERFORM get_data.

* Prepare Output table
  PERFORM prepare_output.

  gr_salv1->refresh( ).
  gr_salv2->refresh( ).
  gr_salv3->refresh( ).
  gr_salv4->refresh( ).
  PERFORM display_long_text.

ENDFORM.                    " REFRESH_RESULTS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LONG_TEXT
*&---------------------------------------------------------------------*
*       Display Long text
*----------------------------------------------------------------------*
FORM display_long_text .

  DATA: lv_name  TYPE thead-tdname,
        lt_lines TYPE STANDARD TABLE OF tline,
        ls_long_text    TYPE gty_long_text.

  FIELD-SYMBOLS <ls_lines> TYPE tline.

  IF gr_text_edit IS INITIAL.
    CREATE OBJECT gr_text_edit
      EXPORTING
        parent                     = gr_grid5
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position          = 132
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

    CALL METHOD gr_text_edit->set_statusbar_mode
      EXPORTING
        statusbar_mode = cl_gui_textedit=>false.

    CALL METHOD gr_text_edit->set_readonly_mode
      EXPORTING
        readonly_mode          = 1
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD gr_text_edit->set_wordwrap_behavior
      EXPORTING
        wordwrap_to_linebreak_mode = 1
      EXCEPTIONS
        error_cntl_call_method     = 1
        OTHERS                     = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  lv_name = gs_iflo-tplnr.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = 'LTXT'
      language                = sy-langu
      name                    = lv_name
      object                  = 'IFLOT'
    TABLES
      lines                   = lt_lines
    EXCEPTIONS
      id                      = 0
      language                = 0
      name                    = 0
      not_found               = 0
      object                  = 0
      reference_check         = 0
      wrong_access_to_archive = 0
      OTHERS                  = 0.

  REFRESH git_long_text.
  IF lt_lines IS NOT INITIAL.
    LOOP AT lt_lines ASSIGNING <ls_lines>.
      ls_long_text-long_text = <ls_lines>-tdline.
      APPEND ls_long_text TO git_long_text.
      CLEAR ls_long_text.
    ENDLOOP.
  ENDIF.

  CALL METHOD gr_text_edit->set_text_as_stream
    EXPORTING
      text            = git_long_text
    EXCEPTIONS
      error_dp        = 1
      error_dp_create = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY_LONG_TEXT
