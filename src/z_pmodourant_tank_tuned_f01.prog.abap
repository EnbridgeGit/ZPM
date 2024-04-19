*&---------------------------------------------------------------------*
*&  Include           Z_PMODOURANT_TANK_TOP
* Author             : Shiladitya Ghosh                                *
* Date               : 17/07/2014 (dd/mm/yyyy)                         *
* Technical Contact  : Shiladitya Ghosh                                *
* Business Contact   : Eric VanRumybeke                                *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By           CTS          Description           *
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


*----------------------------------------------------------------------*
* CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS event_class IMPLEMENTATION.
  METHOD handle_double_click.

    DATA: wa_final TYPE ty_final.

    READ TABLE ta_final INTO wa_final INDEX row.
    IF sy-subrc EQ 0.
      SET PARAMETER ID 'EQN' FIELD wa_final-equipment.
      CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN.
    ENDIF.

  ENDMETHOD.                    "handle_double_click

ENDCLASS. "lcl_event_receiver IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV1
*&---------------------------------------------------------------------*
*       Display Primary ALV
*----------------------------------------------------------------------*
FORM display_alv1 .

  DATA: lv_title TYPE lvc_title,
        lv_ltext TYPE scrtext_l,
        lv_mtext TYPE scrtext_m,
        lv_stext TYPE scrtext_s.

  DATA ls_layout_key TYPE salv_s_layout_key.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = rf_alv
        CHANGING
          t_table      = ta_final.
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  rf_columns = rf_alv->get_columns( ).
  rf_display = rf_alv->get_display_settings( ).

*   set ZEBRA pattern
  rf_display->set_striped_pattern( abap_true ).

* Title of ALV
  WRITE p_date TO lv_title.
  CONCATENATE 'Odourant Tank Levels'(001) '-' lv_title INTO lv_title SEPARATED BY space.

*   Title to ALV
  rf_display->set_list_header( lv_title ).

  TRY.

      lv_ltext = 'Station ID'(004).
      lv_mtext = 'Station ID'(004).
      lv_stext = 'Station ID'(004).
      rf_column = rf_columns->get_column('STATIONID').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Equipment'(013).
      lv_mtext = 'Equipment'(013).
      lv_stext = 'Equipment'(013).
      rf_column = rf_columns->get_column('EQUIP_DESC').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Equipment No.'(014).
      lv_ltext = 'Equipment No.'(014).
      lv_ltext = 'Equip. No.'(015).
      rf_column = rf_columns->get_column('EQUIPMENT').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).
      rf_column->set_visible( value  = if_salv_c_bool_sap=>false ).

      lv_ltext = 'Messages'(016).
      lv_mtext = 'Messages'(016).
      lv_stext = 'Messages'(016).
      rf_column = rf_columns->get_column('MESSAGE').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).
      rf_column->set_visible( value  = if_salv_c_bool_sap=>false ).

      lv_ltext = 'Station Name'(005).
      lv_mtext = 'Station Name'(005).
      lv_stext = 'St. Name'(017).
      rf_column = rf_columns->get_column('STATIONNAME').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Technician Responsible'(018).
      lv_mtext = 'Techn. Resp.'(019).
      lv_stext = 'Technician'(020).

      rf_column = rf_columns->get_column('TECHRESPN').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Tank Type'(021).
      lv_mtext = 'Tank Type'(021).
      lv_stext = 'Tank Type'(021).
      rf_column = rf_columns->get_column('TANK_TYPE').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).
      rf_column->set_visible( value  = if_salv_c_bool_sap=>false ).

      lv_ltext = 'Head Type'(022).
      lv_mtext = 'Head Type'(022).
      lv_stext = 'Head Type'(022).
      rf_column = rf_columns->get_column('HEAD_TYPE').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Capacity'(008).
      lv_mtext = 'Capacity'(008).
      lv_stext = 'Capacity'(008).
      rf_column = rf_columns->get_column('CAPACITY').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Capacity UOM'(023).
      lv_mtext = 'Capacity UOM'(023).
      lv_stext = 'UOM'(024).
      rf_column = rf_columns->get_column('CAPACITY_UOM').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Liquid Left'(025).
      lv_mtext = 'Liquid Left'(025).
      lv_stext = 'Left'(009).
      rf_column = rf_columns->get_column('LEFT').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Tank Level UOM'(002).
      lv_mtext = 'Tank Level UOM'(002).
      lv_stext = 'UoM'(003).
      rf_column = rf_columns->get_column('LEVEL_UOM').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Measurement Date'(026).
      lv_mtext = 'Measurement Date'(026).
      lv_stext = 'Date'(027).
      rf_column = rf_columns->get_column('MEASURE_DATE').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Volume'(010).
      lv_mtext = 'Volume'(010).
      lv_stext = 'Volume'(010).
      rf_column = rf_columns->get_column('VOLUME').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = 'Fill'(028).
      lv_mtext = 'Fill'(028).
      lv_stext = 'Fill'(028).
      rf_column = rf_columns->get_column('FILL').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      lv_ltext = '%'(012).
      lv_mtext = '%'(012).
      lv_stext = '%'(012).
      rf_column = rf_columns->get_column('%').
      rf_column->set_long_text( lv_ltext ).
      rf_column->set_medium_text( lv_mtext ).
      rf_column->set_short_text( lv_stext ).

      rf_columns->set_optimize( 'X' ).

    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  rf_events = rf_alv->get_event( ).
  CREATE OBJECT rf_event.
  SET HANDLER rf_event->handle_double_click FOR rf_events. "register the event handler
*** Functions
  rf_alv_functions = rf_alv->get_functions( ).
  rf_alv_functions->set_all( abap_true ).

* Allow users to Save layout
  rf_layout = rf_alv->get_layout( ).
  ls_layout_key-report = sy-repid.
  rf_layout->set_key( ls_layout_key ).
  rf_layout->set_save_restriction( if_salv_c_layout=>restrict_user_dependant ).
  rf_layout->set_default( if_salv_c_bool_sap=>true ).

** Display the table
  rf_alv->display( ).

ENDFORM.                    " DISPLAY_ALV1

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Select Data
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lv_atinn TYPE atinn,
        lv_objek TYPE ausp-objek,
        wa_equi  TYPE ty_equi,
        wa_cabn  TYPE ty_cabn,
        wa_atinn LIKE LINE OF ta_atinn,
        wa_objek TYPE ty_objek,
        wa_ausp  TYPE ty_ausp.

  FIELD-SYMBOLS: <fs_equi> TYPE ty_equi,
                 <fs_ausp> TYPE ty_ausp,
                 <fs_inob> TYPE ty_inob.

* Get Equipments
  SELECT equnr
         eqart
         datab
         objnr
         datbi
         pm_objty
         gewrk
         eqktx
         tplnr
         swerk AS werks
         stort
         INTO TABLE ta_equi
         FROM v_equi
         WHERE equnr IN s_equnr  AND
               spras EQ sy-langu AND
*               datbi GT sy-datum AND
*               datab LE sy-datum AND
               stort IN s_staton.

  LOOP AT ta_equi ASSIGNING <fs_equi>.
    IF NOT ( <fs_equi>-datbi GT sy-datum AND <fs_equi>-datab LE sy-datum ).
      <fs_equi>-del = abap_true.
    ENDIF.
  ENDLOOP.

  SORT ta_equi BY del.
  DELETE ta_equi WHERE del = abap_true.

  IF ta_equi IS INITIAL.
    MESSAGE 'No equipments identified'(029) TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

* Get Characteristics
  SELECT atinn
         atnam
         atfor
         FROM cabn
         INTO TABLE ta_cabn
         WHERE atnam IN ('TANK_MEASUREMENT_UOM',
                         'WALL_THICKNESS',
                         'EXTERNAL_DIAMETER',
                         'CYLINDER_LENGTH',
                         'HEAD_TYPE',
                         'ODOURANT_TANK',
                         'TANK_CAPACITY_UOM',
                         'TANK_CAPACITY',
                         'TANK_TYPE').

  IF sy-subrc EQ 0.
    SORT ta_cabn BY atnam.
  ENDIF.

  LOOP AT ta_cabn INTO wa_cabn.
    wa_atinn-low = wa_cabn-atinn.
    wa_atinn-option = 'EQ'.
    wa_atinn-sign = 'I'.
    APPEND wa_atinn TO ta_atinn.
    CLEAR wa_atinn.
  ENDLOOP.

* Prepare Object Ids to retrieve characteristics
  LOOP AT ta_equi INTO wa_equi.
    wa_objek-objek = wa_equi-equnr.
    APPEND wa_objek TO ta_objek.
  ENDLOOP.

  SELECT cuobj
         objek
         FROM inob
         INTO TABLE ta_inob
         FOR ALL ENTRIES IN ta_objek
         WHERE obtab = 'EQUI' AND
               objek = ta_objek-objek AND
               klart = '002'.

  IF sy-subrc EQ 0.
    SORT ta_inob BY objek.
    LOOP AT ta_inob ASSIGNING <fs_inob>.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_inob>-cuobj
        IMPORTING
          output = <fs_inob>-cuobj.

      wa_objek-objek = <fs_inob>-cuobj.
      APPEND wa_objek TO ta_objek.
    ENDLOOP.
  ENDIF.

  SORT ta_objek BY objek.

* Get Characteristic Values
  SELECT objek
         atinn
         mafid
         klart
         adzhl
         atwrt
         atflv
         FROM ausp
         INTO TABLE ta_ausp
         FOR ALL ENTRIES IN ta_objek
         WHERE objek EQ ta_objek-objek AND
               atinn IN ta_atinn." AND
*               mafid = 'O' AND
*               klart = '002' AND
*               adzhl = space.

  IF sy-subrc EQ 0.
    LOOP AT ta_ausp ASSIGNING <fs_ausp>.
      IF NOT ( <fs_ausp>-mafid EQ 'O' AND <fs_ausp>-klart EQ '002' AND <fs_ausp>-adzhl EQ space ).
        <fs_ausp>-del = abap_true.
      ENDIF.
    ENDLOOP.

    SORT ta_ausp BY del.
    DELETE ta_ausp WHERE del = abap_true.

    SORT ta_ausp BY objek atinn atwrt.
  ENDIF.

* Get Intertnal number of Tank Fluid Charactertistic
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'ODOURANT_TANK'
    IMPORTING
      output = lv_atinn.

* Remove all Equipments which do not hold odourant fluid
  LOOP AT ta_equi ASSIGNING <fs_equi>.
    lv_objek = <fs_equi>-equnr.

    READ TABLE ta_inob ASSIGNING <fs_inob>
                       WITH KEY objek = lv_objek
                       BINARY SEARCH.

    IF sy-subrc EQ 0.
      lv_objek = <fs_inob>-cuobj.
    ENDIF.

    READ TABLE ta_ausp WITH KEY objek = lv_objek
                                atinn = lv_atinn
                                atwrt = 'Y'
                       BINARY SEARCH
                       TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      <fs_equi>-del = abap_true.
    ENDIF.
  ENDLOOP.

  DELETE ta_equi WHERE del = abap_true.
  IF ta_equi IS INITIAL.
    MESSAGE 'No equipments identified'(029) TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

* Filter Equipments by Tank Type
  IF s_ttype[] IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'TANK_TYPE'
      IMPORTING
        output = lv_atinn.

    LOOP AT ta_equi ASSIGNING <fs_equi>.
      lv_objek = <fs_equi>-equnr.

      READ TABLE ta_inob ASSIGNING <fs_inob>
                         WITH KEY objek = lv_objek
                         BINARY SEARCH.

      IF sy-subrc EQ 0.
        lv_objek = <fs_inob>-cuobj.
      ENDIF.

      READ TABLE ta_ausp INTO wa_ausp WITH KEY objek = lv_objek
                                               atinn = lv_atinn
                                               BINARY SEARCH.
      IF sy-subrc NE 0.
        <fs_equi>-del = abap_true.
      ELSE.
        IF wa_ausp-atwrt NOT IN s_ttype[].
          <fs_equi>-del = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DELETE ta_equi WHERE del = abap_true.
  IF ta_equi IS INITIAL.
    MESSAGE 'No equipments identified'(029) TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

* Get Location Text
  SELECT werks stand ktext FROM t499s
         INTO TABLE ta_t499s
         FOR ALL ENTRIES IN ta_equi
         WHERE werks = ta_equi-werks AND
               stand = ta_equi-stort.

  IF sy-subrc EQ 0.
    SORT ta_t499s BY werks stand.
  ENDIF.

* Get Measurement points for Equipment
  SELECT point
         mpobj
         psort
         pttxt
         mrngu
         FROM imptt
         INTO TABLE ta_imptt
         FOR ALL ENTRIES IN ta_equi
         WHERE mpobj = ta_equi-objnr.

  IF sy-subrc EQ 0.
    SORT ta_imptt BY mpobj psort.
  ENDIF.

* Get Measurement documents with Readings
  IF ta_imptt IS NOT INITIAL.
    SELECT mdocm
           point
           idate
           itime
           recdv
           recdu
           vlcod
           FROM imrg
           INTO TABLE ta_imrg
           FOR ALL ENTRIES IN ta_imptt
           WHERE point EQ ta_imptt-point AND
                 idate LE p_date.

    IF sy-subrc EQ 0.
      SORT ta_imrg BY point ASCENDING idate DESCENDING itime DESCENDING.
    ENDIF.
  ENDIF.

* Get Technicians
  SELECT objty
         objid
         ktext INTO TABLE ta_tech
               FROM crhd_v1
               FOR ALL ENTRIES IN ta_equi
               WHERE objty = ta_equi-pm_objty AND
                     objid = ta_equi-gewrk    AND
                     spras = sy-langu.

  IF sy-subrc EQ 0.
    SORT ta_tech BY objty objid.
  ENDIF.

* Get internal number of tank type Characteristic
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'HEAD_TYPE'
    IMPORTING
      output = lv_atinn.

* Get All possible values for Tank Type
  SELECT atwrt
         atwtb
         FROM wrf_apc_v_cawnt
         INTO TABLE ta_cawnt
         WHERE atinn = lv_atinn AND
               spras = sy-langu.

  IF sy-subrc EQ 0.
    SORT ta_cawnt BY atwrt.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PREPARE_OUTPUT
*&---------------------------------------------------------------------*
*       Prepare out table
*----------------------------------------------------------------------*
FORM prepare_output .

  DATA: wa_equi    TYPE ty_equi,
        wa_final   TYPE ty_final,
        wa_cawnt   TYPE ty_cawnt,
        wa_tech    TYPE ty_tech,
        wa_t499s   TYPE ty_t499s.

  LOOP AT ta_equi INTO wa_equi.

    wa_final-equipment = wa_equi-equnr.
    wa_final-equip_desc = wa_equi-eqktx.

* Get Technician
    READ TABLE ta_tech INTO wa_tech
                       WITH KEY objty = wa_equi-pm_objty
                                objid = wa_equi-gewrk
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_final-techrespn = wa_tech-ktext.
    ENDIF.

* Get Tank Type
    PERFORM get_characteristic_value USING wa_equi-equnr
                                       'TANK_TYPE'
                                     CHANGING wa_final-tank_type.

* Pass Station ID
    wa_final-stationid = wa_equi-stort.

* GEt Station Description
    READ TABLE ta_t499s INTO wa_t499s WITH KEY werks = wa_equi-werks
                                               stand = wa_equi-stort
                                               BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_final-stationname = wa_t499s-ktext.
    ENDIF.

* Pass Functional Location
    wa_final-funcloc = wa_equi-tplnr.

* Get Head Type
    PERFORM get_characteristic_value USING wa_equi-equnr
                                       'HEAD_TYPE'
                                     CHANGING wa_final-head_type.

* Get Tank values
    PERFORM get_tank_values   USING wa_equi-equnr
                                    wa_equi-objnr
                                    wa_final-head_type
                              CHANGING wa_final-capacity
                                       wa_final-capacity_uom
                                       wa_final-volume
                                       wa_final-measure_date
                                       wa_final-left
                                       wa_final-level_uom
                                       wa_final-fill
                                       wa_final-%
                                       wa_final-message.

    READ TABLE ta_cawnt INTO wa_cawnt WITH KEY atwrt = wa_final-head_type
                                      BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_final-head_type = wa_cawnt-atwtb.
    ENDIF.

    APPEND wa_final TO ta_final.
    CLEAR: wa_final,
           wa_tech.
  ENDLOOP.

ENDFORM.                    " PREPARE_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  GET_CHARACTERISTIC_VALUE
*&---------------------------------------------------------------------*
*       Get Characteristic Value
*----------------------------------------------------------------------*
*      -->P_TPLNR  Functional location
*      -->P_ATNAM  Characeristic value
*      <--P_VALUE  Characteristic Value
*----------------------------------------------------------------------*
FORM get_characteristic_value  USING    p_equnr TYPE equnr
                                        p_atnam TYPE cabn-atnam
                               CHANGING p_value TYPE any.

  FIELD-SYMBOLS: <lfs_cabn> TYPE ty_cabn,
                 <lfs_inob> TYPE ty_inob,
                 <lfs_ausp> TYPE ty_ausp.
  DATA: lv_objek  TYPE ausp-objek,
        lv_char10 TYPE char10.

  CLEAR p_value.
  READ TABLE ta_cabn ASSIGNING <lfs_cabn> WITH KEY atnam = p_atnam
                                          BINARY SEARCH.
  IF sy-subrc NE 0 OR <lfs_cabn> IS NOT ASSIGNED.
    RETURN.
  ENDIF.

  lv_objek = p_equnr.

  READ TABLE ta_inob ASSIGNING <lfs_inob>
                     WITH KEY objek = lv_objek
                     BINARY SEARCH.

  IF sy-subrc EQ 0.
    lv_objek = <lfs_inob>-cuobj.
  ENDIF.

  READ TABLE ta_ausp ASSIGNING <lfs_ausp> WITH KEY objek = lv_objek
                                                   atinn = <lfs_cabn>-atinn
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
      p_value = <lfs_ausp>-atflv.
    WHEN 'CHAR'.
      p_value = <lfs_ausp>-atwrt.
  ENDCASE.

ENDFORM.                    " GET_CHARACTERISTIC_VALUE
*&---------------------------------------------------------------------*
*&      Form  GET_LAST_MEASURE_READING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EQUI_EQUNR  text
*      -->P_0761   text
*      <--P_WA_FINAL_LEFT  text
*----------------------------------------------------------------------*
FORM get_last_measure_reading  USING    p_objnr TYPE equi-objnr
                                        p_psort TYPE imptt-psort
                                        p_vlcod TYPE char1
                               CHANGING p_value TYPE any
                                        p_idate TYPE sy-datum.

  DATA: wa_imrg TYPE ty_imrg,
        wa_imptt TYPE ty_imptt.

  READ TABLE ta_imptt INTO wa_imptt WITH KEY mpobj = p_objnr
                                             psort = p_psort
                                             BINARY SEARCH.
  IF sy-subrc EQ 0.
    READ TABLE ta_imrg INTO wa_imrg WITH KEY point = wa_imptt-point
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
*&      Form  GET_TANK_CAPACITY
*&---------------------------------------------------------------------*
*       Get Tank Values
*----------------------------------------------------------------------*
FORM get_tank_values    USING    p_equnr TYPE equnr " Equipment Number
                                 p_objnr TYPE equi-objnr " Object No.
                                 p_headtype TYPE atwrt " Head Type
                        CHANGING p_capacity TYPE any " Tank Capacity
                                 p_uom TYPE any " Tank UOM
                                 p_volume TYPE any "Liquid Volume
                                 p_measure_date TYPE sy-datum "Measurement date
                                 p_left TYPE any " Liquid Height
                                 p_level_uom TYPE atwrt
                                 p_fill TYPE any " Fill
                                 p_% TYPE any " % Fill
                                 p_message TYPE char200. "Error Messages

  DATA: lv_length    TYPE p DECIMALS 5,
        lv_diameter  TYPE p DECIMALS 5,
        lv_radius    TYPE p DECIMALS 5,
        lv_thickness TYPE p DECIMALS 5,
        lv_capacity  TYPE p DECIMALS 5,
        lv_volume    TYPE p DECIMALS 5,
        lv_part1     TYPE p DECIMALS 5,
        lv_part2     TYPE p DECIMALS 5,
        lv_part3     TYPE p DECIMALS 5,
        lv_height    TYPE p DECIMALS 5,
        lv_uom       TYPE atwrt.

  DATA: lv_date      TYPE sy-datum.

* Tank Capacity
  PERFORM get_characteristic_value USING p_equnr
                                     'TANK_CAPACITY'
                                   CHANGING lv_capacity.

*TANK Measurement UOM
  PERFORM get_characteristic_value USING p_equnr
                                     'TANK_MEASUREMENT_UOM'
                                      CHANGING lv_uom.

* Get External diameter
  PERFORM get_characteristic_value USING p_equnr
                                     'EXTERNAL_DIAMETER'
                                   CHANGING lv_diameter.

* Get External Length
  PERFORM get_characteristic_value USING p_equnr
                                     'CYLINDER_LENGTH'
                                   CHANGING lv_length.

* Get thickness
  PERFORM get_characteristic_value USING p_equnr
                                     'WALL_THICKNESS'
                                   CHANGING lv_thickness.

* Calculate internal diameter
  lv_diameter = lv_diameter - ( 2 * lv_thickness ).

  IF lv_capacity IS INITIAL.

    IF lv_diameter IS INITIAL.
      p_message = 'Diameter of Tank not maintained'(031).
      RETURN.
    ENDIF.

    IF lv_length IS INITIAL.
      p_message = 'Length of Tank not maintained'(032).
      RETURN.
    ENDIF.

    IF lv_uom IS INITIAL.
      p_message = 'Tank Measurement UOM not maintained'(033).
      RETURN.
    ENDIF.

* Calculate Tank capacity
    CASE  p_headtype.
      WHEN 'HEMISPHERE' OR '1'.

        lv_capacity = ( 22 / 7 * lv_diameter * lv_diameter * lv_length / 4 ) +
                      ( 22 / 7 * 4 / 3 * lv_diameter * lv_diameter * lv_diameter / 8 ).

      WHEN 'CAP STYLE' OR '2'.

        lv_capacity = ( 22 / 7 * lv_diameter * lv_diameter * lv_length / 4 ) +
                      ( 22 / 7 * 43 / 100 * lv_diameter * lv_diameter * lv_diameter / 8 ).
      WHEN OTHERS.
        p_message = 'Invalid Head type'(039).
        RETURN.
    ENDCASE.

* Convert Volume to US GAL
    IF lv_capacity IS NOT INITIAL.
      CASE  lv_uom.
        WHEN '"'.
          p_uom = 'GL'.
          p_capacity = lv_capacity / 231.
        WHEN 'M'.
          p_uom = 'GL'.
          p_capacity = lv_capacity * 264.
        WHEN 'MM'.
          p_uom = 'GL'.
          p_capacity = lv_capacity * 264 / 1000000000.
        WHEN OTHERS.
          p_message = 'Invalid Tank Measurement UOM'(034).
          RETURN.
      ENDCASE.
    ENDIF.
  ELSE.

* Get TANK_CAPACITY_UOM
    PERFORM get_characteristic_value USING p_equnr
                                       'TANK_CAPACITY_UOM'
                                     CHANGING p_uom.

    IF p_uom IS INITIAL.
      p_message = 'Tank Capacity UOM not maintained'(035).
      RETURN.
    ENDIF.

* Convert Volume to US GAL
    IF p_uom EQ 'LITRES' OR p_uom EQ 'L'.
      p_capacity = lv_capacity * 264172 / 1000000.
      p_uom = 'GL'.
    ELSEIF p_uom = 'US GAL' OR p_uom EQ 'GL'.
      p_capacity = lv_capacity.
    ELSE.
      p_message = 'Invalid Tank Capacity UOM'(036).
      RETURN.
    ENDIF.
  ENDIF.

* Get mesaurement doc. for liquid left
  PERFORM get_last_measure_reading USING p_objnr
                                    'TANK_LEVEL_AS_LEFT'
                                     space
                                   CHANGING p_left
                                            p_measure_date.

  IF p_left IS NOT INITIAL.
* Get Measurement doc. for liquid height percentage
    PERFORM get_last_measure_reading USING p_objnr
                                       'TANK_LEVEL_UOM'
                                       abap_true
                                       CHANGING p_level_uom
                                       lv_date.

    IF p_level_uom IS NOT INITIAL.
      IF lv_diameter IS INITIAL.
        p_message = 'Diameter of Tank not maintained'(031).
        RETURN.
      ENDIF.

      IF lv_length IS INITIAL.
        p_message = 'Length of Tank not maintained'(032).
        RETURN.
      ENDIF.

* Convert Measurement UOM to tank measurement UOM in Characteristic
      CASE  p_level_uom.
        WHEN 'IN' OR '"' OR 'INCH'.
          IF lv_uom EQ '"' OR lv_uom EQ 'IN' OR lv_uom EQ 'INCH'.
            lv_height = p_left.
          ELSEIF lv_uom EQ 'M'.
            lv_height = p_left * 254 / 10000.
          ELSE.
            p_message = 'Invalid Tank Measurement UOM'(034).
            RETURN.
          ENDIF.
        WHEN 'M'.
          IF lv_uom EQ '"' OR lv_uom EQ 'IN' OR lv_uom EQ 'INCH'.
            lv_height = p_left * 393701 / 10000.
          ELSEIF lv_uom EQ 'M'.
            lv_height = p_left.
          ELSE.
            p_message = 'Invalid Tank Measurement UOM'(034).
            RETURN.
          ENDIF.
          p_left = p_left * 393701 / 10000.
        WHEN 'MM'.
          IF lv_uom EQ '"' OR lv_uom EQ 'IN' OR lv_uom EQ 'INCH'.
            lv_height = p_left * 393701 / 10000 / 1000.
          ELSEIF lv_uom EQ 'M'.
            lv_height = p_left / 1000.
          ELSE.
            p_message = 'Invalid Tank Measurement UOM'(034).
            RETURN.
          ENDIF.
          p_left = p_left * 393701 / 10000 / 1000.
        WHEN OTHERS.
          p_message = 'Invalid Reading for Tank Level UOM'(037).
          RETURN.
      ENDCASE.
      p_level_uom = 'INCH'.

      IF lv_height GT lv_diameter.
        p_message = 'Liquid Level cannot be greater than diameter of tank'(038).
        RETURN.
      ENDIF.

* Calculate Liquid Volume
      CASE  p_headtype.
        WHEN 'HEMISPHERE' OR '1'.
          lv_radius = lv_diameter / 2.

          lv_part1 = ( lv_radius - lv_height ) / lv_radius.
          lv_part1 = acos( lv_part1 ).
          lv_part1 = lv_part1 * lv_radius * lv_radius.

          lv_part2 = ( 3 * lv_radius ) - lv_height.
          lv_part2 = lv_part2 * 1 / 3 * 22 / 7 * lv_height * lv_height.

          lv_part3 = ( 2 * lv_radius * lv_height ) - ( lv_height * lv_height ).
          lv_part3 = sqrt(  lv_part3 ).
          lv_part3 = lv_part3 * ( lv_radius - lv_height ).

          lv_volume = lv_part2 + ( ( lv_part1 - lv_part3 ) * lv_length ).

        WHEN 'CAP STYLE' OR '2'.
          lv_radius = lv_diameter / 2.

          lv_part1 = ( lv_radius - lv_height ) / lv_radius.
          lv_part1 = acos( lv_part1 ).
          lv_part1 = lv_part1 * lv_radius * lv_radius.

          lv_part2 = ( 3 * lv_radius ) - lv_height.
          lv_part2 = lv_part2 * 215 / 1000 * lv_height * lv_height.

          lv_part3 = ( 2 * lv_radius * lv_height ) - ( lv_height * lv_height ).
          lv_part3 = sqrt(  lv_part3 ).
          lv_part3 = lv_part3 * ( lv_radius - lv_height ).

          lv_volume = lv_part2 + ( ( lv_part1 - lv_part3 ) * lv_length ).
        WHEN OTHERS.
          p_message = 'Invalid Head type'(039).
          RETURN.
      ENDCASE.

* Convert Volume to US GAL
      CASE  lv_uom.
        WHEN '"' OR 'INCH' OR 'IN'.
          p_volume = lv_volume / 231.
        WHEN 'M'.
          p_volume = lv_volume * 264.
        WHEN 'MM'.
          p_volume = lv_volume * 264 / 1000000000.
        WHEN OTHERS.
          p_message = 'Invalid Tank Measurement UOM'(034).
          RETURN.
      ENDCASE.

      p_fill = ( p_capacity * 8 / 10 ) - p_volume.
      p_% = p_volume / p_capacity * 100.

    ELSE.
      p_message = 'No measurement Reading for Liquid Level Unit Of Measure'(040).
      RETURN.
    ENDIF.
  ELSE.
    p_fill = p_capacity * 8 / 10.
    p_message = 'No measurement Reading for Liquid Level'(041).
    RETURN.
  ENDIF.

ENDFORM.                    " GET_TANK_CAPACITY
*&---------------------------------------------------------------------*
*&      Form  F4_TANK_TYPE
*&---------------------------------------------------------------------*
*       F4 help for Tank type
*----------------------------------------------------------------------*
FORM f4_tank_type .

  DATA: lv_atinn TYPE atinn.

* Get internal number of tank type Characteristic
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'TANK_TYPE'
    IMPORTING
      output = lv_atinn.

* Get All possible values for Tank Type
  SELECT atwrt
         atwtb
         FROM wrf_apc_v_cawnt
         INTO TABLE ta_cawnt
         WHERE atinn = lv_atinn AND
               spras = sy-langu.

  IF sy-subrc EQ 0.
    SORT ta_cawnt BY atwrt.
  ENDIF.

* Pass the list of values to F4 function
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ATWRT'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'P_TTYPE'
      value_org       = 'S'
    TABLES
      value_tab       = ta_cawnt
    EXCEPTIONS
      parameter_error = 0
      no_values_found = 0
      OTHERS          = 0.

ENDFORM.                    " F4_TANK_TYPE
