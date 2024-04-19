FUNCTION zpm_get_station_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IMP_DATE) TYPE  SY-DATUM DEFAULT SY-DATUM
*"  TABLES
*"      TBL_STATION_DETAILS STRUCTURE  ZPMS_STATION_DETAIL OPTIONAL
*"----------------------------------------------------------------------

  DATA: lwa_objek          TYPE gty_objek,
        lwa_station_detail TYPE zpms_station_detail,
        lwa_cawnt          TYPE gty_cawnt.

  DATA: gt_iwert TYPE RANGE OF iflo-iwerk.

  CONSTANTS: gc_edw_planning_plant TYPE tvarvc-name VALUE 'ZPM_EDW_PLANNING_PLANT'.

  FIELD-SYMBOLS: <lfs_station>  TYPE gty_station,
                 <lfs_crhd>     TYPE gty_crhd,
                 <fs_inob>      TYPE gty_inob,
                 <lfs_district> TYPE gty_district,
                 <lfs_branch>   TYPE gty_branch.

  SELECT sign opti low high FROM tvarvc INTO TABLE gt_iwert
                            WHERE name = gc_edw_planning_plant.
  SELECT tplnr
         pltxt
         tplma
         erdat
         aedat
         pm_objty
         lgwid
         objnr
         stort
         msgrp
         FROM iflo
         INTO TABLE git_station
         WHERE spras EQ sy-langu AND
               ( erdat GE imp_date OR aedat GE imp_date ) AND
               stort NE space    AND
               eqart EQ 'ST_RG'  AND
               iwerk IN gt_iwert.

  IF git_station IS INITIAL.
    RETURN.
  ENDIF.

  SELECT objnr
         FROM jest
         INTO TABLE git_jest
         FOR ALL ENTRIES IN git_station
         WHERE objnr = git_station-objnr AND
               stat  = 'I0076' AND
               inact = space.

  SELECT objty
         objid
         arbpl
         ktext
         FROM crhd_v1
         INTO TABLE git_crhd
         FOR ALL ENTRIES IN git_station
         WHERE objty = git_station-pm_objty AND
               objid = git_station-lgwid    AND
               spras = sy-langu.

  SELECT tplnr
         pltxt
         tplma
         FROM iflo
         INTO TABLE git_branch
         FOR ALL ENTRIES IN git_station
         WHERE spras = sy-langu       AND
               tplnr = git_station-tplma.

  IF git_branch IS NOT INITIAL.
    SELECT tplnr
           pltxt
           FROM iflo
           INTO TABLE git_district
           FOR ALL ENTRIES IN git_branch
           WHERE spras = sy-langu       AND
                 tplnr = git_branch-tplma.
  ENDIF.

  SELECT atinn
         atnam
         atfor
         FROM cabn
         INTO TABLE git_cabn
         WHERE atnam IN ('STN_CLASS_CODE',   " Class description
                         'STN_PURPOSE_CODE', " Purpose Description
                         'DESIGN_CODE1',  " Design Code 1
                         'DESIGN_CODE2', " Design Code 2
                         'DESIGN_CODE3', " Design Code 3
                         'DESIGN_CODE4', " Design Code 4
                         'STN_STAGE_1',        " Stage 1
                         'STN_STAGE_2',        " Stage 2
                         'STN_STAGE_3',        " Stage 3
                         'STN_STAGE_4',        " Stage 4
                         'STN_STAGE_5',        " Stage 5
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
                         'DSGN_CAP_STATUS', " Design Capacity Stat
                         'DSGNCAP_REVSD_DT'). " Design Capacity REVDT

* Get internal number of Purpose Code
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'STN_PURPOSE_CODE'
    IMPORTING
      output = gv_purps_code.

* Get internal number of Class Code
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'STN_CLASS_CODE'
    IMPORTING
      output = gv_class_code.

  IF gv_class_code IS NOT INITIAL OR
     gv_purps_code IS NOT INITIAL.

* Get characteristic value and descrption for Purpose Code
    SELECT atwrt
           atwtb
           FROM wrf_apc_v_cawnt
           INTO TABLE git_cawnt
           WHERE ( atinn = gv_purps_code OR
                   atinn = gv_class_code ) AND
                   spras = sy-langu.

    IF sy-subrc EQ 0.
      SORT git_cawnt BY atwrt.
    ENDIF.
  ENDIF.

  LOOP AT git_station ASSIGNING <lfs_station>.
    lwa_objek-objek = <lfs_station>-tplnr.
    INSERT lwa_objek INTO TABLE git_objek.
  ENDLOOP.

  SELECT cuobj
         objek
         FROM inob
         INTO TABLE git_inob
         FOR ALL ENTRIES IN git_objek
         WHERE obtab = 'IFLOT' AND
               objek = git_objek-objek AND
               klart = '003'.

  IF sy-subrc EQ 0.
    SORT git_inob BY objek.
    LOOP AT git_inob ASSIGNING <fs_inob>.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_inob>-cuobj
        IMPORTING
          output = <fs_inob>-cuobj.

      lwa_objek-objek = <fs_inob>-cuobj.
      INSERT lwa_objek INTO TABLE git_objek.            "SDP85637
    ENDLOOP.
  ENDIF.

  SELECT objek
         atinn
         atwrt
         atflv
         FROM ausp
         INTO TABLE git_ausp
         FOR ALL ENTRIES IN git_objek
         WHERE objek = git_objek-objek AND
               mafid = 'O' AND
               klart = '003' AND
               adzhl = space.

  LOOP AT git_station ASSIGNING <lfs_station>.
    lwa_station_detail-station_id = <lfs_station>-stort.
    lwa_station_detail-station_name = <lfs_station>-pltxt.
    lwa_station_detail-premises_code = <lfs_station>-msgrp.

    READ TABLE git_branch ASSIGNING <lfs_branch>
                          WITH TABLE KEY tplnr = <lfs_station>-tplma.
    IF <lfs_branch> IS ASSIGNED.
      lwa_station_detail-branch_id = <lfs_branch>-tplnr.
      lwa_station_detail-branch_name = <lfs_branch>-pltxt.

      READ TABLE git_district ASSIGNING <lfs_district>
                              WITH TABLE KEY tplnr = <lfs_branch>-tplma.

      IF <lfs_district> IS ASSIGNED.
        lwa_station_detail-district_id = <lfs_district>-tplnr.
        lwa_station_detail-district_name = <lfs_district>-pltxt.
      ENDIF.
    ENDIF.

    READ TABLE git_jest WITH TABLE KEY objnr = <lfs_station>-objnr
                        TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      lwa_station_detail-delete_flag = abap_true.
    ENDIF.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'STN_CLASS_CODE'
                                           0
                                     CHANGING lwa_station_detail-class_code.

    CLEAR lwa_cawnt.
    READ TABLE git_cawnt INTO lwa_cawnt WITH KEY atwrt = lwa_station_detail-class_code
                                        BINARY SEARCH
                                        TRANSPORTING atwtb.
    IF sy-subrc EQ 0.
      lwa_station_detail-class_desc = lwa_cawnt-atwtb.
    ENDIF.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'STN_PURPOSE_CODE'
                                           0
                                     CHANGING lwa_station_detail-purpose_code.

    CLEAR lwa_cawnt.
    READ TABLE git_cawnt INTO lwa_cawnt WITH KEY atwrt = lwa_station_detail-purpose_code
                                        BINARY SEARCH
                                        TRANSPORTING atwtb.
    IF sy-subrc EQ 0.
      lwa_station_detail-purpose_desc = lwa_cawnt-atwtb.
    ENDIF.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'DESIGN_CODE1'
                                           0
                                     CHANGING lwa_station_detail-design_code1.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'DESIGN_CODE2'
                                           0
                                     CHANGING lwa_station_detail-design_code2.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'DESIGN_CODE3'
                                           0
                                     CHANGING lwa_station_detail-design_code3.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'DESIGN_CODE4'
                                           0
                                     CHANGING lwa_station_detail-design_code3.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'STN_STAGE_1'
                                           0
                                     CHANGING lwa_station_detail-stage_1.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'INLET_MOP_1'
                                           2
                                     CHANGING lwa_station_detail-inlet_maop_1.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'INLET_MIN_1'
                                           2
                                     CHANGING lwa_station_detail-inlet_min_1.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'OUTLET_MOP_1'
                                           2
                                     CHANGING lwa_station_detail-outlet_maop_1.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'OUTLET_MIN_1'
                                           2
                                     CHANGING lwa_station_detail-outlet_min_1.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'MAX_PRESS_1'
                                           2
                                     CHANGING lwa_station_detail-max_pressure_1.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'CAPACITY_QUANTITY_1'
                                           0
                                     CHANGING lwa_station_detail-capacity_qty_1.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'STN_STAGE_2'
                                           0
                                     CHANGING lwa_station_detail-stage_2.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'INLET_MOP_2'
                                           2
                                     CHANGING lwa_station_detail-inlet_maop_2.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'INLET_MIN_2'
                                           2
                                     CHANGING lwa_station_detail-inlet_min_2.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'OUTLET_MOP_2'
                                           2
                                     CHANGING lwa_station_detail-outlet_maop_2.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'OUTLET_MIN_2'
                                           2
                                     CHANGING lwa_station_detail-outlet_min_2.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'MAX_PRESS_2'
                                           2
                                     CHANGING lwa_station_detail-max_pressure_2.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'CAPACITY_QUANTITY_2'
                                           0
                                     CHANGING lwa_station_detail-capacity_qty_2.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'STN_STAGE_3'
                                           0
                                     CHANGING lwa_station_detail-stage_3.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'INLET_MOP_3'
                                           2
                                     CHANGING lwa_station_detail-inlet_maop_3.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'INLET_MIN_3'
                                           2
                                     CHANGING lwa_station_detail-inlet_min_3.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'OUTLET_MOP_3'
                                           2
                                     CHANGING lwa_station_detail-outlet_maop_3.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'OUTLET_MIN_3'
                                           2
                                     CHANGING lwa_station_detail-outlet_min_3.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'MAX_PRESS_3'
                                           2
                                     CHANGING lwa_station_detail-max_pressure_3.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'CAPACITY_QUANTITY_3'
                                           0
                                     CHANGING lwa_station_detail-capacity_qty_3.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'STN_STAGE_4'
                                           0
                                     CHANGING lwa_station_detail-stage_4.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'INLET_MOP_4'
                                           2
                                     CHANGING lwa_station_detail-inlet_maop_4.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'INLET_MIN_4'
                                           2
                                     CHANGING lwa_station_detail-inlet_min_4.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'OUTLET_MOP_4'
                                           2
                                     CHANGING lwa_station_detail-outlet_maop_4.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'OUTLET_MIN_4'
                                           2
                                     CHANGING lwa_station_detail-outlet_min_4.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'MAX_PRESS_4'
                                           2
                                     CHANGING lwa_station_detail-max_pressure_4.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'CAPACITY_QUANTITY_4'
                                           0
                                     CHANGING lwa_station_detail-capacity_qty_4.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'STN_STAGE_5'
                                           0
                                     CHANGING lwa_station_detail-stage_5.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'INLET_MOP_5'
                                           2
                                     CHANGING lwa_station_detail-inlet_maop_5.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'INLET_MIN_5'
                                           2
                                     CHANGING lwa_station_detail-inlet_min_5.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'OUTLET_MOP_5'
                                           2
                                     CHANGING lwa_station_detail-outlet_maop_5.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'OUTLET_MIN_5'
                                           2
                                     CHANGING lwa_station_detail-outlet_min_5.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'MAX_PRESS_5'
                                           2
                                     CHANGING lwa_station_detail-max_pressure_5.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'CAPACITY_QUANTITY_5'
                                           0
                                     CHANGING lwa_station_detail-capacity_qty_5.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'DSGN_CAP_STATUS'
                                           0
                                     CHANGING lwa_station_detail-design_cap_stat.

    PERFORM get_characteristic_value USING <lfs_station>-tplnr
                                           'DSGNCAP_REVSD_DT'
                                           0
                                     CHANGING lwa_station_detail-design_cap_revdt.

    READ TABLE git_crhd ASSIGNING <lfs_crhd> WITH TABLE KEY objty = <lfs_station>-pm_objty
                                                            objid = <lfs_station>-lgwid.
    IF sy-subrc EQ 0 AND <lfs_crhd> IS ASSIGNED.
      lwa_station_detail-workcenter_id = <lfs_crhd>-arbpl.
      lwa_station_detail-workcenter_desc = <lfs_crhd>-ktext.
    ENDIF.

    lwa_station_detail-date_upd = <lfs_station>-aedat.
    lwa_station_detail-date_crt = <lfs_station>-erdat.
    APPEND lwa_station_detail TO tbl_station_details.
    CLEAR lwa_station_detail.

  ENDLOOP.

ENDFUNCTION.
