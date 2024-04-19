class ZCL_IM_PM_EQUI_UPDATE definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_PM_EQUI_UPDATE
*"* do not include other source files here!!!

  interfaces IF_EX_EQUI_UPDATE .
protected section.
*"* protected components of class ZCL_IM_PM_EQUI_UPDATE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_PM_EQUI_UPDATE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_PM_EQUI_UPDATE IMPLEMENTATION.


METHOD if_ex_equi_update~in_update.

  DATA: lv_tplkz TYPE tplkz, "Structure indicator
        lv_iloan TYPE iloan, "Location assignment
        lv_tplnr TYPE tplnr, "Functional Location
        lv_cuobj TYPE cuobj, "Internal number
        lv_count(3) TYPE N,
        lv_enh_active TYPE tvarv_val, "Enhancemnet active check variable
        lv_clint_c1 TYPE clint value 2591,                "ACR247
        lv_clint_c2 TYPE clint value 2590.                "ACR247

  SELECT SINGLE low FROM tvarvc INTO lv_enh_active
    WHERE name = 'ZPM_DSGN_CAP_ENH_ACTIVE'.
  IF sy-subrc EQ 0 AND lv_enh_active = 'N'.
*-- Enhancement is disabled. Return.
    RETURN.
  ENDIF.

*-- If functional locaiton is not being assigned, return
  IF i_data_equi-itob-tplnr IS INITIAL.
    RETURN.
  ENDIF.

*-- ACR247 GYMANA
*-- If functional location is blank, check the equipment time segment table
*-- for the last installed location for the current date for the equipment.

  IF i_data_equi-itob-tplkz IS INITIAL.
    CLEAR: lv_tplnr, lv_iloan.
    SELECT MAX( iloan ) FROM equz into lv_iloan
      WHERE equnr = i_data_equi-itob-equnr
        AND datbi = sy-datum.

      IF sy-subrc EQ 0.
         SELECT tplnr FROM iloa into lv_tplnr
          WHERE iloan = lv_iloan.

         IF sy-subrc EQ 0 AND lv_tplnr IS INITIAL.
            RETURN.
         ENDIF.
         ENDSELECT.
      ENDIF.

  ELSE.
    lv_tplkz = i_data_equi-itob-tplkz.
  ENDIF.



*-- Get the structure indicator, if it is initial.
  IF i_data_equi-itob-tplkz IS INITIAL.
    SELECT SINGLE tplkz FROM iflot INTO lv_tplkz
      WHERE tplnr = i_data_equi-itob-tplnr.
  ELSE.
    lv_tplkz = i_data_equi-itob-tplkz.
  ENDIF.

*-- ACR247 GYMANA
*-- Validate that the equipment is engineering relevant
  SELECT cuobj FROM inob into lv_cuobj
   WHERE objek = i_data_equi-itob-tplnr
     AND klart = '003'.

   IF sy-subrc EQ 0.
      SELECT COUNT( * ) FROM kssk into lv_count
       WHERE objek = lv_cuobj
         AND klart = '003'
         AND ( clint = lv_clint_c1 OR
               clint = lv_clint_c2 ).

   ENDIF.
   ENDSELECT.

*-- Call method to handle the required functionality

  IF lv_count > 0.
     zpm_cl_funcloc_util=>handle_equi_loc_change(
       iv_equnr = i_data_equi-itob-equnr
       iv_tplnr = i_data_equi-itob-tplnr
       iv_tplkz = lv_tplkz ).
  ENDIF.

ENDMETHOD.
ENDCLASS.
