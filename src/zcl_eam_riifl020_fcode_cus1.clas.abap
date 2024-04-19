class ZCL_EAM_RIIFL020_FCODE_CUS1 definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_EAM_RIIFL020_FCODE_CUS1
*"* do not include other source files here!!!

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_BADI_EAM_LIST_FCODE .
protected section.
*"* protected components of class ZCL_EAM_RIIFL020_FCODE_CUS1
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_EAM_RIIFL020_FCODE_CUS1
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_EAM_RIIFL020_FCODE_CUS1 IMPLEMENTATION.


METHOD if_ex_badi_eam_list_fcode~execute_function_code.

  DATA : ta_selparams TYPE STANDARD TABLE OF rsparams,
        wa_selparams TYPE rsparams.
  FIELD-SYMBOLS : <fs_objects> TYPE any,
                 <fs_comp> TYPE any.
  DATA : ta_loc TYPE STANDARD TABLE OF stort,
        wa_loc TYPE stort.

  LOOP AT it_selected_objects ASSIGNING <fs_objects> .

    ASSIGN COMPONENT 'SELECTED' OF STRUCTURE <fs_objects> TO <fs_comp>.
    IF <fs_comp> = 'X'.
      UNASSIGN <fs_comp>.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT 'STORT' OF STRUCTURE <fs_objects> TO <fs_comp>.
        IF <fs_comp> IS NOT INITIAL.
          wa_loc = <fs_comp>.
          APPEND wa_loc TO ta_loc.
          CLEAR : wa_loc.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT ta_loc.
  DELETE ADJACENT DUPLICATES FROM ta_loc.

  LOOP AT ta_loc INTO wa_loc.
    CLEAR wa_selparams.
    wa_selparams-kind    = 'P'.
    wa_selparams-selname = 'P_STN'.
    wa_selparams-sign    = 'I'.
    wa_selparams-option  = 'EQ'.
    wa_selparams-low     = wa_loc.
    APPEND wa_selparams TO ta_selparams.
    SUBMIT z_pm_ind_station_capacity WITH SELECTION-TABLE ta_selparams AND RETURN.
  ENDLOOP.

ENDMETHOD.
ENDCLASS.
