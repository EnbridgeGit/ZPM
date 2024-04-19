class ZPM_CL_FUNCLOC_UTIL definition
  public
  final
  create public .

public section.
*"* public components of class ZPM_CL_FUNCLOC_UTIL
*"* do not include other source files here!!!

  class-methods GET_HIERARCHY_FROM_H5
    importing
      !IV_TPLNR type TPLNR
      !IV_TPLKZ type TPLKZ
      !IV_HIERARCHY type CHAR1
    exporting
      !EV_TPLNR_H3 type TPLNR .
  class-methods HANDLE_EQUI_LOC_CHANGE
    importing
      !IV_EQUNR type EQUNR
      !IV_TPLNR type TPLNR
      !IV_TPLKZ type TPLKZ .
  class-methods UPDATE_FLOC_CHAR_VALUE
    importing
      !IV_TPLNR type TPLNR
      !IV_CLASS type KLASSE_D
      !IV_ATNAM type ATNAM
      !IV_ATWRT type ATWRT .
protected section.
*"* protected components of class ZPM_CL_FUNCLOC_UTIL
*"* do not include other source files here!!!
private section.
*"* private components of class ZPM_CL_FUNCLOC_UTIL
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZPM_CL_FUNCLOC_UTIL IMPLEMENTATION.


METHOD GET_HIERARCHY_FROM_H5.
  DATA: lv_t370s TYPE t370s,
        lv_len TYPE i,
        lv_hierarchy TYPE n,
        lv_str1 TYPE string,
        lv_str2 TYPE string.

*-- Prerequisies
*-- iv_tplnr must be H5 functional location
*-- iv_hierarchy can only contain 1, 2, 3, 4 or 5.

*-- Validate hierarchy
  CASE iv_hierarchy.
    WHEN 1.
*-- Do nothing
    WHEN 2.
*-- Do nothing
    WHEN 3.
*-- Do nothing
    WHEN 4.
*-- Do nothing
    WHEN 5.
*-- Do nothing
    WHEN OTHERS.
*-- can not contain anything else.
      RETURN.
  ENDCASE.

*-- Need to use the next hierarchy to split
  lv_hierarchy = iv_hierarchy + 1.

*-- Select structure indicator data
  SELECT SINGLE * FROM t370s INTO lv_t370s
    WHERE tplkz = iv_tplkz.
  IF sy-subrc EQ 0.
    SPLIT lv_t370s-stufm AT lv_hierarchy INTO lv_str1 lv_str2.
    lv_len = strlen( lv_str1 ).
    ev_tplnr_h3 = iv_tplnr(lv_len).
  ENDIF.

ENDMETHOD.


METHOD handle_equi_loc_change.
  CONSTANTS: lc_dsgn_cap_stat TYPE atnam VALUE 'DSGN_CAPACITY_STATUS',
             lc_stn_dsgn_cap_stat TYPE atnam VALUE 'DSGN_CAP_STATUS'.

  TYPES: BEGIN OF ty_char,
          atinn TYPE atinn,
          adzhl TYPE adzhl,
          atnam TYPE atnam,
         END OF ty_char,
         tt_char TYPE STANDARD TABLE OF ty_char,
         BEGIN OF ty_objclass,
           objek TYPE objnum,
           clint TYPE clint,
           adzhl TYPE adzhl,
         END OF ty_objclass,
         tt_objclass TYPE STANDARD TABLE OF ty_objclass,
         BEGIN OF ty_class,
           clint TYPE clint,
           klart TYPE klassenart,
           class TYPE klasse_d,
         END OF ty_class,
         tt_class TYPE STANDARD TABLE OF ty_class,
         BEGIN OF ty_classchar,
           clint TYPE clint,
           posnr TYPE kposnr,
           adzhl TYPE adzhl,
           atinn TYPE atinn,
           klart TYPE klassenart,
         END OF ty_classchar,
         tt_classchar TYPE STANDARD TABLE OF ty_classchar.

  DATA: lt_char TYPE tt_char,
        lv_tplnr_stn TYPE tplnr,
        lt_objclass TYPE tt_objclass,
        lv_objnr TYPE objnum,
        lv_objnr_stn TYPE objnum,
        lt_class TYPE tt_class,
        lt_classchar TYPE tt_classchar,
        lv_atwrt TYPE atwrt,
        iv_cuobj(18) TYPE c,           "H5 FLoc               "SDP85964
        iv_cuobj_stn(18) TYPE c,       "H3 FLOC               "SDP85964
        lv_cuobj TYPE tplnr.                                  "SDP85964

  FIELD-SYMBOLS: <fs_char> TYPE ty_char,
                 <fs_classchar> TYPE ty_classchar,
                 <fs_class> TYPE ty_class.


*-- Get internal number for the characteristics being used.
  SELECT atinn adzhl atnam FROM cabn INTO TABLE lt_char
    WHERE atnam = lc_dsgn_cap_stat OR
          atnam = lc_stn_dsgn_cap_stat.
  IF sy-subrc EQ 0.
    SORT lt_char BY atinn adzhl DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_char COMPARING atinn adzhl.
  ENDIF.

*-- If characteristics not available in system, return
  IF lt_char IS INITIAL.
    RETURN.
  ENDIF.

*-- Get the H3 functional location for the provided h5
  CALL METHOD zpm_cl_funcloc_util=>get_hierarchy_from_h5
    EXPORTING
      iv_tplnr     = iv_tplnr
      iv_tplkz     = iv_tplkz
      iv_hierarchy = '3'  " Hierarchy 3
    IMPORTING
      ev_tplnr_h3  = lv_tplnr_stn.

*-- Get assigned classes for the functional location in consideration

* SDP89564 - GYmana - Modified code to fix equip status bug   "SDP85964
*  lv_objnr = iv_tplnr.                                       "SDP85964
*  lv_objnr_stn = lv_tplnr_stn.                               "SDP85964
  SELECT cuobj FROM inob INTO iv_cuobj                        "SDP85964
   WHERE klart = '003' AND                                    "SDP85964
         obtab = 'IFLOT' AND                                  "SDP85964
         objek = iv_tplnr.                                    "SDP85964
  ENDSELECT.                                                  "SDP85964
                                                              "SDP85964
  IF sy-subrc NE 0.                                           "SDP85964
     RETURN.                                                  "SDP85964
  ENDIF.                                                      "SDP85964
                                                              "SDP85964
  SELECT cuobj FROM inob INTO iv_cuobj_stn                    "SDP85964
   WHERE klart = '003' AND                                    "SDP85964
         obtab = 'IFLOT' AND                                  "SDP85964
         objek = lv_tplnr_stn.                                "SDP85964
  ENDSELECT.                                                  "SDP85964
                                                              "SDP85964
  IF sy-subrc NE 0.                                           "SDP85964
     RETURN.                                                  "SDP85964
  ENDIF.                                                      "SDP85964
                                                              "SDP85964
  SELECT objek clint adzhl FROM kssk INTO TABLE lt_objclass   "SDP85964
*    WHERE ( objek = lv_objnr OR objek = lv_objnr_stn ) AND   "SDP85964
    WHERE ( objek = iv_cuobj OR objek = iv_cuobj_stn ) AND    "SDP85964
          mafid = 'O' AND                                     "SDP85964
          klart = '003'.                                      "SDP85964
                                                              "SDP85964
  IF  sy-subrc EQ 0.
    SORT lt_objclass BY objek clint adzhl DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_objclass COMPARING objek clint.
  ENDIF.

*-- If there are no assigned classes to the functional locations, return
  IF lt_objclass IS INITIAL.
    RETURN.
  ENDIF.

*-- Get internal number for class.
  SELECT clint klart class FROM klah INTO TABLE lt_class
    FOR ALL ENTRIES IN lt_objclass
    WHERE clint = lt_objclass-clint.
*-- If you class details are unavailble, return.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

*-- For binary search
  SORT lt_class BY clint ASCENDING.

*-- Get characteristics of class.
  SELECT clint posnr adzhl imerk klart FROM ksml INTO TABLE lt_classchar
    FOR ALL ENTRIES IN lt_objclass
    WHERE clint = lt_objclass-clint.
  IF sy-subrc EQ 0.
    SORT lt_classchar BY clint posnr adzhl atinn klart DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_classchar COMPARING clint atinn klart.
*-- For binary search
    SORT lt_classchar BY atinn ASCENDING.
  ENDIF.

  READ TABLE lt_char ASSIGNING <fs_char> WITH KEY atnam = lc_dsgn_cap_stat .
  IF sy-subrc EQ 0.
    READ TABLE lt_classchar ASSIGNING <fs_classchar> WITH KEY atinn = <fs_char>-atinn BINARY SEARCH.
    IF sy-subrc EQ 0.
*-- Update functional location char.
      READ TABLE lt_class ASSIGNING <fs_class> WITH KEY clint = <fs_classchar>-clint BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_atwrt = 'Y'.
        CALL METHOD zpm_cl_funcloc_util=>update_floc_char_value
          EXPORTING
            iv_tplnr = iv_tplnr
            iv_class = <fs_class>-class
            iv_atnam = <fs_char>-atnam
            iv_atwrt = lv_atwrt.
      ENDIF.
    ENDIF.
  ENDIF.

  READ TABLE lt_char ASSIGNING <fs_char> WITH KEY atnam = lc_stn_dsgn_cap_stat.
  IF sy-subrc EQ 0.
    READ TABLE lt_classchar ASSIGNING <fs_classchar> WITH KEY atinn = <fs_char>-atinn BINARY SEARCH.
    IF sy-subrc EQ 0.
*-- Update functional location char.
      READ TABLE lt_class ASSIGNING <fs_class> WITH KEY clint = <fs_classchar>-clint BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_atwrt = 'Y'.
        CALL METHOD zpm_cl_funcloc_util=>update_floc_char_value
          EXPORTING
            iv_tplnr = lv_tplnr_stn
            iv_class = <fs_class>-class
            iv_atnam = <fs_char>-atnam
            iv_atwrt = lv_atwrt.
      ENDIF.
    ENDIF.
  ENDIF.


ENDMETHOD.


METHOD update_floc_char_value.
  DATA: lt_numval TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
        lt_charval TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
        lw_charval TYPE bapi1003_alloc_values_char,
        lt_currval TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
        lt_result TYPE STANDARD TABLE OF bapiret2,
        lv_objnr TYPE bapi1003_key-object.

  FIELD-SYMBOLS: <fs_result> TYPE bapiret2,
                 <fs_charval> TYPE bapi1003_alloc_values_char.

*-- Set the Object key
  lv_objnr = iv_tplnr.

*-- Get the existing object characteristic values
  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      objectkey       = lv_objnr
      objecttable     = 'IFLOT'
      classnum        = iv_class
      classtype       = '003'
    TABLES
      allocvaluesnum  = lt_numval
      allocvalueschar = lt_charval
      allocvaluescurr = lt_currval
      return          = lt_result.

  READ TABLE lt_charval ASSIGNING <fs_charval> WITH KEY charact = iv_atnam.
  IF sy-subrc EQ 0.
*-- The char has value check and assign the new value if required.
    IF  <fs_charval>-value_char = iv_atwrt.
*-- There is no change in the value of the characteristic. Return
      RETURN.
    ELSE.
*-- Change the value
      <fs_charval>-value_char = iv_atwrt.
      <fs_charval>-value_neutral = iv_atwrt.
    ENDIF.
  ELSE.
*-- The char does not have value. Assign the value.
    lw_charval-charact = iv_atnam.
    lw_charval-value_char = iv_atwrt.
    lw_charval-value_neutral = iv_atwrt.

    APPEND  lw_charval TO lt_charval.
  ENDIF.

  CALL FUNCTION 'BAPI_OBJCL_CHANGE'
    EXPORTING
      objectkey          = lv_objnr
      objecttable        = 'IFLOT'
      classnum           = iv_class
      classtype          = '003'
      status             = '1'
    TABLES
      allocvaluesnumnew  = lt_numval
      allocvaluescharnew = lt_charval
      allocvaluescurrnew = lt_currval
      return             = lt_result.
  READ TABLE lt_result ASSIGNING <fs_result> WITH KEY type = 'E'.
  IF sy-subrc IS NOT INITIAL.
*-- Handle error message
  ENDIF.

ENDMETHOD.
ENDCLASS.
