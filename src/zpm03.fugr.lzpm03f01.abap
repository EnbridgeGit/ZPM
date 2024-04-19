*----------------------------------------------------------------------*
***INCLUDE LZPM03F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_CHARACTERISTIC_VALUE
*&---------------------------------------------------------------------*
*       Get Characteristic Value
*----------------------------------------------------------------------*
FORM get_characteristic_value  USING    p_objek TYPE iflo-tplnr
                                        p_atnam TYPE cabn-atnam
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
