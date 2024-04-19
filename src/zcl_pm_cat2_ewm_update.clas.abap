class ZCL_PM_CAT2_EWM_UPDATE definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_PM_CAT2_EWM_UPDATE
*"* do not include other source files here!!!

  class-methods SET_RECEIVING_ORDER
    importing
      !IM_VARIANT type CATSVARIAN
    changing
      !CH_CATSDB type CATSDB_EXT .
protected section.
*"* protected components of class ZCL_PM_CAT2_EWM_UPDATE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_PM_CAT2_EWM_UPDATE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_PM_CAT2_EWM_UPDATE IMPLEMENTATION.


METHOD set_receiving_order.

  CONSTANTS: lc_tclas TYPE pspar-tclas VALUE 'A',
             lc_infty TYPE prelp-infty VALUE '0027',
             lc_subty TYPE prelp-subty VALUE '12'.

  DATA: lv_awart TYPE setleaf-valfrom,
        lv_kostl TYPE pa0001-kostl,

        lt_p0027 TYPE TABLE OF p0027,
        ls_p0027 TYPE p0027,
        lt_p0001 TYPE TABLE OF p0001,
        ls_p0001 TYPE p0001,
        ls_hio   TYPE zhrcat_alt_hio.

  IF im_variant NE 'ZUG_EWM1'.
    RETURN.
  ENDIF.

* Check if receiving order already populated
*  IF ch_catsdb-raufnr IS NOT INITIAL.
*    RETURN.
*  ENDIF.

* Check if Absense type exist in exlcusion set, if yes, return
  IF ch_catsdb-awart IS NOT INITIAL.
    SELECT SINGLE valfrom FROM setleaf INTO lv_awart
           WHERE setname = 'ZPM_EWM1_AWART_EXCL' AND
                 valfrom = ch_catsdb-awart.
    IF sy-subrc EQ 0.
*      RETURN.
      IF ch_catsdb-raufnr IS NOT INITIAL.
        CLEAR: ch_catsdb-raufnr, ch_catsdb-vornr, ch_catsdb-lstar.
      ENDIF.
      RETURN.
    ENDIF.
  ENDIF.

* Check if Wage type exist in exlcusion set, if yes, return
  IF ch_catsdb-lgart IS NOT INITIAL.
    SELECT SINGLE valfrom FROM setleaf INTO lv_awart
           WHERE setname = 'ZPM_EWM1_LGART_EXCL' AND
                 valfrom = ch_catsdb-lgart.
    IF sy-subrc EQ 0.
*      RETURN.
      IF ch_catsdb-raufnr IS NOT INITIAL.
        CLEAR: ch_catsdb-raufnr, ch_catsdb-vornr, ch_catsdb-lstar.
      ENDIF.
      RETURN.
    ENDIF.
  ENDIF.

* Check if receiving order already populated
  IF ch_catsdb-raufnr IS NOT INITIAL.
    RETURN.
  ENDIF.

* Read infotype 0027
  CALL FUNCTION 'HR_READ_INFOTYPE_0027'
    EXPORTING
      tclas           = lc_tclas
      pernr           = ch_catsdb-pernr
      infty           = lc_infty
      subty           = lc_subty
      begda           = ch_catsdb-workdate
      endda           = ch_catsdb-workdate
    TABLES
      p0027           = lt_p0027
    EXCEPTIONS
      infty_not_found = 1
      internal_error  = 2
      OTHERS          = 3.

  IF sy-subrc NE 0.
    CASE  sy-subrc.
      WHEN 12.
        MESSAGE w000(zpm) WITH 'Unable to read Internal Order'(001)
                               'Default for employee due to missing'(002)
                               'HR authorizations(Read InfoType 27)'(003).
      WHEN OTHERS.
        MESSAGE w000(zpm) WITH 'Error in reading HR data'(004).
    ENDCASE.
  ELSE.

* Set order if order exist
    READ TABLE lt_p0027 INTO ls_p0027 INDEX 1.
    IF sy-subrc EQ 0.
      IF ls_p0027-auf01 IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_p0027-auf01
          IMPORTING
            output = ch_catsdb-raufnr.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ch_catsdb-raufnr IS INITIAL.
    IF ch_catsdb-rkostl IS NOT INITIAL.
      lv_kostl = ch_catsdb-rkostl.
    ELSE.

      CALL FUNCTION 'HR_READ_INFOTYPE'
        EXPORTING
          tclas           = 'A'
          pernr           = ch_catsdb-pernr
          infty           = '0001'
          begda           = ch_catsdb-workdate
          endda           = ch_catsdb-workdate
        TABLES
          infty_tab       = lt_p0001
        EXCEPTIONS
          infty_not_found = 1
          OTHERS          = 2.
      IF sy-subrc EQ 0.
* Retrieve the cost center
        READ TABLE lt_p0001 INTO ls_p0001 INDEX 1.
        IF sy-subrc EQ 0.
          lv_kostl = ls_p0001-kostl.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_kostl IS NOT INITIAL.
      SELECT SINGLE * INTO ls_hio FROM zhrcat_alt_hio
                      WHERE kostl = lv_kostl.

      IF sy-subrc EQ 0.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_hio-aufnr
          IMPORTING
            output = ch_catsdb-raufnr.
      ELSE.
        MESSAGE w000(zpm) WITH 'Unable to determine IO from'(005)
                               'Infotype 27 and ZHRCAT_ALT_HIO table'(006).
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.
ENDCLASS.
