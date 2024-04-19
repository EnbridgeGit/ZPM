*&---------------------------------------------------------------------*
*&  Include           ZXWOCU24
*&---------------------------------------------------------------------*

************************************************************************
* PROGRAM: ZXWOCU24
* Date:  June 16th 2014
* Description:
*        This customer exist is to automatically determine the
*        the settlement rule based on functional location and
*        Maintananence activity
***********************************************************************
* AUTHOR : ELDHOSE MATHEW ( PWC )
***********************************************************************
*"----------------------------------------------------------------------
*"  IMPORTING
*"     REFERENCE(CAUFVD_IMP) LIKE  CAUFVD STRUCTURE  CAUFVD
*"  TABLES
*"      PMDFU_TAB STRUCTURE  PMDFU OPTIONAL
*"      RIWOL_TAB STRUCTURE  RIWOL OPTIONAL
*"      COMP_TAB STRUCTURE  RESBDGET OPTIONAL
*"      OPR_TAB STRUCTURE  AFVGDGET OPTIONAL
*"  CHANGING
*"     REFERENCE(APROF) LIKE  TKB1A-APROF OPTIONAL
*"  EXCEPTIONS
*"      DO_NOT_BUILD_SETTLEMENTRULE
*"----------------------------------------------------------------------

INCLUDE rbonrart.
CONSTANTS: yc_perbz_f TYPE perbz VALUE 'GES', " Full settlement
           yc_perbz_p TYPE perbz VALUE 'PER', "Partial Settlement
           yc_prozs TYPE prozs VALUE '100'. " 100% Settlement rate

DATA: lwa_pmdfu TYPE pmdfu,
      lv_konty  TYPE konty,
      lv_tplnr  TYPE tplnr,
      lv_lengt  TYPE int4,
      lv_anln1  TYPE anln1,
      lv_anln2  TYPE anln2,
      lv_first  TYPE char4,
      lv_secnd  TYPE char30,
      lv_matnr  TYPE matnr,
      lv_hkont  TYPE hkont,
      lv_nplnr  TYPE nplnr,
      lv_kdauf  TYPE kdauf,
      lv_kdpos  TYPE kdpos,
      lv_posnr  TYPE posnr,
      lv_vornr  TYPE vornr,
      lv_objnr  TYPE ionra-objnr,
      lv_gsber  TYPE gsber,
      lv_dummy  TYPE char1,
      lv_aufnr  TYPE aufnr,
      lv_pspnr  TYPE cobrb-ps_psp_pnr,
      lv_prznr  TYPE co_prznr,
      lv_kostl  TYPE brgkostl,
      lv_empge  TYPE abr_emp,
      lit_setmt TYPE TABLE OF zpmt_setmt_rules.

FIELD-SYMBOLS: <lfs_setmt> TYPE zpmt_setmt_rules.

** Get Account assignment category and Settlement reciever from Custom table
** based on Functional location and Maintanence type
*SELECT SINGLE konty
*              empge
*       FROM zpmt_setmt_rules
*       INTO (lv_konty, lv_empge)
*       WHERE tplnr = caufvd_imp-tplnr AND
*             ilart = caufvd_imp-ilart.

** proceed only if record exists. Else display information message
*IF sy-subrc NE 0.
*  MESSAGE i001(zpm) WITH caufvd_imp-tplnr caufvd_imp-ilart.
*  RETURN.
*ENDIF.

*SELECT *
*       FROM zpmt_setmt_rules
*       INTO TABLE lit_setmt
*       WHERE ( auart = '*' OR auart = caufvd_imp-auart ) AND
*             ( vaplz = '*' OR vaplz = caufvd_imp-vaplz ) AND
*             ( ilart = '*' OR ilart = caufvd_imp-ilart ).

SELECT *
       FROM zpmt_setmt_rules
       INTO TABLE lit_setmt
       WHERE ( auart = '*' OR auart = caufvd_imp-auart ) AND
             ( ilart = '*' OR ilart = caufvd_imp-ilart ).

CHECK sy-subrc EQ 0.

LOOP AT lit_setmt ASSIGNING <lfs_setmt>.
  IF <lfs_setmt>-auart = '*'.
    <lfs_setmt>-auart = caufvd_imp-auart.
  ENDIF.

  IF caufvd_imp-vaplz CP <lfs_setmt>-vaplz.
    <lfs_setmt>-vaplz = caufvd_imp-vaplz.
  ENDIF.

  IF caufvd_imp-ilart CP <lfs_setmt>-ilart.
    <lfs_setmt>-ilart = caufvd_imp-ilart.
  ENDIF.

  IF caufvd_imp-tplnr CP <lfs_setmt>-tplnr.
    <lfs_setmt>-tplnr = caufvd_imp-tplnr.
  ENDIF.
ENDLOOP.

SORT lit_setmt BY auart vaplz tplnr ilart.
READ TABLE lit_setmt ASSIGNING <lfs_setmt>
                     WITH KEY auart = caufvd_imp-auart
                              vaplz = caufvd_imp-vaplz
                              tplnr = caufvd_imp-tplnr
                              ilart = caufvd_imp-ilart
                              BINARY SEARCH.

CHECK  sy-subrc EQ 0.

lv_konty = <lfs_setmt>-konty.
lv_empge = <lfs_setmt>-empge.
*
*SORT lit_setmt BY auart DESCENDING vaplz DESCENDING ilart DESCENDING tplnr DESCENDING.
*
*LOOP AT lit_setmt ASSIGNING <lfs_setmt>.
*
*  IF <lfs_setmt>-tplnr CS '*'.
*    lv_tplnr = <lfs_setmt>-tplnr.
*    REPLACE ALL OCCURRENCES OF '*' IN lv_tplnr WITH space.
*    CONDENSE lv_tplnr.
*    lv_lengt = strlen( lv_tplnr ).
*    IF lv_tplnr = caufvd_imp-tplnr+0(lv_lengt).
*      lv_konty = <lfs_setmt>-konty.
*      lv_empge = <lfs_setmt>-empge.
*      EXIT.
*    ENDIF.
*  ELSE.
*    IF lv_tplnr = caufvd_imp-tplnr.
*      lv_konty = <lfs_setmt>-konty.
*      lv_empge = <lfs_setmt>-empge.
*      EXIT.
*    ENDIF.
*  ENDIF.
*ENDLOOP.

CASE lv_konty.
  WHEN space.
    RETURN.

  WHEN objektart_an. "Asset
    SPLIT lv_empge AT space INTO lv_first lv_secnd. " Remove the compnay code
    SPLIT lv_secnd AT '-' INTO lv_anln1 lv_anln2. "Get Asset class & Subclass

* Convert to internal format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_anln1
      IMPORTING
        output = lv_anln1.

    lwa_pmdfu-fdind = lv_anln1.

* Convert to internal format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_anln2
      IMPORTING
        output = lv_anln2.

    lwa_pmdfu-fdind2 = lv_anln2.

  WHEN objektart_ks. "Cost Center
    lv_kostl = lv_empge+0(12).

* Convert to internal format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_kostl
      IMPORTING
        output = lv_kostl.

    lwa_pmdfu-fdind = lv_kostl.

  WHEN objektart_or. "Order
    lv_aufnr = lv_empge.

* Convert to internal format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_aufnr
      IMPORTING
        output = lv_aufnr.

    lwa_pmdfu-fdind = lv_aufnr.

  WHEN objektart_bp. "Business Process
    lv_prznr = lv_empge.
* Convert to internal format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_prznr
      IMPORTING
        output = lv_prznr.

    lwa_pmdfu-fdind = lv_prznr.

  WHEN objektart_pr. "WBS
* Convert to internal format
    PERFORM empge_to_pspnr IN PROGRAM saplkobs IF FOUND
                           USING lv_empge
                           CHANGING lv_pspnr
                                    lv_kostl.

    lwa_pmdfu-fdind = lv_pspnr.

  WHEN objektart_ma. "Material

* Convert to internal format
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = lv_empge
      IMPORTING
        output       = lv_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    lwa_pmdfu-fdind = lv_matnr.

  WHEN objektart_sk. "G/L Account
    SPLIT lv_empge AT space INTO lv_dummy lv_hkont lv_gsber.

* Convert to internal format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_hkont
      IMPORTING
        output = lv_hkont.

    lwa_pmdfu-fdind = lv_hkont.

* Convert to internal format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_gsber
      IMPORTING
        output = lv_gsber.

    lwa_pmdfu-fdind2 = lv_gsber.

  WHEN objektart_eo.
    lwa_pmdfu-fdind = lv_empge.
    lwa_pmdfu-fdind2 = '0001'.

  WHEN objektart_np.
    SPLIT lv_empge AT space INTO lv_nplnr lv_vornr lv_dummy.

* Convert to internal format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_nplnr
      IMPORTING
        output = lv_nplnr.

    lwa_pmdfu-fdind = lv_nplnr.

* Convert to internal format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_vornr
      IMPORTING
        output = lv_vornr.

    lwa_pmdfu-fdind2 = lv_vornr.

  WHEN objektart_vb.
    SPLIT lv_empge AT space INTO lv_kdauf lv_kdpos lv_dummy.

* Convert to internal format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_kdauf
      IMPORTING
        output = lv_kdauf.

    lwa_pmdfu-fdind = lv_kdauf.

* Convert to internal format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_kdpos
      IMPORTING
        output = lv_kdpos.

    lwa_pmdfu-fdind2 = lv_kdpos.

  WHEN objektart_hp.
    lwa_pmdfu-fdind = lv_empge.

  WHEN objektart_op.
    REPLACE ALL OCCURRENCES OF space IN lv_empge WITH '/'.
    SPLIT lv_empge AT '/' INTO lv_aufnr lv_posnr lv_dummy.

* Convert to internal format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_aufnr
      IMPORTING
        output = lv_aufnr.

    lwa_pmdfu-fdind = lv_aufnr.

* Convert to internal format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_posnr
      IMPORTING
        output = lv_posnr.

    lwa_pmdfu-fdind2 = lv_posnr.

  WHEN objektart_ia OR objektart_ib OR objektart_ic OR objektart_ig OR
       objektart_im OR objektart_iv OR objektart_iw OR objektart_is OR objektart_i1.

* Convert to internal format
    CALL FUNCTION 'REMD_EMPGE_TO_OBJNR'
      EXPORTING
        i_obart   = lv_konty
        i_empge   = lv_empge
      IMPORTING
        e_objnr   = lv_objnr
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lwa_pmdfu-fdind = lv_objnr.
ENDCASE.

lwa_pmdfu-konty = lv_konty.
lwa_pmdfu-perbz = yc_perbz_f.
lwa_pmdfu-prozs = yc_prozs.
APPEND lwa_pmdfu TO pmdfu_tab.

lwa_pmdfu-konty = lv_konty.
lwa_pmdfu-perbz = yc_perbz_p.
lwa_pmdfu-prozs = yc_prozs.
APPEND lwa_pmdfu TO pmdfu_tab.
CLEAR lwa_pmdfu.
