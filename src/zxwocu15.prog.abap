*&---------------------------------------------------------------------*
*&  Include           ZXWOCU15
*&---------------------------------------------------------------------*

*-- Move data from input structure into custom structure

*--  Banner work order data
MOVE-CORRESPONDING coci_aufk_imp TO zpms_banner_addfields.

*-- Display only indicator
*gv_display = sap_display_only.
*-- Always set to display
gv_display = abap_true.
