"Name: \FU:CATS_ENRICH_INPUT\SE:BEGIN\EI
ENHANCEMENT 0 ZPM_CATS_ENRICH_INPUT.
*

  FIELD-SYMBOLS: <fs_catsdb> TYPE catsdb_ext.

  LOOP AT enrich_catsdb ASSIGNING <fs_catsdb>.

* Default Recieving order
  CALL METHOD zcl_pm_cat2_ewm_update=>set_receiving_order
    EXPORTING
      im_variant  = enrich_cats-variant
    CHANGING
      ch_catsdb   = <fs_catsdb>.

  ENDLOOP.

ENDENHANCEMENT.
