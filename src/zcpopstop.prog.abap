*&---------------------------------------------------------------------*
*& Include ZCPOETOP                                          Module Pool      ZCPOE
*&
*&---------------------------------------------------------------------*

PROGRAM  zcpops MESSAGE-ID zntemplate.

TABLES : zntlogo.


CONSTANTS: c_tcode_iw38   TYPE sy-tcode VALUE 'IW38',
           c_tcode_iw28   TYPE sy-tcode VALUE 'IW28',
           c_authority_ok TYPE sy-subrc VALUE '1',
           c_mode_error   TYPE char1    VALUE 'E',
           c_sync         TYPE char1    VALUE 'S',
           c_tcode_iw22   TYPE sy-tcode VALUE 'IW22',
           c_tcode_iw3d   TYPE sy-tcode VALUE 'IW3D',
           c_tcode_iw66   TYPE sy-tcode VALUE 'IW66',
           c_tcode_ik33   TYPE sy-tcode VALUE 'IK33',
           c_tcode_il03   TYPE sy-tcode VALUE 'IL03',
           c_tcode_ih06   TYPE sy-tcode VALUE 'IH06',
           c_tcode_ie03   TYPE sy-tcode VALUE 'IE03',
           c_tcode_ih08   TYPE sy-tcode VALUE 'IH08',
           c_tcode_iw37n TYPE sy-tcode VALUE 'IW37N'.

DATA: t_selparams TYPE STANDARD TABLE OF rsparams  WITH HEADER LINE,
      t_config    TYPE STANDARD TABLE OF zntconfig WITH HEADER LINE,
      t_bdcdata   TYPE STANDARD TABLE OF bdcdata   WITH HEADER LINE,
      lt_vrm_values TYPE TABLE OF vrm_value,
      x_defaults    TYPE zzntdefaults,
      x_nt_defaults TYPE zzntdefaults,
      w_create TYPE c,
      w_change TYPE c,
      w_display TYPE c,
      w_rev_code TYPE t352r-revnr,
      w_tr  TYPE c,      "Radio button for revision code
      w_t0 TYPE c,    "This week
      w_t1 TYPE c,      "Next Week
      w_t2 TYPE c,      "Two Weeks
      w_t3 TYPE c,      "T-3 THRU T-6 3-6 WEEKS FROM NOW
      w_t6 TYPE c,      "GREATER THAN 6 WEEKS IN FUTURE
      w_tall TYPE c.   "All
DATA : w_avail TYPE c,
w_s TYPE c,
w_uns TYPE c,
w_online TYPE c,
w_nd TYPE c,
w_d TYPE c.

DATA:ok_code LIKE sy-ucomm,
     gv_ucomm LIKE sy-ucomm,
     gv_value TYPE string,
     t_value  LIKE t352r-revnr.
