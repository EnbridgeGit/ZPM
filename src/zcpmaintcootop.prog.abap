*&---------------------------------------------------------------------*
*& Include ZCPMAINTCOOTOP                                    Module Pool      ZCPMAINTCOO
*&
*&---------------------------------------------------------------------*

PROGRAM  zcpmaintcoo MESSAGE-ID zntemplate.
DATA : ok_code           TYPE sy-ucomm.

DATA : t_config TYPE STANDARD TABLE OF zntconfig WITH HEADER LINE,
       t_selparams TYPE STANDARD TABLE OF rsparams  WITH HEADER LINE,
       lt_vrm_values TYPE TABLE OF vrm_value,
       lt_vrm_values1 TYPE TABLE OF vrm_value,
       w_arbpl type arbpl,
       x_defaults    TYPE zzntdefaults,
       x_nt_defaults TYPE zzntdefaults.

DATA:   w_month TYPE c,
        w_week TYPE c,
        w_year TYPE c,
        w_all TYPE c,
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
        w_tall TYPE c,   "All
        w_chk1 TYPE c, "rdpl
        w_chk2 TYPE c, "pipr
        w_chk3 TYPE c, " rtsc
        w_chk4 TYPE c, " schd
        w_chk5 TYPE c, " asgn
        w_chk6 TYPE c, " wrkg
        w_chk7 TYPE c, " fcom
        w_chk8 TYPE c, " srvr
        w_chk9 TYPE c, " rwvs
        w_chk10 TYPE c, " canc
        w_r_ntmob TYPE c, " not mobile
        w_r_onmob TYPE c, " on mobile
        w_r_all TYPE c," all
        w_r_sto TYPE c, " - P107 TO PLANNING PLANT
        w_r_smc TYPE c, " - P103 TO PLANNING PLANT
        w_chk11 TYPE c,
        w_chk12 TYPE c,
        w_chk13 TYPE c,
        w_chk14 TYPE c,
        w_chk15 TYPE c,
        w_chk16 TYPE c,
        w_chk17 TYPE c,
        w_sto   TYPE c,
        w_smc   TYPE c,
        chk18 type c,
        chk19 type c,
        chk43 type c,
        chk44 type c,
        chk45 type c.


CONSTANTS: c_tcode_iw38   TYPE sy-tcode VALUE 'IW38',
           c_tcode_iw22   TYPE sy-tcode VALUE 'IW22',
           c_tcode_iw28   TYPE sy-tcode VALUE 'IW28',
           c_tcode_iw3d   TYPE sy-tcode VALUE 'IW3D',
           c_tcode_ia05   TYPE sy-tcode VALUE 'IA05',
           c_tcode_ia06   TYPE sy-tcode VALUE 'IA06',
           c_tcode_ia07   TYPE sy-tcode VALUE 'IA07',
           c_tcode_ia01   TYPE sy-tcode VALUE 'IA01',
           c_tcode_ia02   TYPE sy-tcode VALUE 'IA02',
           c_tcode_ia03   TYPE sy-tcode VALUE 'IA03',
           c_tcode_ia08   TYPE sy-tcode VALUE 'IA08',
           c_tcode_ipm3   TYPE sy-tcode VALUE 'IPM3',
           c_tcode_iw37n  TYPE sy-tcode VALUE 'IW37N',
           c_authority_ok TYPE sy-subrc VALUE '1'.
