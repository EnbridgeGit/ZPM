*&---------------------------------------------------------------------*
*&  Include           ZCPENGTOP
*&---------------------------------------------------------------------*

DATA : ok_code           TYPE sy-ucomm,
      c_mode_error        TYPE char1      VALUE 'E',
      c_sync              TYPE char1      VALUE 'S'.
DATA :t_bdcdata   TYPE STANDARD TABLE OF bdcdata   WITH HEADER LINE,
      t_selparams TYPE STANDARD TABLE OF rsparams  WITH HEADER LINE,
      x_defaults    TYPE zzntdefaults,
       x_nt_defaults TYPE zzntdefaults,
       t_config    TYPE STANDARD TABLE OF zntconfig WITH HEADER LINE.
DATA : w_chk TYPE c,
        w_x_retu TYPE c,
        w_x_appr TYPE c,
        w_x_dupl TYPE c,
        c_authority_ok TYPE sy-subrc VALUE '1',
        w_x_canc TYPE c,
        w_month TYPE c,
        w_week TYPE c,
        w_year TYPE c,
        w_all TYPE c,
        w_create TYPE c,
        w_change TYPE c,
        w_display TYPE c,
        w_smc TYPE c,
        w_sto TYPE c.
