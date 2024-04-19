*&---------------------------------------------------------------------*
*&  Include           ZXWOCU21
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
*-- Automate the population of Reported By field in notification
*--   during creation of notification.
*-- EAMagine - EAM-PM-E-089-N
*--------------------------------------------------------------------*

*-- Select data from the variant table only once.
  IF gra_qmart IS INITIAL.
*-- Select options mainteained in variant table
    SELECT sign opti low high FROM tvarvc INTO TABLE gra_qmart
        WHERE name = gc_reportedby_vname.
  ENDIF.

  IF e_viqmel-qmart IN gra_qmart.
*-- Default request by to the current user
    IF e_viqmel-qmnam IS INITIAL.
      e_viqmel-qmnam = sy-uname.
    ENDIF.

  ENDIF.
