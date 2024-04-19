*&---------------------------------------------------------------------*
*& Report  Z_PM_BANNER_INTERFACE_ANALYSIS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  y_pm_banner_interface_analysis.

INCLUDE Y_PM_BANNER_INTERFACE_ANALYTOP.
*INCLUDE z_pm_banner_interface_analytop.
INCLUDE Y_PM_BANNER_INTERFACE_ANALYF01.
*INCLUDE z_pm_banner_interface_analyf01.

**********************************************************
* Start of Selection
**********************************************************
START-OF-SELECTION.

* Prepare
  PERFORM prepare.

**********************************************************
* End of Selection
**********************************************************
END-OF-SELECTION.

* ALV display
  PERFORM display_alv.

  IF sy-batch IS NOT INITIAL AND
     gt_email IS NOT INITIAL.
*  display email
    PERFORM send_email.
  ENDIF.
