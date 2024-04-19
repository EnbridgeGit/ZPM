*&---------------------------------------------------------------------*
*& Report  ZLPMR031_STN_DSN_CAP_IND
*&---------------------------------------------------------------------*
REPORT  zlpmr031_stn_dsn_cap_ind NO STANDARD PAGE HEADING
                                 LINE-SIZE 130 LINE-COUNT 65.
*&---------------------------------------------------------------------*
*& Program Name       : ZLPMR031_STN_DSN_CAP_IND                       *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 17-Aug-2017                                    *
*& Object ID          : ACR-4673                                       *
*& Application Area   : PM                                             *
*& Description        : Update Station design capacity indicator.      *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 17-Aug-2017                                          *
* Modified By   : PANUSURI                                             *
* Correction No : D30K928360                                           *
* Description   : Initial Version                                      *
*----------------------------------------------------------------------*

*Include for data declarations
INCLUDE zlpmr031_stn_dsn_cap_ind_top.
*Include for subroutines
INCLUDE zlpmr031_stn_dsn_cap_ind_f01.

*----------------------------------------------------------------------*
*START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Get data
  PERFORM get_equi_change_data.

*----------------------------------------------------------------------*
*END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Set indicator
  IF ta_v_equi[] IS NOT INITIAL.
    PERFORM set_indicator.
  ELSE.
    WRITE:/ 'No data selected.'(001).
  ENDIF.
