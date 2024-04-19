*&---------------------------------------------------------------------*
*& Report  ZLPMR030_EQUI_CHANGE_REP
*&---------------------------------------------------------------------*
REPORT  zlpmr030_equi_change_rep NO STANDARD PAGE HEADING
                                 LINE-SIZE 130 LINE-COUNT 65.
*&---------------------------------------------------------------------*
*& Program Name       : ZLPMR030_EQUI_CHANGE_REP                       *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 04-Aug-2017                                    *
*& Object ID          : ACR-4556                                       *
*& Application Area   : PM                                             *
*& Description        : Equipment change report.                       *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 04-Aug-2017                                          *
* Modified By   : PANUSURI                                             *
* Correction No : D30K928298                                           *
* Description   : Initial Version                                      *
*----------------------------------------------------------------------*

*Include for data declarations
INCLUDE zlpmr030_equi_change_rep_top.
*Include for subroutines
INCLUDE zlpmr030_equi_change_rep_f01.

*----------------------------------------------------------------------*
*START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Get data
  PERFORM get_equi_change_data.
* Get output data
  PERFORM get_output_data.

*----------------------------------------------------------------------*
*END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Display data
  IF ta_final[] IS NOT INITIAL.
    PERFORM display_data.
  ELSE.
    WRITE:/ 'No data selected.'(002).
  ENDIF.
