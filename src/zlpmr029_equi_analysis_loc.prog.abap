*&---------------------------------------------------------------------*
*& Report  ZLPMR029_EQUI_ANALYSIS_LOC
*&---------------------------------------------------------------------*
REPORT  zlpmr029_equi_analysis_loc NO STANDARD PAGE HEADING
                                   LINE-SIZE 130 LINE-COUNT 65.
*&---------------------------------------------------------------------*
*& Program Name       : ZLPMR029_EQUI_ANALYSIS_LOC                     *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 10-Jul-2017                                    *
*& Object ID          : ACR-4555                                       *
*& Application Area   : PM                                             *
*& Description        : Equipment Analysis by Location.                *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 10-Jul-2017                                          *
* Modified By   : PANUSURI                                             *
* Correction No : D30K928268                                           *
* Description   : Initial Version                                      *
*----------------------------------------------------------------------*
* Version No    : 2.0                                                  *
* Date          : 01-Oct-2018                                          *
* Modified By   : KBANERJEE                                            *
* Correction No : D30K929166                                           *
* Description   : Changes for performance improvement  and correction  *
*                 of timeout error.                                    *
*----------------------------------------------------------------------*

*Include for data declarations
INCLUDE zlpmr029_equi_analysis_loc_top.
*Include for subroutines
INCLUDE zlpmr029_equi_analysis_loc_f01.

*----------------------------------------------------------------------*
*AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name CS 'SO_TPLNR'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name CS 'SO_EQART'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON so_tplnr.
  IF sy-ucomm = 'ONLI' OR sy-ucomm = space.
    IF so_tplnr IS INITIAL.
      MESSAGE 'Fill in all required entry fields.'(001) TYPE 'E'.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON so_eqart.
  IF sy-ucomm = 'ONLI' OR sy-ucomm = space.
    IF so_eqart IS INITIAL.
      MESSAGE 'Fill in all required entry fields.'(001) TYPE 'E'.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
*START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Get data
  PERFORM get_equi_data.
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
