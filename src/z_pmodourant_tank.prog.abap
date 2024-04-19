*&---------------------------------------------------------------------*
*& Report  Z_PMODOURANT_TANK
* Author             : Shiladitya Ghosh                                *
* Date               : 17/07/2014 (dd/mm/yyyy)                         *
* Technical Contact  : Eldhose Mathew                                  *
* Business Contact   : Eric VanRumybeke                                *
* Purpose            : This report is required to read the the last    *
*                      recorded levels for all Odourant Tanks and then *
*                      calculate the percentage that the Odourant tanks*
*                      are full. This report supports the odourant tank*
*                      filling and odourant ordering process.          *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By     CTS          Description                 *
*&---------------------------------------------------------------------*
*& 08/09/2017  PANUSURI    D30K928328  Exclude equipment with status   *
*                                      INAC and DLFL.                  *
* 17/07/2018  AKMADASU  D30K928853 CHG0116397 - Performance improvemnet*
*&---------------------------------------------------------------------*

REPORT  z_pmodourant_tank.

**********************************************************
* Includes
**********************************************************
INCLUDE z_pmodourant_tank_top.   " Data declaration and selection screen
INCLUDE z_pmodourant_tank_f01.   " Dealing data fetch

**********************************************************
* Initialization
**********************************************************
INITIALIZATION.

* Date defaulted to today's date
  p_date = sy-datum.

**********************************************************
* F4 Value request
**********************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_ttype-low.

* F4 help for Tank type
  PERFORM f4_tank_type.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_ttype-high.

* F4 help for Tank type
  PERFORM f4_tank_type.

**********************************************************
* Start of Selection
**********************************************************
START-OF-SELECTION.

* Date defaulted to today's date
  IF p_date IS INITIAL.
    p_date = sy-datum.
  ENDIF.

* Get Data
  PERFORM get_data.

**********************************************************
* End of Selection
**********************************************************
END-OF-SELECTION.

* Prepare Output table
  PERFORM prepare_output.

* ALV display
  PERFORM display_alv1.
