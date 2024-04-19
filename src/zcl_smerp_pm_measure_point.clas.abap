class ZCL_SMERP_PM_MEASURE_POINT definition
  public
  create public .

public section.
*"* public components of class ZCL_SMERP_PM_MEASURE_POINT
*"* do not include other source files here!!!

  interfaces /SYCLO/IF_CORE_FILTER_HANDLER .
*"* protected components of class ZCL_SMERP_PM_MEASURE_POINT
*"* do not include other source files here!!!
protected section.
private section.
*"* private components of class ZCL_SMERP_PM_MEASURE_POINT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_SMERP_PM_MEASURE_POINT IMPLEMENTATION.


METHOD /SYCLO/IF_CORE_FILTER_HANDLER~GET_FILTER_LEVEL.
*======================================================================*
*<SYCLODOC>
*  <CREATE_DATE> 12/13/2010 </CREATE_DATE>
*  <AUTHOR> Syam Yalamati(Syclo LLC) </AUTHOR>
*  <DESCRIPTION>
*     Set filter level to Mobile Application
*  </DESCRIPTION>
*<!-- *----------------------CHANGE HISTORY-------------------------* -->
*  <REVISION_TAG date='12/13/2010' version='320_700 SP6' user='SYALAMA' >
*    <DESCRIPTION> Initial release. </DESCRIPTION>
*  </REVISION_TAG>
*<!-- *-------------------------------------------------------------* -->
*</SYCLODOC>
*======================================================================*
  ev_filter_level = 1.
ENDMETHOD.


METHOD /syclo/if_core_filter_handler~get_filter_value.
*----------------------------------------------------------------------*
* Report Name:      ZCL_SMERP_PM_MEASURE_POINT
* Author:           KBANERJEE
* Date:	            26-JUN-2019
* Logical Database: NA
* SAPScript name:   NA
* Application Area: Plant Management(PM)
* Description:      Performance issue with RoundManager.Measuring point
*                   added  to improve performance impact.
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 26-JUN-2019  KBANERJEE   D30K929971  CHG0132185 -Initial development *
*                                                                      *
************************************************************************
*Local table types
  TYPES:BEGIN OF lty_mept,
         point TYPE imrc_point,
    END OF lty_mept.
*Internal tables
  DATA: lt_mept  TYPE STANDARD TABLE OF lty_mept
                 INITIAL SIZE 0,
        ls_mept  TYPE lty_mept.
*Local constants
  CONSTANTS:lc_i   TYPE bapisign   VALUE 'I',
            lc_eq  TYPE bapioption VALUE 'EQ'.
*Field Symbols
  FIELD-SYMBOLS: <lfs_filter_value> TYPE /syclo/core_filter_value_str.

  REFRESH et_filter_value.

* This Data Object Filter includes the measuring points
  REFRESH lt_mept.
  SELECT point "Measuring Point
    FROM imep
    INTO TABLE lt_mept.
  IF sy-subrc IS INITIAL.
* Return all measuring points in the hierarchy
    LOOP AT lt_mept INTO ls_mept.

      APPEND INITIAL LINE TO et_filter_value
                   ASSIGNING <lfs_filter_value>.
      <lfs_filter_value>-dof_name = iv_dof_name.
      <lfs_filter_value>-sign     = lc_i.
      <lfs_filter_value>-option   = lc_eq.
      <lfs_filter_value>-low      = ls_mept-point.
      CLEAR ls_mept.
    ENDLOOP.
  ENDIF.
ENDMETHOD.


method /SYCLO/IF_CORE_FILTER_HANDLER~GET_TABLE_FILTER_VALUE.
*======================================================================*
*<SMERPDOC>
*  <CREATE_DATE> 5/4/2014 </CREATE_DATE>
*  <AUTHOR> Jirong Wang (SAP Labs) </AUTHOR>
*  <DESCRIPTION>
*    Default implementation.
*  </DESCRIPTION>
*<!-- *----------------------CHANGE HISTORY-------------------------* -->
*  <REVISION_TAG date='5/4/2014' version='SMERP 610_700 SP03' user='WANGJIR' >
*    <DESCRIPTION> Initial release. </DESCRIPTION>
*  </REVISION_TAG>
*<!-- *-------------------------------------------------------------* -->
*</SMERPDOC>
*======================================================================*
**********************************************************************
* Data Declaration Section
**********************************************************************
*

**********************************************************************
* Main Section
**********************************************************************

  clear et_filter_value.

endmethod.
ENDCLASS.
