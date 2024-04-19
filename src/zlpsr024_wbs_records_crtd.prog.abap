*&---------------------------------------------------------------------*
*& Report  ZLPSR024_WBS_RECORDS_CRTD
*&
*&---------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 25-Sep-2018                                          *
* Created By    : AKMADASU                                             *
* Correction No : S01K900775                                           *
* Object ID     :                                                      *
* Description   : WBS records report to shows only 'CRTD' status       *
*                 but with 'NO' settlement rule GL account exist       *
*----------------------------------------------------------------------*

REPORT  zlpsr024_wbs_records_crtd.

* Global declarations and selection screen
INCLUDE zlpsr024_wbs_records_top.

* Subroutines
INCLUDE zlpsr024_wbs_records_f01.

START-OF-SELECTION.
* Get WBS Element Master Data
  PERFORM get_prps.

* Get Object status.
  PERFORM get_jest.

* Get Distribution Rules Settlement Rule Order Settlement
  PERFORM get_cobrb.

END-OF-SELECTION.
* Process and Prepare final table
  PERFORM prepare_final_table.

  IF gt_final IS NOT INITIAL.
*   Instantiate the ALV object
    PERFORM instance_alv.
*   Prepare field catalog and column names
    PERFORM field_catalog.
*   Display data
    PERFORM display_data.
  ELSE.
    MESSAGE text-025 TYPE 'S'." DISPLAY LIKE 'S'.
    LEAVE LIST-PROCESSING.
  ENDIF.
