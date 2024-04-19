*&---------------------------------------------------------------------*
*&  Include           ZXWOCTOP
*&---------------------------------------------------------------------*

*-- Global data for PM:D:E-088:OrderHeaderFields
TABLES: zpms_banner_addfields. "For custom screen fields
DATA:  gv_display TYPE flg_show. "Indicator for display only

*-- Global dtaa for PM:D:E-089:Default Reported by
CONSTANTS: gc_reportedby_vname TYPE rvari_vnam VALUE 'ZPM_REPORTEDBY_NOTIFTYPE'. "Constant for variant table name
DATA: gra_qmart TYPE RANGE OF qmart. "Notification Type range table
