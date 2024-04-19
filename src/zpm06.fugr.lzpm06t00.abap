*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMT_MAIN_MATPNT................................*
DATA:  BEGIN OF STATUS_ZPMT_MAIN_MATPNT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT_MAIN_MATPNT              .
CONTROLS: TCTRL_ZPMT_MAIN_MATPNT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPMT_MAIN_MATPNT              .
TABLES: ZPMT_MAIN_MATPNT               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
