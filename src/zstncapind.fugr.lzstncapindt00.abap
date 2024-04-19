*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMT_STNCAPIND..................................*
DATA:  BEGIN OF STATUS_ZPMT_STNCAPIND                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT_STNCAPIND                .
CONTROLS: TCTRL_ZPMT_STNCAPIND
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPMT_STNCAPIND                .
TABLES: ZPMT_STNCAPIND                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
