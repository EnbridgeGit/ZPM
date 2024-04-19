*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMT_SETMT_RULES................................*
DATA:  BEGIN OF STATUS_ZPMT_SETMT_RULES              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT_SETMT_RULES              .
CONTROLS: TCTRL_ZPMT_SETMT_RULES
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPMT_SETMT_RULES              .
TABLES: ZPMT_SETMT_RULES               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
