*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMT_LIST_MEAS..................................*
DATA:  BEGIN OF STATUS_ZPMT_LIST_MEAS                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT_LIST_MEAS                .
CONTROLS: TCTRL_ZPMT_LIST_MEAS
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZPMT_MEAS_POS...................................*
DATA:  BEGIN OF STATUS_ZPMT_MEAS_POS                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT_MEAS_POS                 .
CONTROLS: TCTRL_ZPMT_MEAS_POS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPMT_LIST_MEAS                .
TABLES: *ZPMT_MEAS_POS                 .
TABLES: ZPMT_LIST_MEAS                 .
TABLES: ZPMT_MEAS_POS                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
