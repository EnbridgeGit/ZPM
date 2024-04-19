*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMT_BANNER_TYPE................................*
DATA:  BEGIN OF STATUS_ZPMT_BANNER_TYPE              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT_BANNER_TYPE              .
CONTROLS: TCTRL_ZPMT_BANNER_TYPE
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZPMT_FL_LEVEL...................................*
DATA:  BEGIN OF STATUS_ZPMT_FL_LEVEL                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT_FL_LEVEL                 .
CONTROLS: TCTRL_ZPMT_FL_LEVEL
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZPMT_BANNER_TYPE              .
TABLES: *ZPMT_FL_LEVEL                 .
TABLES: ZPMT_BANNER_TYPE               .
TABLES: ZPMT_FL_LEVEL                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
