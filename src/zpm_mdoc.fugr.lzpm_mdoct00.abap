*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPM_FOXBORO_FILE................................*
DATA:  BEGIN OF STATUS_ZPM_FOXBORO_FILE              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPM_FOXBORO_FILE              .
CONTROLS: TCTRL_ZPM_FOXBORO_FILE
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZPM_FOXBORO_INTE................................*
DATA:  BEGIN OF STATUS_ZPM_FOXBORO_INTE              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPM_FOXBORO_INTE              .
CONTROLS: TCTRL_ZPM_FOXBORO_INTE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPM_FOXBORO_FILE              .
TABLES: *ZPM_FOXBORO_INTE              .
TABLES: ZPM_FOXBORO_FILE               .
TABLES: ZPM_FOXBORO_INTE               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
