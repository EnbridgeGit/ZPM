*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHRCAT_ALT_HIO..................................*
DATA:  BEGIN OF STATUS_ZHRCAT_ALT_HIO                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHRCAT_ALT_HIO                .
CONTROLS: TCTRL_ZHRCAT_ALT_HIO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHRCAT_ALT_HIO                .
TABLES: ZHRCAT_ALT_HIO                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
