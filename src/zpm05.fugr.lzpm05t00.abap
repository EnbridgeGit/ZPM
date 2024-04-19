*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCPBUTTONS......................................*
DATA:  BEGIN OF STATUS_ZCPBUTTONS                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCPBUTTONS                    .
CONTROLS: TCTRL_ZCPBUTTONS
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZNTCONFIG.......................................*
DATA:  BEGIN OF STATUS_ZNTCONFIG                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZNTCONFIG                     .
CONTROLS: TCTRL_ZNTCONFIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZNTDEFLT........................................*
DATA:  BEGIN OF STATUS_ZNTDEFLT                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZNTDEFLT                      .
CONTROLS: TCTRL_ZNTDEFLT
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZNTLOGO.........................................*
DATA:  BEGIN OF STATUS_ZNTLOGO                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZNTLOGO                       .
CONTROLS: TCTRL_ZNTLOGO
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZCPBUTTONS                    .
TABLES: *ZNTCONFIG                     .
TABLES: *ZNTDEFLT                      .
TABLES: *ZNTLOGO                       .
TABLES: ZCPBUTTONS                     .
TABLES: ZNTCONFIG                      .
TABLES: ZNTDEFLT                       .
TABLES: ZNTLOGO                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
