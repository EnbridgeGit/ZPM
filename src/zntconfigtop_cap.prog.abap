*&---------------------------------------------------------------------*
*& Include ZNTCONFIGTOP                                      Module Pool      ZNTCONFIG
*&
*&---------------------------------------------------------------------*

PROGRAM  zntconfigtop_cap MESSAGE-ID zntemplate.

TYPES : BEGIN OF ty_config.
        INCLUDE STRUCTURE zntconfig_cap.
TYPES :  functxt TYPE char30,
         check   TYPE char1,
        END OF   ty_config.

TYPES : BEGIN OF ty_tab_help,
         helptxt TYPE char25,
        END OF   ty_tab_help.

TYPES : BEGIN OF ty_prog,
         tcode TYPE syst-tcode,
         pgmna TYPE tstc-pgmna,
        END OF   ty_prog.

DATA : ok_code    TYPE sy-ucomm,
       w_variant  TYPE varid-variant,
       w_filt_tab TYPE zztabtxt.

CONTROLS tabconfig TYPE TABLEVIEW USING SCREEN 9000.

DATA : t_config    TYPE STANDARD TABLE OF ty_config   WITH HEADER LINE,
       t_prog      TYPE STANDARD TABLE OF ty_prog     WITH HEADER LINE,
       t_config_cp TYPE STANDARD TABLE OF ty_config   WITH HEADER LINE,
       t_tab_help  TYPE STANDARD TABLE OF ty_tab_help WITH HEADER LINE.
