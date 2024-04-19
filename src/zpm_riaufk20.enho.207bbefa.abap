"Name: \PR:RIAUFK20\EX:RIAUFK20_02\EI
ENHANCEMENT 0 ZPM_RIAUFK20.
SELECTION-SCREEN BEGIN OF BLOCK ZBANNER WITH FRAME TITLE title.
*-- Compliance date
    SELECTION-SCREEN BEGIN OF line.
    SELECTION-SCREEN COMMENT 1(20) cmcodate for FIELD zscodate.
    Select-OPTIONS zscodate FOR gw_banner-ZZPMCOMPDATE.
    SELECTION-SCREEN end OF line.

*-- Banner work order
    SELECTION-SCREEN BEGIN OF line.
    SELECTION-SCREEN COMMENT 1(20) cmbword for FIELD zsbword.
    Select-OPTIONS zsbword FOR gw_banner-ZZPMBWORKORD.
    SELECTION-SCREEN end OF line.

*-- Banner work type
    SELECTION-SCREEN BEGIN OF line.
    SELECTION-SCREEN COMMENT 1(20) cmbwtyp for FIELD zsbwtyp.
    Select-OPTIONS zsbwtyp FOR gw_banner-ZZPMBWORKTYPE.
    SELECTION-SCREEN end OF line.

*-- Banner Scheduled Date
    SELECTION-SCREEN BEGIN OF line.
    SELECTION-SCREEN COMMENT 1(20) cmbsdate for FIELD zsbsdate.
    Select-OPTIONS zsbsdate FOR gw_banner-ZZPMBSCHEDDATE.
    SELECTION-SCREEN end OF line.

*-- Banner Status
    SELECTION-SCREEN BEGIN OF line.
    SELECTION-SCREEN COMMENT 1(20) cmbstatu for FIELD zsbstatu.
    Select-OPTIONS zsbstatu FOR gw_banner-ZZPMBSTATUS.
    SELECTION-SCREEN end OF line.

selection-SCREEN END OF block zbanner.

ENDENHANCEMENT.
