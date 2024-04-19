REPORT z_pmr028_text_items NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65.
************************************************************************
*    Program     :  Z_PMR028_TEXT_ITEMS
*    Programmer  :  Eldhose Mathew
*    Client      :  Union Gas Limited
*    Date        :  February 2015
*
* This report will list the information on all outstanding reservations
* by plant and storage locations for text items
*
* 2017/01/25 GYMANA ACR2842 D30K927885 - Modify report headers and add
*                                        Functional Loc column
************************************************************************

TABLES  : resb,          "Reservations
          makt,          "Material Description
          marc,          "Plant Information
          mard,          "Storage Location Information
          ekpo,          "Purchase order detail
          ekbe,          "Purchase order history
          rkpf.          "Reservation Document Header

DATA    : qtyhand         TYPE i,
          qooamt          TYPE i,
          temp-bdmng      LIKE resb-bdmng.

*-------------------------- For Excel SpreadSheet ----------------------
DATA:  retcode         LIKE sy-subrc,
       w_option(11)    TYPE c         VALUE 'start_excel',
       w_head01(40)    TYPE c,
       w_head02(40)    TYPE c,
       w_repttl        LIKE sy-title.

DATA:  BEGIN OF prot_header  OCCURS 1,
       spaltenname(20) TYPE c,
       ddic_table(5)   TYPE c,
       ddic_field(5)   TYPE c,
       key             TYPE c,
       END OF prot_header.

DATA:  BEGIN OF exceltab     OCCURS 0,
       werks     LIKE mard-werks,
       lgort     LIKE mard-lgort,
       matnr     LIKE mard-matnr,
       postp     TYPE char8,
       maktx     TYPE c LENGTH 1000,
       dismm     LIKE marc-dismm,
       bdter     LIKE resb-bdter,
       rsnum(10) TYPE c,
       rspos(4)  TYPE c,
       bdmng(10) TYPE c,
       meins     LIKE resb-meins,
       ablad     TYPE char20,
       accounting(20) TYPE c,
       wempf     LIKE resb-wempf,            "Recipient
       fing      TYPE char12,
       arbpl     TYPE char11,
       pltxt     TYPE iflotx-pltxt,                         "D30K927885
       END OF exceltab.

TYPES: BEGIN OF ty_twlad,
       werks      TYPE werks_d,
       lgort      TYPE lgort_d,
       adrnr      TYPE ad_addrnum,
       END OF ty_twlad,

       BEGIN OF ty_adrc,
       addrnumber TYPE ad_addrnum,
       name4      TYPE ad_name4,
       END OF ty_adrc,

       BEGIN OF ty_beber,
        aufnr     TYPE aufnr,
        fing      TYPE t357-fing,
       END OF ty_beber,

       BEGIN OF ty_arbid,
        aufpl    TYPE resb-aufpl,
        arbpl    TYPE arbpl,
       END OF ty_arbid.

DATA: lt_twlad   TYPE STANDARD TABLE OF ty_twlad,
      lwa_twlad  TYPE ty_twlad,
      lt_adrc    TYPE STANDARD TABLE OF ty_adrc,
      lwa_adrc   TYPE ty_adrc.

DATA: gs_tline   TYPE tline,
      gs_beber   TYPE ty_beber,
      gs_arbid   TYPE ty_arbid,
      gt_lines   TYPE STANDARD TABLE OF tline,
      gv_name    TYPE thead-tdname.

DATA:  errortab LIKE hrerror OCCURS 0 WITH HEADER LINE.

DATA: gv_bdter TYPE bdter,
      gt_resb  TYPE STANDARD TABLE OF resb,
      gt_rkpf  TYPE STANDARD TABLE OF rkpf,
      gt_marc  TYPE STANDARD TABLE OF marc,
      gt_mard  TYPE STANDARD TABLE OF mard,
      gt_beber TYPE SORTED TABLE OF ty_beber WITH NON-UNIQUE KEY aufnr,
      gt_arbid TYPE SORTED TABLE OF ty_arbid WITH NON-UNIQUE KEY aufpl,
      gt_makt  TYPE STANDARD TABLE OF makt,
      gt_ekpo  TYPE STANDARD TABLE OF ekpo,
      gt_ekbe  TYPE STANDARD TABLE OF ekbe.
*--------------------- End of EXCEL SpreadSheet ------------------------

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) text-001.

SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-102.

SELECT-OPTIONS:
     s_plant         FOR   marc-werks,
     s_storlo        FOR   mard-lgort,
     s_matnum        FOR   marc-matnr.
SELECTION-SCREEN END OF BLOCK box1.

SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-103.

PARAMETERS: p_date    LIKE sy-datum  OBLIGATORY.
PARAMETERS: p_pltm    TYPE char3.
SELECTION-SCREEN END OF BLOCK box2.

SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-100.
PARAMETERS: p_mr_al TYPE c RADIOBUTTON GROUP rvtp,
            p_mr_mn TYPE c RADIOBUTTON GROUP rvtp,
            p_mr_pm TYPE c RADIOBUTTON GROUP rvtp DEFAULT 'X'.
PARAMETERS: p_text TYPE c AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK box3.

SELECTION-SCREEN BEGIN OF BLOCK box4 WITH FRAME TITLE text-101.
PARAMETERS:  p_excl  RADIOBUTTON GROUP rbcr,   "Excel Spreadsheet
             p_file LIKE rlgrap-filename
                    DEFAULT 'H:\ZPMOUTRES_T.xlsx',
             p_rprt  RADIOBUTTON GROUP rbcr.   "Print Report
SELECTION-SCREEN  END OF BLOCK box4.

******************************************************************
*                   AT SELECTION-SCREEN                          *
******************************************************************

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: wif_window_title        TYPE string VALUE 'Please Select File',
        wif_initial_directory   TYPE string VALUE 'h:\',
        wit_filename_tab        TYPE filetable WITH HEADER LINE,
        wif_rc                  TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wif_window_title
*     DEFAULT_EXTENSION       =
*     default_filename        = wif_default_filename
*     FILE_FILTER             = WIF_FILE_FILTER
      initial_directory       = wif_initial_directory
*     MULTISELECTION          =
    CHANGING
      file_table              = wit_filename_tab[]
      rc                      = wif_rc
*     USER_ACTION             =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF ( sy-subrc = 0 ).
*Return user selection
    READ TABLE wit_filename_tab INDEX 1.
    IF sy-subrc IS INITIAL AND wif_rc > 0.
      p_file = wit_filename_tab.
    ELSE.
      CLEAR p_file.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_file.
  PERFORM check_file_path.

*******************************  MAIN  *********************************
TOP-OF-PAGE.
  WRITE: /1 text-rpt, sy-repid COLOR COL_NEGATIVE INVERSE ON,
         58 text-002 COLOR COL_HEADING,
        105 text-dte, sy-datum, text-amp, sy-uzeit.
  WRITE:  / text-clt UNDER text-rpt, sy-mandt,
         56 text-901 INTENSIFIED OFF,
             p_date COLOR COL_GROUP INVERSE ON, text-014,
             text-pge UNDER text-dte, sy-pagno.
  WRITE: /.
  ULINE.
  FORMAT COLOR COL_NORMAL.
  WRITE: /1 text-020, text-028, text-004, text-005, text-029,
            text-021, text-024,
            text-007, text-008,
            text-031 RIGHT-JUSTIFIED,
            text-040,
            text-201,
            text-042 RIGHT-JUSTIFIED,
            text-043,
            text-006,
            text-010,
            text-046.                                       "D30K927885

  ULINE.
  WRITE: /.

*-------------------------- START-OF-SELECTION -------------------------
START-OF-SELECTION.
  FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
  CLEAR exceltab.
  REFRESH exceltab.

* Determination of Address from Plant and Storage Location
  SELECT werks
         lgort
         adrnr
         FROM twlad
         INTO TABLE lt_twlad
         WHERE lfdnr = '001'.

* Addresses (Business Address Services)
  SELECT addrnumber
         name4
         FROM adrc
         INTO TABLE lt_adrc.

  gv_bdter = p_date + p_pltm.
  SELECT * FROM resb
    INTO TABLE gt_resb
    WHERE matnr IN s_matnum
      AND werks IN s_plant
      AND lgort IN s_storlo
      AND xloek NE 'X'
      AND kzear NE 'X'
      AND bdter LE gv_bdter.

  IF sy-subrc EQ 0.
    SELECT * FROM rkpf
    INTO TABLE gt_rkpf
    FOR ALL ENTRIES IN gt_resb
    WHERE rsnum = gt_resb-rsnum.

    IF sy-subrc EQ 0.
      SORT gt_rkpf BY rsnum.
    ENDIF.

    SELECT a~aufnr
           c~fing
           FROM afih AS a INNER JOIN iloa AS b ON a~mandt = b~mandt AND
                                                  a~iloan = b~iloan
                          INNER JOIN t357 AS c ON b~mandt = c~mandt AND
                                                  b~swerk = c~werks AND
                                                  b~beber = c~beber
           INTO TABLE gt_beber
           FOR ALL ENTRIES IN gt_resb
           WHERE a~aufnr = gt_resb-aufnr.

    SELECT a~aufpl
           b~arbpl
           FROM afvc AS a INNER JOIN crhd AS b ON a~mandt = b~mandt AND
                                                  a~arbid = b~objid
           INTO TABLE gt_arbid
           FOR ALL ENTRIES IN gt_resb
           WHERE a~aufpl = gt_resb-aufpl.

  ENDIF.

  LOOP AT gt_resb INTO resb.
    READ TABLE gt_rkpf INTO rkpf WITH KEY rsnum = resb-rsnum
                                 BINARY SEARCH.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    CASE rkpf-kzver.
      WHEN 'F'.
        IF p_mr_mn EQ abap_true.
          CONTINUE.
        ENDIF.
      WHEN OTHERS.
        IF p_mr_pm EQ abap_true.
          CONTINUE.
        ENDIF.
    ENDCASE.

    IF p_text IS INITIAL.
* If Material number is blank, then do not populate Excel
      IF resb-matnr IS INITIAL.
        CONTINUE.
      ENDIF.
    ELSE.
      exceltab-maktx = resb-potx1.
    ENDIF.

    IF resb-lgort EQ space.
      MOVE 'A001' TO exceltab-lgort.
    ELSE.
      exceltab-lgort = resb-lgort.
    ENDIF.

* ACR-2842 D30K927885 BEGIN
    SELECT c~pltxt
           FROM afih AS a INNER JOIN iloa AS b ON a~mandt = b~mandt AND
                                                  a~iloan = b~iloan
                          INNER JOIN iflotx AS c ON b~mandt = c~mandt AND
                                                  b~tplnr = c~tplnr
           INTO exceltab-pltxt
           WHERE a~aufnr = rkpf-aufnr.
    ENDSELECT.
* ACR-2842 D30K927885 END

    temp-bdmng = resb-bdmng - resb-enmng.
    MOVE resb-matnr TO exceltab-matnr.
    MOVE resb-postp TO exceltab-postp.
    MOVE resb-werks TO exceltab-werks.
    MOVE resb-rsnum TO exceltab-rsnum.
    MOVE resb-rspos TO exceltab-rspos.
    MOVE resb-bdmng TO exceltab-bdmng.       "Requirements qty
    MOVE resb-meins TO exceltab-meins.
    MOVE resb-ablad TO exceltab-ablad.
    MOVE resb-bdter TO exceltab-bdter.

    READ TABLE gt_beber INTO gs_beber WITH TABLE KEY aufnr = resb-aufnr.
    IF sy-subrc EQ 0.
      MOVE gs_beber-fing TO exceltab-fing.
    ENDIF.

    READ TABLE gt_arbid INTO gs_arbid WITH TABLE KEY aufpl = resb-aufpl.
    IF sy-subrc EQ 0.
      MOVE gs_arbid-arbpl TO exceltab-arbpl.
    ENDIF.
*-----------------------------------------------------------------------
*  Reservation Document Header for Recipient & Accounting Information
*-----------------------------------------------------------------------

    IF resb-wempf IS NOT INITIAL.
      MOVE resb-wempf TO exceltab-wempf.

    ELSEIF rkpf-wempf IS NOT INITIAL.
      MOVE rkpf-wempf TO exceltab-wempf.
    ELSE.
      IF rkpf-umlgo IS NOT INITIAL.
        READ TABLE lt_twlad INTO lwa_twlad WITH KEY werks = rkpf-umwrk
                                                    lgort = rkpf-umlgo.
        IF sy-subrc = 0.
          READ TABLE lt_adrc INTO lwa_adrc WITH KEY addrnumber = lwa_twlad-adrnr.
          IF sy-subrc = 0.
            MOVE lwa_adrc-name4 TO exceltab-wempf.  "Recipient
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF rkpf-aufnr <> space.

      WRITE rkpf-aufnr TO exceltab-accounting NO-ZERO RIGHT-JUSTIFIED.
    ELSEIF rkpf-kostl <> space.
      WRITE rkpf-kostl TO exceltab-accounting NO-ZERO RIGHT-JUSTIFIED.
    ELSEIF rkpf-ps_psp_pnr <> space.
      WRITE rkpf-ps_psp_pnr TO exceltab-accounting RIGHT-JUSTIFIED.
    ENDIF.

    IF resb-ltxsp IS NOT INITIAL.

      CALL FUNCTION 'CO_ZK_TEXTKEY_RESB'
        EXPORTING
          rsnum = resb-rsnum
          rspos = resb-rspos
          rsart = resb-rsart
        IMPORTING
          ltsch = gv_name.

      REFRESH gt_lines.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = 'MATK'
          language                = resb-ltxsp
          name                    = gv_name
          object                  = 'AUFK'
        TABLES
          lines                   = gt_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      IF sy-subrc IS INITIAL.
        IF gt_lines IS NOT INITIAL.
          CLEAR exceltab-maktx.
        ENDIF.
        LOOP AT gt_lines INTO gs_tline.
          IF exceltab-maktx IS INITIAL.
            exceltab-maktx = gs_tline-tdline.
          ELSE.
            CONCATENATE exceltab-maktx gs_tline-tdline INTO exceltab-maktx
            SEPARATED BY space.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    APPEND exceltab.
    CLEAR exceltab.
    CLEAR qtyhand.
  ENDLOOP.

  IF exceltab[] IS NOT INITIAL.
    SELECT * FROM marc
             INTO TABLE gt_marc
             FOR ALL ENTRIES IN exceltab
             WHERE matnr = exceltab-matnr AND
                   werks = exceltab-werks AND
                   lvorm NE 'X'.

    IF sy-subrc EQ 0.
      SORT gt_marc BY matnr werks.
    ENDIF.

    SELECT * FROM mard
             INTO TABLE gt_mard
             FOR ALL ENTRIES IN exceltab
             WHERE matnr = exceltab-matnr AND
                   werks = exceltab-werks AND
                   lgort = exceltab-lgort AND
                   lvorm NE 'X'.

    IF sy-subrc EQ 0.
      SORT gt_mard BY matnr werks lgort.
    ENDIF.

    SELECT * FROM makt
             INTO TABLE gt_makt
             FOR ALL ENTRIES IN exceltab
             WHERE matnr = exceltab-matnr AND
                   spras = sy-langu.

    IF sy-subrc EQ 0.
      SORT gt_makt BY matnr.
    ENDIF.

    SELECT * FROM ekpo
             INTO TABLE gt_ekpo
             FOR ALL ENTRIES IN exceltab
             WHERE matnr = exceltab-matnr AND
                   werks = exceltab-werks AND
                   ( lgort = exceltab-lgort OR lgort = space )
            AND loekz NE 'L' AND
                   elikz NE 'X'.

    IF sy-subrc EQ 0.
      SORT gt_ekpo BY matnr werks lgort.

      SELECT * FROM ekbe
               INTO TABLE gt_ekbe
               FOR ALL ENTRIES IN gt_ekpo
               WHERE ebeln = gt_ekpo-ebeln AND
                     ebelp = gt_ekpo-ebelp AND
                     bewtp = 'E'.
      IF sy-subrc EQ 0.
        SORT gt_ekbe BY ebeln ebelp.
      ENDIF.
    ENDIF.
  ENDIF.

*-----------------------------------------------------------------------
*  extract rest of information from MARC, MARD, MAKT,
  LOOP AT exceltab.

    READ TABLE gt_marc INTO marc WITH KEY matnr = exceltab-matnr
                                          werks = exceltab-werks
                                          BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE marc-dismm TO exceltab-dismm.
    ENDIF.


    READ TABLE gt_mard INTO mard WITH KEY matnr = exceltab-matnr
                                          werks = exceltab-werks
                                          lgort = exceltab-lgort
                                          BINARY SEARCH.
    IF  sy-subrc = '0'.
      qtyhand = mard-labst + mard-umlme +
                mard-insme + mard-speme + mard-einme.
    ENDIF.

    IF exceltab-matnr IS NOT INITIAL.
      READ TABLE gt_makt INTO makt WITH KEY matnr = exceltab-matnr
                                   BINARY SEARCH.
      IF sy-subrc = '0'.
        MOVE makt-maktx TO exceltab-maktx.            "Description
      ENDIF.
    ENDIF.

* determine quantity on order
    CLEAR qooamt.

    CLEAR ekpo.
    LOOP AT gt_ekpo INTO ekpo WHERE matnr = exceltab-matnr
                                AND werks = exceltab-werks
                              AND ( lgort = exceltab-lgort
                                 OR lgort = space ).
      ADD ekpo-menge TO qooamt.

      LOOP AT gt_ekbe INTO ekbe WHERE ebeln = ekpo-ebeln AND
                                      ebelp = ekpo-ebelp.

        IF ekbe-shkzg = 'H'.
          qooamt =  qooamt + ekbe-menge.
        ELSE.
          qooamt = qooamt - ekbe-menge.
        ENDIF.

      ENDLOOP.
    ENDLOOP.

    MODIFY exceltab.
  ENDLOOP.
*-----------------------------------------------------------------------
  SORT exceltab BY werks  ASCENDING
                 lgort  ASCENDING
                 bdter  ASCENDING
                 matnr  ASCENDING.

  IF p_excl = 'X'.
    PERFORM create_excel_report.
  ELSE.
    PERFORM create_print_report.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  create_print_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_print_report.

  TYPES: BEGIN OF ty_lines,
          line(40) TYPE c,
        END OF ty_lines.

  DATA: lt_lines TYPE TABLE OF ty_lines,
        ls_lines TYPE ty_lines.

  IF NOT exceltab IS INITIAL.
    LOOP AT exceltab.

      WRITE: /  exceltab-werks UNDER text-020,        "Plant
               exceltab-lgort UNDER text-028,        "Storage Loc
               exceltab-matnr+12(6) UNDER text-004,  "Material
               exceltab-postp UNDER text-005,        " Item Category
               exceltab-rsnum UNDER text-007,        "Reserv#
               exceltab-bdter UNDER text-024,        "Req. Date
               exceltab-rspos UNDER text-008,        "Item #
               exceltab-bdmng UNDER text-030,        "Requirement Qty
               exceltab-meins UNDER text-040,        "Unit of M
               exceltab-ablad UNDER text-201,         " Unloading Point
               exceltab-wempf UNDER text-043,        "Recipient
               exceltab-accounting UNDER text-042 RIGHT-JUSTIFIED,   "Accounting
               exceltab-dismm UNDER text-021,       "MRP type
               exceltab-fing UNDER text-006,        "Per. Resp
               exceltab-arbpl UNDER text-010,       "Work Center
               exceltab-pltxt UNDER text-046.    "Func Loc Desc D30K927885

      REFRESH: lt_lines.
      CALL FUNCTION 'RKD_WORD_WRAP'
        EXPORTING
          textline            = exceltab-maktx
*         DELIMITER           = ' '
          outputlen           = 40
        TABLES
          out_lines           = lt_lines
        EXCEPTIONS
          outputlen_too_large = 1
          OTHERS              = 2.

      LOOP AT lt_lines INTO ls_lines.
        IF sy-tabix EQ 1.
          WRITE: ls_lines-line+0(40) UNDER text-029.       "Description
        ELSE.
          WRITE: / ls_lines-line+0(40) UNDER text-029.       "Description
        ENDIF.
      ENDLOOP.

    ENDLOOP.
  ELSE.
    MESSAGE 'No Records found'(003) TYPE 'I'.
  ENDIF.

ENDFORM.                    "create_print_report

*---------------------------  CREATE_OUTPUT_REPORT ---------------------
FORM create_excel_report.
  PERFORM prot_header.
  MOVE text-ttl             TO w_repttl.
  MOVE text-dte             TO w_head01.
  WRITE sy-datum            TO w_head01+6(10).
  WRITE text-amp            TO w_head01+17(1).
  MOVE sy-uzeit             TO w_head01+19(10).
  MOVE text-clt             TO w_head02.
  MOVE sy-mandt             TO w_head02+8(4).
  MOVE sy-sysid             TO w_head02+13(4).
  IF p_rprt = 'X'.
    CLEAR w_option.
    IF sy-batch = 'X'.
      w_option = 'LINESELMOD:1'.
    ENDIF.
  ENDIF.

  IF NOT exceltab IS INITIAL.
    CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
      EXPORTING
        file_name                 = p_file
        create_pivot              = 0
      TABLES
        data_tab                  = exceltab
        fieldnames                = prot_header
      EXCEPTIONS
        file_not_exist            = 1
        filename_expected         = 2
        communication_error       = 3
        ole_object_method_error   = 4
        ole_object_property_block = 5
        invalid_filename          = 6
        invalid_pivot_fields      = 7
        download_problem          = 8
        OTHERS                    = 9.

    IF sy-subrc <> '0'.
      WRITE: /1 'table download unsuccessful - reason = ', sy-subrc.
    ENDIF.
  ELSE.
    MESSAGE 'No Records found'(003) TYPE 'I'.
  ENDIF.

ENDFORM.                    "create_excel_report

*--------------------------- PROT_HEADER -------------------------------
* Each field title in the spreadsheet must be added in the same order as
* the order of the data.
*-----------------------------------------------------------------------
FORM prot_header.
  MOVE text-020 TO prot_header-spaltenname.   "Plant
  APPEND prot_header.

  MOVE text-028 TO prot_header-spaltenname.   "Storage Location
  APPEND prot_header.

  MOVE text-004 TO prot_header-spaltenname.   "Material #
  APPEND prot_header.

  MOVE text-005 TO prot_header-spaltenname.   "Item Category
  APPEND prot_header.

  MOVE text-029 TO prot_header-spaltenname.   "Material Description
  APPEND prot_header.

  MOVE text-021 TO prot_header-spaltenname.   "MRP
  APPEND prot_header.

  MOVE text-024 TO prot_header-spaltenname.   "Requirement Date
  APPEND prot_header.

  MOVE text-007 TO prot_header-spaltenname.   "Reservation #
  APPEND prot_header.

  MOVE text-008 TO prot_header-spaltenname.   "Item
  APPEND prot_header.

  MOVE text-030 TO prot_header-spaltenname.   "Requirement Qty
  APPEND prot_header.

  MOVE text-040 TO prot_header-spaltenname.   "UOM
  APPEND prot_header.

  MOVE text-201 TO prot_header-spaltenname.
  APPEND prot_header.

  MOVE text-042 TO prot_header-spaltenname.   "Account
  APPEND prot_header.

  MOVE text-043 TO prot_header-spaltenname.   "Recipient
  APPEND prot_header.

  MOVE text-006 TO prot_header-spaltenname.   "Plant Section
  APPEND prot_header.

  MOVE text-010 TO prot_header-spaltenname.   "Work Center
  APPEND prot_header.

  MOVE text-046 TO prot_header-spaltenname.   "Func Loc Desc D30K927885
  APPEND prot_header.

ENDFORM.                    "PROT_HEADER

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM check_file_path.
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "abap_bool.

*Separate Path and file
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_file
    IMPORTING
      stripped_name = sep_file
      file_path     = sep_path
    EXCEPTIONS
      x_error       = 0
      OTHERS        = 0.

  IF sep_path CS 'C:' OR sep_path CS 'c:'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH text-098.
  ELSE.
*Check if directory path exist or not.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = sep_path      "lv_dir
      RECEIVING
        result               = lv_bol
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF lv_bol IS INITIAL.
      CONCATENATE text-099 sep_path sep_file INTO sep_path.
      MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH sep_path.
    ENDIF.
  ENDIF.

ENDFORM.                    "CHECK_FILE_PATH
