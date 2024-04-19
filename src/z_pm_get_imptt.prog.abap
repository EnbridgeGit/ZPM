*&---------------------------------------------------------------------*
*& Report  Z_PM_GET_IMPTT
*&
*&---------------------------------------------------------------------*
*& This is an auto generated program copied from BODS
*&---------------------------------------------------------------------*

REPORT  z_pm_get_imptt MESSAGE-ID 26 LINE-SIZE 255
NO STANDARD PAGE HEADING.
* Z_PM_GET_IMPTT.
PARAMETER download(1) DEFAULT 'S' LOWER CASE. "N-svr,Y-clnt
PARAMETER execmode(1) DEFAULT 'B' LOWER CASE. "D-dlg,B-btch
PARAMETER out_dir(48) DEFAULT "output file dir
'\\chobissb\' LOWER CASE.
PARAMETER in_dir(48) DEFAULT "input file dir
'\\chobissb\' LOWER CASE.
PARAMETER p_dest TYPE rfcdes-rfcdest DEFAULT
'NONE'.
PARAMETER p_progid TYPE rfcopt-rfcexec DEFAULT
space.
PARAMETER p_gwhost TYPE rfcopt-rfcgwhost DEFAULT
space.
PARAMETER p_gwserv TYPE rfcopt-rfcgwserv DEFAULT
space.
PARAMETER p_srvfm(30) DEFAULT "Server Callback function
space.
PARAMETER p_pkgsz TYPE i DEFAULT "Stream package size
5000.
PARAMETER p_snc_on(1) DEFAULT "X-on SPACE-off
space.

PARAMETER p_df_vk(40) DEFAULT "ABAP data flow object key
'' LOWER CASE.
PARAMETER p_di_gen(40) DEFAULT "DI version that generated ABAP
'' LOWER CASE.

*** Machine generated ABAP. Do not modify.            ***
*** (C)Copyright Business Objects S.A.  All rights reserved. ***
*
* Date Time:
*    11/07/14 21:36:13
* SAP used for generated this ABAP:
*    Release: 702
*    Host   : chobissb
*
* ABAP Dataflow Name:
*    Z_PM_GET_IMPTT
* ABAP program name in SAP:
*  Z_PM_GET_IMPTT
* Generated ABAP file name:
*  C:/ProgramData/SAP BusinessObjects/Data Services/workspace/Z_PM_G
*    ET_IMPTT
TABLES imptt.

DATA: BEGIN OF itab2 OCCURS 0,
mandt(3) TYPE c,
point(12) TYPE c,
mpobj(22) TYPE c,
dstxt(40) TYPE c.
DATA: END OF itab2.

DATA: append_flag(1) VALUE ' ',
      cntbuf TYPE i,
      delimleng TYPE i,last_batch(1) VALUE ' '.

CONSTANTS c_df_vk(40) VALUE '347'.
CONSTANTS c_di_gen(40) VALUE '14.2.1.224'.
DATA warn_msg(50).


START-OF-SELECTION.


  IF download = 'S' OR
     download = 'N' OR
     download = 'Y'.
    .
  ELSE.
    DATA: m_xfer_err_msg(700).
    CONCATENATE
'ABAP program does not recognize this new '
'data transfer method: ' download
'. Regenerate the ABAP program and upload to this system.'
    INTO m_xfer_err_msg.
    MESSAGE  e240(s#) WITH m_xfer_err_msg.
  ENDIF.

  IF execmode = 'B' OR
     execmode = 'D'.
    .
  ELSE.
    DATA: m_exec_err_msg(700).
    CONCATENATE
'ABAP program does not recognize this new '
'execution option: ' execmode
'. Regenerate the ABAP program and upload to this system.'
    INTO m_exec_err_msg.
    MESSAGE  e240(s#) WITH m_exec_err_msg.
  ENDIF.

  IF download = 'S'.
    PERFORM connect_rfcdest_to_progid.
  ENDIF.



  PERFORM form2.
  last_batch = 'X'.
  PERFORM form3.
  FREE itab2.
  IF download = 'S'.
    PERFORM disconnect_rfcdest_from_progid.
  ENDIF.


END-OF-SELECTION.

  CLEAR warn_msg.

  IF NOT p_df_vk IS INITIAL.
    IF p_df_vk <> c_df_vk.
      CONCATENATE '$$Warning$'
                  c_df_vk
                  '$' INTO warn_msg.
    ENDIF.
  ENDIF.
  IF NOT p_di_gen IS INITIAL.
    IF p_di_gen <> c_di_gen.
      IF warn_msg IS INITIAL.
        CONCATENATE '$$Warning$$'
                    c_di_gen
                    INTO warn_msg.
      ELSE.
        CONCATENATE warn_msg
                    c_di_gen
                    INTO warn_msg.
      ENDIF.
    ENDIF.
  ENDIF.

  IF NOT warn_msg IS INITIAL.
    IF execmode = 'D'.
      WRITE warn_msg.
      NEW-LINE.
    ELSE.
      MESSAGE s240(s#) WITH warn_msg.
    ENDIF.
  ENDIF.


  WRITE  '* Program Complete *'.
  WRITE  '(C)Copyright Business Objects S.A.  All rights reserved.'.

*&---------------------------------------------------------------------*
*&      Form  FORM2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM form2.
  DATA altmp8(3) TYPE c.
  DATA altmp9(12) TYPE c.
  DATA altmp10(22) TYPE c.
  DATA altmp11(40) TYPE c.




  SELECT
    mandt
    point
    mpobj
    dstxt
  INTO CORRESPONDING FIELDS OF imptt
  FROM imptt
  .
    altmp8 = imptt-mandt.
    altmp9 = imptt-point.
    altmp10 = imptt-mpobj.
    altmp11 = imptt-dstxt.
    MOVE altmp8 TO itab2-mandt.
    MOVE altmp9 TO itab2-point.
    MOVE altmp10 TO itab2-mpobj.
    MOVE altmp11 TO itab2-dstxt.
    APPEND itab2.
    cntbuf = cntbuf + 1.
    IF download = 'N'.
      IF cntbuf > 5000.
        PERFORM form3.
        CLEAR cntbuf.
        REFRESH itab2.
        append_flag = 'A'.
      ENDIF.
    ENDIF.
    IF download = 'S'.
      IF cntbuf > p_pkgsz.
        PERFORM form3.
        CLEAR cntbuf.
        REFRESH itab2.
      ENDIF.
    ENDIF.
  ENDSELECT.
ENDFORM.                    "FORM2

*&---------------------------------------------------------------------*
*&      Form  FORM3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM form3.
  DATA: outfile(512), ldfile(50).
  ldfile = 'ZPM_GET_IMPT'.
  CONCATENATE out_dir ldfile INTO outfile
    SEPARATED BY '/'.
  IF download = 'S'.
    DATA: error_message(700),mtext(800),ieop(1).
    CALL FUNCTION p_srvfm
      DESTINATION p_dest
      KEEPING LOGICAL UNIT OF WORK
      EXPORTING
        eos                   = last_batch
      IMPORTING
        eop                   = ieop
      TABLES
        e_table               = itab2
      EXCEPTIONS
        read_error            = 1
        system_failure        = 2  MESSAGE error_message
        communication_failure = 3  MESSAGE error_message
        OTHERS                = 4.

    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1.
          CONCATENATE
          'Data Services read error. '
          'Check Data Services error log.'
          INTO mtext.
          MESSAGE  e240(s#) WITH mtext.
        WHEN 2.
          CONCATENATE
  'SAP System Failure while calling DS remote function: '
        error_message INTO mtext.
          MESSAGE  e240(s#) WITH mtext.
        WHEN 3.
          CONCATENATE
  'SAP System Failure while calling DS remote function: '
        error_message INTO mtext.
          MESSAGE  e240(s#) WITH mtext.
        WHEN 4.
          MESSAGE  e240(s#) WITH
  'Other SAP System Failure while calling DS remote function.'.
      ENDCASE.
    ENDIF.
    IF ieop = 'X'.
      PERFORM disconnect_rfcdest_from_progid.
      LEAVE PROGRAM.
    ENDIF.
  ELSE.
    DATA  dlmtlen TYPE i VALUE '1'.
    DATA xdlmtlen TYPE i VALUE '1'.
    DATA:
      ht(1) TYPE c,
      xht(1) TYPE x,
      conv TYPE REF TO cl_abap_conv_in_ce.
    xht = '7F'.
    conv = cl_abap_conv_in_ce=>create(
      encoding = '1100'
      input = xht
    ).
    CALL METHOD conv->read(
      EXPORTING
        n    = xdlmtlen
      IMPORTING
        data = ht
        len  = dlmtlen
               ).
    DATA return_code TYPE i.
    PERFORM write_delimited_file
              TABLES   itab2
              USING    outfile
                       append_flag
                       ht
                       dlmtlen
                       download
              CHANGING return_code.

    CASE return_code.
      WHEN 1.
        IF execmode = 'D'.
          WRITE: /5 'No line selected'.
        ELSE.
          MESSAGE e047(s#).
        ENDIF.
      WHEN 2.
        IF execmode = 'D'.
          WRITE: /5 'Open File Error -- ', 25 outfile.
        ELSE.
          MESSAGE  e084(e0) WITH outfile.
        ENDIF.
      WHEN 3.
        IF execmode = 'D'.
          WRITE: /5 'Data exceed length limit (8192) '.
        ELSE.
          MESSAGE  e240(s#) WITH
               'Data exceed length limit (8192) '.
        ENDIF.
      WHEN 4.
        IF execmode = 'D'.
          WRITE: /5 'Call function WS_DOWNLOAD error'.
        ELSE.
          MESSAGE  e240(s#) WITH
               'Call function WS_DOWNLOAD error'.
        ENDIF.
    ENDCASE.
  ENDIF.
ENDFORM.                    "FORM3

*&---------------------------------------------------------------------*
*&      Form  SUBSTRING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->SRC        text
*      -->BEG        text
*      -->LEN        text
*      -->RET        text
*----------------------------------------------------------------------*
FORM substring USING src beg len CHANGING ret.

  DATA: va1 TYPE i.
  DATA: va2 TYPE i.
  DATA: va3 TYPE i.

  va3 = strlen( src ).

  IF  beg = 0.
    va1 = 0.
  ELSE.
    IF  beg < 0.
      va1 = va3 + beg.
      IF  va1 < 0.
        va1 = 0.
      ENDIF.
    ELSE.
      va1 = beg - 1.
    ENDIF.
  ENDIF.

  IF  len < 0.
    va2 = 0.
  ELSE.
    va2 = va3 - va1.
  ENDIF.

  IF  va2 > len.
    va2 = len.
  ENDIF.

  IF  va2 < 1.
    MOVE ''           TO ret.
  ELSE.
    MOVE src+va1(va2) TO ret.
  ENDIF.

ENDFORM.                    "SUBSTRING

*&---------------------------------------------------------------------*
*&      Form  write_delimited_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->DATATAB    text
*      -->FILE       text
*      -->APPEND     text
*      -->DELIMIT    text
*      -->DLENGTH    text
*      -->DWNLOAD    text
*      -->RC         text
*----------------------------------------------------------------------*
FORM write_delimited_file
           TABLES   datatab
           USING    file
                    append
                    delimit
                    dlength
                    dwnload
          CHANGING rc.

  DATA: type1,
        appd(1),
        temp(32),
        time1(8),
        date1(10),
        output(8192),
        rcount TYPE i,
        offset TYPE i,
        tablen TYPE i,
        maxlen TYPE i VALUE '8192'.

  DATA: BEGIN OF clientab OCCURS 0,
             output(8192),
          END OF clientab.

  FIELD-SYMBOLS: <f>.
  FIELD-SYMBOLS <delim1>.
  DATA delim2(16).
  DATA l_filename TYPE string.

  appd = append.
  IF appd IS NOT INITIAL.
    appd = 'X'.
  ENDIF.
  MOVE file TO l_filename.
  DESCRIBE TABLE datatab LINES tablen.


  IF dwnload = 'Y'.
    CLEAR clientab. REFRESH clientab.
    rcount = 0.
  ELSE.
    IF appd = space.
      OPEN DATASET file FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
    ELSE.
      OPEN DATASET file FOR APPENDING IN TEXT MODE ENCODING UTF-8.
    ENDIF.
    IF sy-subrc <> 0.
      rc = 2. EXIT.
    ENDIF.
  ENDIF.

  LOOP AT datatab.
    CLEAR: tablen, offset, output.
    DO.
      ASSIGN COMPONENT sy-index OF
         STRUCTURE datatab TO <f>.
      IF sy-subrc <> 0. EXIT. ENDIF.
      IF sy-index > 1.
        ASSIGN delimit(dlength) TO <delim1> CASTING TYPE c.
        delim2 = <delim1>.
        WRITE delim2(dlength) TO output+offset(dlength).
        ADD dlength TO offset.
      ENDIF.

      DESCRIBE FIELD <f> TYPE type1.

      IF type1 = 'I' OR type1 = 'N'.
        type1 = 'P'.
      ENDIF.

      CASE type1.
        WHEN 'D'.
          IF <f> = '00000000'.
            <f> = ' '.
          ELSE.
            MOVE <f> TO time1.
            ASSIGN time1 TO <f>.
          ENDIF.
        WHEN 'F'.
          IF <f> = '0.0'.
            temp = '0.0'.
          ELSE.
            WRITE <f> TO temp EXPONENT 0.
          ENDIF.
          CONDENSE temp NO-GAPS.
          TRANSLATE temp USING ',.'.
          ASSIGN temp TO <f>.
        WHEN 'P'.
          IF <f> < 0.
            WRITE '-' TO output+offset(1).
            ADD 1 TO offset.
            <f> = <f> * ( -1 ).
          ENDIF.
          MOVE <f> TO temp.
          CONDENSE temp NO-GAPS.
          TRANSLATE temp USING ',.'.
          ASSIGN temp TO <f>.
      ENDCASE.

      sy-fdpos = strlen( <f> ).

      tablen = offset + sy-fdpos.
      IF tablen > maxlen.
        rc = 3. EXIT.
      ENDIF.
      WRITE <f> TO output+offset(sy-fdpos).
      ADD sy-fdpos TO offset.
    ENDDO.

    IF dwnload = 'Y'.
      clientab-output = output.
      APPEND clientab.
      rcount = rcount + 1.
      IF rcount >= 50.
        sy-batch = space.
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename = l_filename
            filetype = 'ASC'
            codepage = '4110'
            append   = appd
            write_field_separator = 'X'
*            IMPORTING
*              FILELENGTH =
          TABLES
            data_tab = clientab
          EXCEPTIONS
            OTHERS = 1.
        IF sy-subrc <> 0.
          rc = 4.
        ENDIF.
        CLEAR clientab. REFRESH clientab.
        rcount = 0. appd = 'A'.
      ENDIF.
    ELSE.
      TRANSFER output TO file.
    ENDIF.
  ENDLOOP.

  IF dwnload = 'Y'.
    sy-batch = space.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename = l_filename
        filetype = 'ASC'
           codepage = '4110'
        append   = appd
        write_field_separator = 'X'
*         IMPORTING
*           FILELENGTH =
      TABLES
        data_tab = clientab
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      rc = 4.
    ENDIF.
  ELSE.
    CLOSE DATASET file.
  ENDIF.
ENDFORM.                    "write_delimited_file

*&---------------------------------------------------------------------*
*&      Form  CONNECT_RFCDEST_TO_PROGID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM connect_rfcdest_to_progid.
  INCLUDE rfctypes.

  DATA: len     TYPE i,
        r3name(4),
        systnr(2),
        uid     LIKE sys_uid,
        options LIKE rfcopt,
        isunicode  TYPE n.

  DATA: ntotal     LIKE gwy_struct-noreg,
        gwy_gwhost LIKE gwy_struct-gwhost,
        gwy_gwserv LIKE gwy_struct-gwserv,
        gwy_tpname LIKE gwy_system-tpname.

  TABLES: rfcsi.

* Check program ID
  IF p_progid = space.
    RAISE invalid_program_id.
  ENDIF.

* determine if the RFC destination authority
  CALL FUNCTION 'RFC_READ_TCPIP_DESTINATION'
    EXPORTING
      destination     = p_dest
      authority_check = 'X'
    IMPORTING
      rfcunicode      = isunicode.

* Use current gateway if no info exits
  IF p_gwhost = space OR p_gwserv = space.
    CALL FUNCTION 'RFC_SYSTEM_INFO'
      IMPORTING
        rfcsi_export = rfcsi.

    len = strlen( rfcsi-rfcdest ) - 2.
    systnr = rfcsi-rfcdest+len.
    len = len - 1 - 3.
    r3name = rfcsi-rfcdest+len(3).
    len = len - 1.
    options-rfcgwhost = rfcsi-rfcdest(len).
    CONCATENATE 'sapgw' systnr INTO options-rfcgwserv.
  ELSE.
    options-rfcgwhost = p_gwhost.
    options-rfcgwserv = p_gwserv.
  ENDIF.

* Parameters for GWY function call
  gwy_gwhost = options-rfcgwhost.
  gwy_gwserv = options-rfcgwserv.
  gwy_tpname = p_progid.

* Check gateway and server program registered
  CALL FUNCTION 'GWY_GET_NO_REG_PROGRAMS'
    EXPORTING
      gwhost      = gwy_gwhost
      gwserv      = gwy_gwserv
      tpname      = gwy_tpname
    IMPORTING
      noreg_total = ntotal
    EXCEPTIONS
      OTHERS      = 1.

  IF sy-subrc NE 0.
    RAISE connect_to_gateway_failed.
  ENDIF.

  IF ntotal = 0.
    RAISE server_not_registered.
  ENDIF.

  IF ntotal GT 1.
    RAISE duplicate_reg_programs.
  ENDIF.

* build new connection to a registered server
  options-rfcexec   = gwy_tpname.
  options-rfcgwhost = gwy_gwhost.
  options-rfcgwserv = gwy_gwserv.
  options-rfchost   = '%%RFCSERVER%%'.
  IF p_snc_on = 'X'.
    options-rfcsnc  = 'X'.
  ENDIF.

  CALL 'RFCControl' ID 'CODE' FIELD 'O'
                    ID 'DESTINATION' FIELD p_dest
                    ID 'TYPE' FIELD rfctype_external_tcp
                    ID 'OPTIONS' FIELD options.

  IF sy-subrc NE 0.
    RAISE connect_to_reg_server_failed.
  ENDIF.

* and set exclusive mode to keep server owned
  CALL FUNCTION 'SYSTEM_SET_REG_SERVER_PROPERTY'
    EXPORTING
      destination                  = p_dest
      exclusiv                     = 'Y'
    EXCEPTIONS
      connect_to_reg_server_failed = 1
      exclusiv_not_supported       = 2.

  IF sy-subrc NE 0.
    CASE sy-subrc.
      WHEN 1.
        RAISE connect_to_reg_server_failed.
      WHEN 2.
        RAISE exclusiv_not_supported.
    ENDCASE.
  ENDIF.

ENDFORM.                    "CONNECT_RFCDEST_TO_PROGID

*&---------------------------------------------------------------------*
*&      Form  DISCONNECT_RFCDEST_FROM_PROGID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM disconnect_rfcdest_from_progid.
* set exclusive mode to E to end the session
  CALL FUNCTION 'SYSTEM_SET_REG_SERVER_PROPERTY'
    EXPORTING
      destination                  = p_dest
      exclusiv                     = 'E'
    EXCEPTIONS
      connect_to_reg_server_failed = 1
      exclusiv_not_supported       = 2.
ENDFORM.                    "DISCONNECT_RFCDEST_FROM_PROGID
