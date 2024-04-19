class ZCL_IM_IW01_PREQ_BADI definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_IW01_PREQ_BADI
*"* do not include other source files here!!!

  interfaces IF_EX_IWO1_PREQ_BADI .

  data GV_BNFPO_1 type EBAN-BNFPO value 00010. "#EC NOTEXT .
  data GV_BNFPO_2 type EBAN-BNFPO .
protected section.
*"* protected components of class ZCL_IM_IW01_PREQ_BADI
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_IW01_PREQ_BADI
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_IW01_PREQ_BADI IMPLEMENTATION.


METHOD if_ex_iwo1_preq_badi~decision_coll_preq.
  "caufvd_imp
  "afvgd_imp
  "resbd_imp
  "eban_imp
  "ebkn_imp
  "c_tmp_banfn

  DATA: prtype TYPE eban-banfn.
*BOI by PANUSURI Ticket 6934
  DATA: lv_banfn(3) TYPE p.
  DATA: prpstyp     TYPE eban-pstyp.
  DATA: lwa_eban    TYPE eban.
*EOI by PANUSURI Ticket 6934

*  IMPORT prtype = prtype FROM MEMORY ID 'ZCL_IM_IW01_PREQ_BADI_MEMORY'.      "(-)PANUSURI Ticket 6934
*BOI by PANUSURI Ticket 6934
* Import previous requisition details from memory, exported in enhancement ZCO_ZF_BANF_PROC
  IMPORT lwa_eban = lwa_eban FROM MEMORY ID 'ZCL_IM_IW01_PREQ_BADI_MEMORY1'.
  prtype  = lwa_eban-banfn.
  prpstyp = lwa_eban-pstyp.
*EOI by PANUSURI Ticket 6934

  "Blank is first
  "If it is not # then someone changed another line and
  "it saved an old PR as the current pr, we wnat a new one, so
  "looking for place holder
*  IF prtype = '' or prtype(1) <> '#'.  "(-)PANUSURI Ticket 6934
  CLEAR c_tmp_banfn.
*    if afvgd_imp is not initial.
*      "Service
*      prtype = 'S'.
*    else.
*      "Material
*      prtype = 'C'.
*    endif.
*  ELSEif prtype = 'S'.
*    c_tmp_banfn = '#       1'.
*  else.
*    c_tmp_banfn = '#       2'.
*  ELSE.                    "(-)PANUSURI Ticket 6934
*   c_tmp_Banfn = prtype.   "(-)PANUSURI Ticket 6934
*  ENDIF.                   "(-)PANUSURI Ticket 6934

*BOI by PANUSURI Ticket 6934
* Generate collective PR for all services
*  IF prtype CA '1'.  "(-)PANUSURI Ticket 6934
  IF prtype(1) = '#' AND prtype CA '1'. "(+)PANUSURI Ticket 6934
    IF prpstyp = eban_imp-pstyp.
      c_tmp_banfn = prtype .
      gv_bnfpo_1 = gv_bnfpo_1 + '00010'.
    ELSE.
      CLEAR lv_banfn.
      prtype+1 = lv_banfn + 2.
      c_tmp_banfn = prtype .
      gv_bnfpo_2 = gv_bnfpo_2 + '00010'.
    ENDIF.
* Generate collective PR for all materials
*  ELSEIF prtype CA '2'.  "(-)PANUSURI Ticket 6934
  ELSEIF prtype(1) = '#' AND prtype CA '2'. "(+)PANUSURI Ticket 6934
    IF prpstyp = eban_imp-pstyp.
      c_tmp_banfn = prtype .
      gv_bnfpo_2 = gv_bnfpo_2 + '00010'.
    ENDIF.
  ENDIF.

* Export PR item number to memory to be used in enhancement 'ZPM_SPLIT_LINE_ITEM_NUMBER'
* for splitting PR line item numbers
  EXPORT gv_bnfpo_1 = gv_bnfpo_1 gv_bnfpo_2 = gv_bnfpo_2 TO MEMORY ID 'ZCL_IM_IW01_PREQ_BADI_MEMORY2'.

  FREE MEMORY ID 'ZCL_IM_IW01_PREQ_BADI_MEMORY1'.
*EOI by PANUSURI Ticket 6934

*  EXPORT prtype = prtype to MEMORY id 'ZCL_IM_IW01_PREQ_BADI_MEMORY'.
ENDMETHOD.


method IF_EX_IWO1_PREQ_BADI~DETERMINE_DATE_FOR_SERVICE.
endmethod.
ENDCLASS.
