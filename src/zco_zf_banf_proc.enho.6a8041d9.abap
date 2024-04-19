"Name: \FU:CO_ZF_BANF_PROC\SE:END\EI
ENHANCEMENT 0 ZCO_ZF_BANF_PROC.
*
*"  IMPORTING
*"     VALUE(EBAN_IMP) LIKE  EBAN STRUCTURE  EBAN
*"     VALUE(EBKN_IMP) LIKE  EBKN STRUCTURE  EBKN
*"     VALUE(AFVGD_IMP) LIKE  AFVGD STRUCTURE  AFVGD OPTIONAL
*"     VALUE(RESBD_IMP) LIKE  RESBD STRUCTURE  RESBD OPTIONAL
*"  EXPORTING
*"     VALUE(EBAN_EXP) LIKE  EBAN STRUCTURE  EBAN
*"     VALUE(EBKN_EXP) LIKE  EBKN STRUCTURE  EBKN

"eban_exp-eprefdoc

"Copy the eprefdoc and eprefitm over to eban even though this is external
  "break btboundy.
  "eban_exp-konnr = eban_imp-eprefdoc.
  "eban_exp-ktpnr = eban_imp-eprefitm.
  IF eban_exp-konnr IS INITIAL.
    eban_exp-konnr = eban_imp-konnr.
  ENDIF.
  IF eban_exp-ktpnr IS INITIAL.
    eban_exp-ktpnr = eban_imp-ktpnr.
  ENDIF.

"Uppercase Requisitioner
  "TRANSLATE eban_exp-afnam TO UPPER CASE.

"Save the PR number for next WO
*  EXPORT prtype = eban_exp-banfn to MEMORY id 'ZCL_IM_IW01_PREQ_BADI_MEMORY'. "(-)PANUSURI Ticket 6934
  EXPORT lwa_eban = eban_exp to MEMORY id 'ZCL_IM_IW01_PREQ_BADI_MEMORY1'.     "(+)PANUSURI Ticket 6934

ENDENHANCEMENT.
