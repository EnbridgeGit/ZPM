*&---------------------------------------------------------------------*
*&  Include           ZXCOZU01
*&---------------------------------------------------------------------*

IF afvgd_imp-slwid = 'P2P_PM'.
  MOVE afvgd_imp-usr00 TO eban_ord_cust_chg-konnr.
  MOVE afvgd_imp-usr02 TO eban_ord_cust_chg-ktpnr.
  MOVE afvgd_imp-usr01 TO eban_ord_cust_chg-zzariba_approver.
  TRANSLATE eban_ord_cust_chg-zzariba_approver TO UPPER CASE.
  MOVE afvgd_imp-usr03 TO eban_ord_cust_chg-zzorigreq.
  MOVE afvgd_imp-usr06 TO eban_ord_cust_chg-preis.
ENDIF.

"Move the operation dates.
"START
*IF afvgd_imp-ntanf IS NOT INITIAL.
*  eban_ord_cust_chg-frgdt = afvgd_imp-ntanf.
*ELSE.
*  "Put system date
*  eban_ord_cust_chg-frgdt = sy-datum.
*ENDIF.
*
*"END
*IF afvgd_imp-ntend IS NOT INITIAL.
*  eban_ord_cust_chg-lfdat = afvgd_imp-ntend.
*ELSE.
*  "Make end date equal to start date
*  eban_ord_cust_chg-lfdat = eban_ord_cust_chg-frgdt.
*ENDIF.

*The new dates fields to use are Earliest scheduled start
*(AFVGD-FSAVD) and Earliest scheduled finish (AFVGD-FSEDD)
*which are replacing Constraint for activity start (AFVGD-NTANF)
*and Constraint for Finish of Activity (AFVGD-NTEND)
"Move the operation dates.
"START
IF afvgd_imp-fsavd IS NOT INITIAL.
  eban_ord_cust_chg-frgdt = afvgd_imp-fsavd.
ELSE.
  "Put system date
  eban_ord_cust_chg-frgdt = sy-datum.
ENDIF.

"END
IF afvgd_imp-fsedd IS NOT INITIAL.
  eban_ord_cust_chg-lfdat = afvgd_imp-fsedd.
ELSE.
  "Make end date equal to start date
  eban_ord_cust_chg-lfdat = eban_ord_cust_chg-frgdt.
ENDIF.
