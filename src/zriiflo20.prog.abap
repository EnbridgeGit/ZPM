REPORT riiflo20 MESSAGE-ID ih
                NO STANDARD PAGE HEADING.
ENHANCEMENT-POINT RIIFLO20_G4 SPOTS ZES_RIIFLO20 STATIC .
ENHANCEMENT-POINT RIIFLO20_G5 SPOTS ZES_RIIFLO20 .
ENHANCEMENT-POINT RIIFLO20_G6 SPOTS ZES_RIIFLO20 STATIC .
ENHANCEMENT-POINT RIIFLO20_G7 SPOTS ZES_RIIFLO20 .

*####################################################################*
* Datenteil                                                          *
*####################################################################*
TYPE-POOLS ilox.
*-------------------------------------------------------------------*
* Datenbanktabellen                                                 *
*-------------------------------------------------------------------*
TABLES: iflo,
        iflos,
        ihpa,
        ihsg,
        rilo0,
        crhd,
        prps.
*-------------------------------------------------------------------*
* ATAB-Tabellen                                                     *
*-------------------------------------------------------------------*
TABLES:
    t370a.

*-------------------------------------------------------------------*
* Interne Tabellen                                                  *
*-------------------------------------------------------------------*
DATA: BEGIN OF object_tab OCCURS 0.
        INCLUDE STRUCTURE rihiflo_list.
ENHANCEMENT-POINT EHP605_RIIFLO20_01 SPOTS ZES_RIIFLO20 STATIC .


* begin of note 1656798
* EAM CC 200
include STRUCTURE rihiflo_list_characteristic.

*DATA:   clda1 LIKE dicldat-ausp1.
*DATA:   clda2 LIKE dicldat-ausp1.
*DATA:   clda3 LIKE dicldat-ausp1.
*DATA:   clda4 LIKE dicldat-ausp1.
*DATA:   clda5 LIKE dicldat-ausp1.
*DATA:   clda6 LIKE dicldat-ausp1.
*DATA:   clda7 LIKE dicldat-ausp1.
*DATA:   clda8 LIKE dicldat-ausp1.
*DATA:   clda9 LIKE dicldat-ausp1.
*DATA:   clda10 LIKE dicldat-ausp1.
*DATA:   clda11 LIKE dicldat-ausp1.
*DATA:   clda12 LIKE dicldat-ausp1.
*DATA:   clda13 LIKE dicldat-ausp1.
*DATA:   clda14 LIKE dicldat-ausp1.
*DATA:   clda15 LIKE dicldat-ausp1.
*DATA:   clda16 LIKE dicldat-ausp1.
*DATA:   clda17 LIKE dicldat-ausp1.
*DATA:   clda18 LIKE dicldat-ausp1.
*DATA:   clda19 LIKE dicldat-ausp1.
*DATA:   clda20 LIKE dicldat-ausp1.
* end of note 1656798

DATA:   ppsid LIKE iflo-ppsid.
DATA:   igewrk LIKE iflo-lgwid.
DATA:   tplma_int LIKE iflo-tplma.
DATA:   selected,
        pm_selected TYPE pm_selected,

***********************************************
***  Customer Connect EAM 2014, IR 7135, note 2079318
***  count lines in ALV list reports
        row_count TYPE sy-tabix,
***********************************************
      END OF object_tab.

* EAM CC 200                                                "1656798
include eamcc_riiflo20_1 if found.                          "1656798

DATA sel_tab         LIKE rihiflo_list OCCURS 50 WITH HEADER LINE.
DATA iflo_u          LIKE iflo         OCCURS 50 WITH HEADER LINE.
DATA l_clobj         LIKE clobj        OCCURS 50 WITH HEADER LINE.
DATA l_jsto_pre_tab  LIKE jsto_pre     OCCURS 50 WITH HEADER LINE.
DATA l_tarbid        LIKE crid         OCCURS 10 WITH HEADER LINE.

DATA g_adrnr_sel_tab LIKE addr1_sel OCCURS 50 WITH HEADER LINE.
DATA g_adrnr_val_tab LIKE addr1_val OCCURS 50 WITH HEADER LINE.
*--- itab für Vorselektion über Genehmigungen -----------------------
DATA: BEGIN OF g_sogen_object OCCURS 10,
        objnr LIKE equi-objnr,
      END OF g_sogen_object.

*--- int. Tab. für Platzkennzeichnung
DATA: g_tplnr_tab2 TYPE ilox_t_tplnr.
DATA: g_iflos_tab  TYPE ilox_t_iflos.

*--- Selektion über Adresse -
DATA g_selopt_tab LIKE ddshselopt OCCURS 0 WITH HEADER LINE.
DATA g_shlp       LIKE ddshdescr.

*ranges: clobj for l_clobj-obj.
RANGES: object FOR object_tab-tplnr.
RANGES: r_submt FOR object_tab-submt.
RANGES: i_gewrk FOR iflo-lgwid.
RANGES: i_ppsid FOR iflo-ppsid.

*--- ranges for WBS elements with internal numbers
DATA: gr_proid_i_all TYPE RANGE OF ps_psp_pnr.

*-------------------------------------------------------------------*
* Feldleisten                                                       *
*-------------------------------------------------------------------*
TABLES: rihiflo.
TABLES: rihiflo_list.
*-------------------------------------------------------------------*
* Flags Sonderverarbeitungen                                        *
*-------------------------------------------------------------------*
DATA: g_adres_flag.
DATA: g_sttxt_flag.
DATA: g_arbpl_flag.
DATA: g_gewrk_flag.
DATA: g_stasl_flag.
DATA: g_submt_flag.
DATA: g_konvr_flag.
DATA: g_mganr_flag.
DATA: g_crhd_flag.
DATA: g_variant_flag.
DATA: g_answer.
DATA: g_errorcode LIKE sy-subrc.
DATA: g_ucomm LIKE sy-ucomm.

* Datenvereinbarungen für die Klassensuche
DATA:
  g_badi_list_editing_ref TYPE REF TO if_ex_ihsa_list_editing,
  g_badi_list_editing_act TYPE char01,
  g_clsd_comw             TYPE TABLE OF comw,
* Flag für die Suchstrategie, Sucheinstieg über Klasse, wenn 'X'
  g_clsd_strategy         TYPE char1 VALUE space,
  g_clsd_select_options   TYPE rsparams_tt,
  g_tplnr_cl              TYPE TABLE OF tplnr,
  g_tplnr_md              TYPE TABLE OF tplnr,
  g_class_old             LIKE clselinput-class,
  g_klart_old             LIKE clselinput-klart,
  g_comw_flag             TYPE flag.

DATA:
  gc_max_md_count TYPE i VALUE 10000,
  gc_max_hits TYPE i VALUE 2000,
  gc_obtab_equi TYPE clobjecttype VALUE 'EQUI',
  gc_obtab_iflo TYPE clobjecttype VALUE 'IFLOT'.

TABLES:
  sscrfields.

*eject
*-------------------------------------------------------------------*
* INCLUDES                                                          *
*-------------------------------------------------------------------*
INCLUDE miolxtop.
*--- EHP605e: Linear Asset Management: global data for selection screen
INCLUDE eaml_sel_global_data.

*--- itab für Nachselektion Materialtext
DATA: g_matnr_tab TYPE irep1_matnr_wa OCCURS 100 WITH HEADER LINE.

*eject
*####################################################################*
* Selektionsbild                                                     *
*####################################################################*
INCLUDE ZPMIH06_SEL."miollsel.

*---------------------------------------------------------------------
* Initialization
*---------------------------------------------------------------------
INITIALIZATION.

*--- Aktivitätstyp bestimmen -----------------------------------------
  PERFORM determine_acttype_iflo_l.
*--- SAPPHONE aktiv -------------------------------------------------
  PERFORM check_sapphone_aktive_f14.

ENHANCEMENT-POINT EHP_RIIFLO20_01 SPOTS ZES_RIIFLO20 .

  g_key = 'TPLNR'.
  g_text = 'PLTXT'.

  PERFORM variant_start_f16.
*--- Übergabestruktur für Adresselektion setzten --------------------
  g_shlp-shlpname = 'IFLMR'.           "Name der Suchhilfe
  g_shlp-selmethod = 'IFLM_ADDR'.      "Name Selektionsview

  PERFORM class_search_init_f78 USING gc_obtab_iflo.

*--- BAdI BADI_EAM_SINGLELEVEL_LIST instantiation
  GET BADI gb_badi_eam_singlelevel_list
    FILTERS
      report_name = 'RIIFLO20'.

*---------------------------------------------------------------------
*--- F4 Eingabehilfe für Listvariante
*---------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR variant.
  PERFORM variant_inputhelp_f14 USING variant.
*---------------------------------------------------------------------
*--- F4 help for app server path
*---------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  PERFORM get_path.

*eject
*---------------------------------------------------------------------
* AT SELECTION-SCREEN OUTPUT
*---------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

  STATICS: l_slset LIKE sy-slset.

  PERFORM init_selection_screen_f16.
*--- Listvariante initialisieren -------------------------------------
  PERFORM variant_init_l.
*--- defaultvariante fürs Selektionsbild ermitteln ------------------
*--- über Flag sicherstellen Aufruf nur einmal ----------------------
  IF variant IS INITIAL AND
    g_variant_flag IS INITIAL.
    PERFORM get_default_variant_f14 USING variant.
    g_variant_flag = g_x.
  ENDIF.

*--- Konvertierung Technischer Platz
  PERFORM conversion_exit_tplnr_outp_f16 TABLES tplnr strno.

*--- convertsion exit WBS-element
  PERFORM covert_range_pspnr_posid_f24 TABLES proid proid_e.

*--- set Icon for adress-button
  PERFORM set_icon_f01 USING dy_adrfl ad_icon text-ad0 text-ad1.

*--- get classification data from select option
*--- (if new variant or if called via submit or F3)
  IF ( l_slset NE sy-slset ) OR
     ( s_comw[] IS NOT INITIAL AND g_clsd_comw[] IS INITIAL ).
    l_slset = sy-slset.
    g_class_old = dy_class.
    g_klart_old = dy_klart.
    PERFORM copy_selopt_comw_f79 TABLES g_clsd_comw s_comw.
    PERFORM class_search_init_f78 USING gc_obtab_iflo.
  ENDIF.

*--- set Icon for classification
  LOOP AT g_clsd_comw TRANSPORTING NO FIELDS              "note 844345
                      WHERE atcod > '0'.                  "note 844345
    EXIT.                                                "note 844345
  ENDLOOP.                                                "note 844345
  IF sy-subrc IS INITIAL.
    g_comw_flag = 'X'.
  ELSE.
    CLEAR g_comw_flag.
  ENDIF.
  PERFORM set_icon_f01 USING g_comw_flag cl_icon text-cl0 text-cl1.
*--- EhP605e Linear Asset Management
*--- Set Block lam_lfe off unswitched
  PERFORM eaml_selection_screen_output USING sy-repid.   "EhP605 EAML

*---------------------------------------------------------------------
* AT SELECTION_SCREEN ON BLOCK
*---------------------------------------------------------------------
AT SELECTION-SCREEN ON BLOCK clse.

*... Klasse / Klassenart geändert ?
  IF dy_class NE g_class_old
    OR dy_klart NE g_klart_old.
    g_class_old = dy_class.
    g_klart_old = dy_klart.
    CLEAR g_comw_flag.
    REFRESH g_clsd_comw.
    REFRESH s_comw.
  ENDIF.

  PERFORM class_exist_f78 USING dy_klart dy_class 'DY_CLASS'.

  IF sy-ucomm = 'COMW'.
    CALL FUNCTION 'IHCLSD_VALUATION_POPUP'
      EXPORTING
        i_klart               = dy_klart
        i_class               = dy_class
        i_language            = sy-langu
        i_key_date            = sy-datum
        i_also_subclasses     = dy_subcl
      TABLES
        ct_comw               = g_clsd_comw
      EXCEPTIONS
        exc_no_class          = 1
        exc_klart_not_allowed = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
*--- fill classification data in select option
    PERFORM copy_comw_selopt_f79 TABLES g_clsd_comw s_comw.
  ENDIF.
* wurde die Suche vom Pop-up aus gestartet ?
  IF sy-ucomm = 'ONLI'.
    sscrfields-ucomm = sy-ucomm.
  ENDIF.

ENHANCEMENT-POINT EHP605_RIIFLO20_02 SPOTS ZES_RIIFLO20 STATIC .


*---------------------------------------------------------------------
* AT SELECTION_SCREEN
*---------------------------------------------------------------------
AT SELECTION-SCREEN.
*--- immer Defaultvariante nehmen, da ALV-GRID diese immer nimmt
  IF variant IS INITIAL.
    PERFORM get_default_variant_f14 USING variant.
  ENDIF.
*--- Korrekte Listvariante ausgewählt ? -----------------------------
  PERFORM variant_existence_f14 USING variant.

  IF sy-ucomm = 'ADDR'.
    PERFORM adress_sel_f01 USING g_shlp-shlpname.
  ENDIF.

*--- wurde Partner Korrekt eingegeben?
  PERFORM check_parnr_f76.

*---------------------------------------------------------------------
* AT SELECTION_SCREEN ON VALUE-REQUEST
*---------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR dy_parnr.

  PERFORM f4_for_parnr_f76.

*---------------------------------------------------------------------
* START-OF-SELECTION
*---------------------------------------------------------------------
START-OF-SELECTION.

*--- Feldkatalog aufbauen -------------------------------------------
  PERFORM create_fieldcat_f14 USING 'RIHIFLO_LIST'.
* enhance fieldcatalog by LFE data for Linear Asset Objects
  IF cl_ops_switch_check=>eam_sfws_lfe( ) IS NOT INITIAL. "EhP605 EAML
    PERFORM eaml_create_fieldcat_f14 CHANGING g_fieldcat_tab.
  ENDIF.

  PERFORM create_fieldgroups_l.
*--- Aktivitätstyp bestimmen -----------------------------------------
  PERFORM determine_g_tcode_f16.
  PERFORM determine_acttype_iflo_l.

  REFRESH sel_tab.
  PERFORM export_seltab_mem_f16.
*--- Allgem. Einstellungen für Listviewer ----------------------------
  PERFORM prepare_display_list_f14.
*--- Besondere Einstellungen wegen Anzeige Klassifizierung -----------
  PERFORM modify_event_exit_tab_f50.
*--- ausgewählte Listfelder ermitteln für dynamischen select ---------
  PERFORM update_fieldcat_variant_f14.
*--- bei submit kann Feldcatalog auch importiert werden --------------
  IF NOT sy-calld IS INITIAL.
    PERFORM import_fieldcat_f14.
  ENDIF.
  PERFORM check_fieldcat_variant_l.

  PERFORM class_search_strategy_iflo_f78 USING strno[]
                                               g_clsd_comw
                                               g_clsd_strategy.

* BADI implementiert ?
* wenn ja, Selektionsbild holen und BADI rufen.
  IF NOT g_badi_list_editing_act IS INITIAL.
    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report           = sy-repid
*   IMPORTING
*     SP                    =
      TABLES
        selection_table       = g_clsd_select_options
      EXCEPTIONS
        not_found             = 1
        no_report             = 2
        OTHERS                = 3
            .
    IF sy-subrc <> 0.
      STOP.
    ENDIF.

    CALL METHOD g_badi_list_editing_ref->search_strategy_for_iflo
      EXPORTING
        i_select_options  = g_clsd_select_options
        i_klart           = dy_klart
        i_class           = dy_class
        i_comw            = g_clsd_comw
      CHANGING
        c_search_strategy = g_clsd_strategy
        c_max_md_count    = gc_max_md_count
        c_max_hits        = gc_max_hits.

  ENDIF.

* EAM CC 200                                                "1656798
  include eamcc_riiflo20_2 if found.                        "1656798

  PERFORM selection_l.

*eject
*---------------------------------------------------------------------
* END-OF-SELECTION
*---------------------------------------------------------------------
END-OF-SELECTION.
  g_ucomm = 'IOBJ'.
*--- Liste ausgeben --------------------------------------------------*
  PERFORM display_list_f14 USING g_ucomm.
  IF p_path IS NOT INITIAL.
    PERFORM write_appl_server.
  ENDIF.

*---------------------------------------------------------------------*
*       FORM USER_COMMAND_L                                           *
*---------------------------------------------------------------------*
*       will be called out of listviewer                              *
*---------------------------------------------------------------------*
*  -->  P_UCOMM                                                       *
*  -->  P_SELFIELD                                                    *
*---------------------------------------------------------------------*
FORM user_command_l USING p_ucomm LIKE sy-ucomm
                          p_selfield TYPE slis_selfield.

*--- BAdI references for menu BAdIs
  DATA: lb_badi_cus1 TYPE REF TO badi_eam_riiflo20_fcode_cus1,
        lb_badi_sap1 TYPE REF TO badi_eam_riiflo20_fcode_sap1.

* begin of note 1656798
* EAM CC 200
  data: lt_object_tab like standard table of object_tab.
  data: lv_eam_cc_processed type xfeld.
* end of note 1656798

  p_selfield-refresh = g_s.
  g_index = p_selfield-tabindex.

  PERFORM set_p_selfield_general_f16 USING p_selfield.
*--- pf2 umbiegen je nach modus (Auswählen/Anzeigen) ---------------
  PERFORM check_pf2_with_object_f16 USING p_ucomm.

*--- Fcode umbiegen bei Doppelcklick auf Meldungsnummer oder text --
  PERFORM check_object_display_f16 USING p_ucomm
                                         p_selfield
                                         'OBJECT_TAB-TPLNR'
                                         'LO  '.
  PERFORM check_object_display_f16 USING p_ucomm
                                         p_selfield
                                         'OBJECT_TAB-PLTXT'
                                         'LO  '.

  INCLUDE EAMCC3_MIOLX_01 IF FOUND. "Customer Connect EAM 2014, IR 7135, note 2079318

  CASE p_ucomm.
    WHEN g_ol0.
*--- Aktuelle Feldauswahl ändern -------------------------------------
      PERFORM refresh_l USING p_selfield.
    WHEN g_olx.
*--- Feldauswahl ändern ---------------------------------------------
      PERFORM refresh_l USING p_selfield.
    WHEN g_oad.
*--- Feldauswahl auswählen ------------------------------------------
      PERFORM refresh_l USING p_selfield.
    WHEN g_lis.
*--- Grundliste aufbauen --------------------------------------------
      PERFORM refresh_l USING p_selfield.
    WHEN 'AKTU'.
*--- Auffrischen ----------------------------------------------------
      p_selfield-refresh = g_x.
*--- Klassifizierung ausschalten ------------------------------------
      IF g_class_on = yes.
* begin of note 1656798
* EAM CC 200

*       PERFORM classification_off_f50. (old coding)
        clear lv_eam_cc_processed.
        include eamcc_riiflo20_31 if found.

        if lv_eam_cc_processed is INITIAL.
          PERFORM classification_off_f50.
        endif.
* end of note 1656798

        g_class_on = yes.
      ENDIF.
*--- Datenbankselektion ---------------------------------------------
      PERFORM selection_l.
*--- Klassifizierung wieder einschalten -----------------------------
      IF g_class_on = yes.
* begin of note 1656798
* EAM CC 200

*       PERFORM classification_on_f50.  (old form call)
        clear lv_eam_cc_processed.
        include eamcc_riiflo20_32 if found.

        if lv_eam_cc_processed is INITIAL.
          PERFORM classification_on_f50.
        endif.
* end of note 1656798

      ENDIF.
    WHEN 'IOBJ'.
*--- Objektstammsatz anzeigen ---------------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      MOVE-CORRESPONDING object_tab TO rihiflo.
      rihiflo_list = object_tab.
      PERFORM master_data_f16 USING p_ucomm
                                    p_selfield.
*--- Wegen doppelclick sicherstellen das F-Code nicht zweimal -------
      CLEAR p_ucomm.
    WHEN 'CLVI'.
*--- Klassifizierungsdaten einblenden -------------------------------
* begin of note 1656798
* EAM CC 200

*       PERFORM classification_f50. (old form call)
        clear lv_eam_cc_processed.
        include eamcc_riiflo20_33 if found.

         if lv_eam_cc_processed is INITIAL.
          PERFORM classification_f50.
         endif.
   when 'CLLO'.
        include eamcc_riiflo20_34 if found.
* end of note 1656798

    WHEN 'TPPL'.
*--- Liste Arbeitspläne --------------------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM display_tppl_l.
    WHEN 'EL  '.
*--- Equipmentliste ------------------------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM display_equi_l.
    WHEN 'LO  '.
*--- Detail Techn.Platz Standortdaten ------------------------------
      PERFORM fcodes_with_mark_f16 USING p_ucomm p_selfield.
    WHEN 'PM  '.
*--- Detail Techn.Platz IH-Daten -----------------------------------
      PERFORM fcodes_with_mark_f16 USING p_ucomm p_selfield.
    WHEN 'TX  '.
*--- Detail Techn.Platz Texte --------------------------------------
      PERFORM fcodes_with_mark_f16 USING p_ucomm p_selfield.
    WHEN 'NET1'.
*--- in Netzverbingen spingen (Liste) ------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM display_net1_l.
    WHEN 'NET2'.
*--- in Netzobjekte spingen (Liste) --------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM display_net2_l.
    WHEN 'MELD'.
*--- Meldungsliste -------------------------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM display_qmel_l.
    WHEN 'AUFK'.
*--- Auftragsliste -------------------------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM display_aufk_l.
    WHEN 'MHIS'.
*--- Wartungsplansimulation ----------------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM display_mhis_l.
    WHEN 'MPOS'.
*--- Wartungspositionen --------------------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM display_mpos_l.
    WHEN 'PLAN'.
*--- Anleitungen (Selektion über Bauteil) --------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM display_plan_l.
    WHEN 'MPTS'.
*--- Meßpunkte -----------------------------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM display_mpts_l.
    WHEN 'VERT'.
*--- Serviceverträge -----------------------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM display_vert_l.
    WHEN 'MUOB'.
*--- mehrstufige Equiliste ----------------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM multi_object_l.
    WHEN 'MUQM'.
*--- mehrstufige Meldungsliste ------------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM multi_qmel_l.
    WHEN 'MUAU'.
*--- mehrstufige Auftragsliste ------------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM multi_aufk_l.
    WHEN 'HILI'.
*--- Strukturdarstellung (Listmodus) ------------------------------
      PERFORM fcodes_with_mark_f16 USING p_ucomm p_selfield.
    WHEN 'HIGR'.
*--- Strukturdarstellung (Grafikmodus) ----------------------------
      PERFORM fcodes_with_mark_f16 USING p_ucomm p_selfield.
    WHEN 'LBFU'.
*--- Kennzeichnung Benutzerprofile einstellen ---------------------
      PERFORM set_profile_strno_l USING p_selfield.
    WHEN 'IW21'.
*--- Meldung allgemein anlegen ------------------------------------
      PERFORM execute_tcode_l USING p_ucomm p_selfield.
    WHEN 'IW24'.
*--- Störmeldung anlegen ------------------------------------------
      PERFORM execute_tcode_l USING p_ucomm p_selfield.
    WHEN 'IW25'.
*--- Anforderung anlegen ------------------------------------------
      PERFORM execute_tcode_l USING p_ucomm p_selfield.
    WHEN 'IW26'.
*--- Tätigkeitsmeldung anlegen ------------------------------------
      PERFORM execute_tcode_l USING p_ucomm p_selfield.
    WHEN 'IW31'.
*--- Auftrag anlegen ----------------------------------------------
      PERFORM execute_tcode_l USING p_ucomm p_selfield.
    WHEN 'LGTX'.
*--- Langtext anzeigen --------------------------------------------
      PERFORM fcodes_with_mark_f16 USING p_ucomm p_selfield.
    WHEN 'PHON'.
*--- Telefonanruf starten -----------------------------------------
      PERFORM fcodes_with_mark_f16 USING p_ucomm p_selfield.
    WHEN 'LVMS'.
*--- Löschvormerkung setzten --------------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM status_change_multi_f25 USING 'LVMS' 'IFL'.
    WHEN 'LVMZ'.
*--- Löschvormerkung zurücknehmen ---------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM status_change_multi_f25 USING 'LVMZ' 'IFL'.
    WHEN 'INAK'.
*--- Inaktivieren -------------------------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM status_change_multi_f25 USING 'INAK' 'IFL'.
    WHEN 'INAZ'.
*--- Inaktivieren zurücknehmen --------------------------------------
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.
      PERFORM status_change_multi_f25 USING 'INAZ' 'IFL'.
    WHEN '+SAP1' OR '+WCM1'.
*--- SAP enhancement, only active if BADI_EAM_RIIFLO20_FCODE_SAP1 is active
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.

      GET BADI lb_badi_sap1.

      CALL BADI lb_badi_sap1->execute_function_code
        EXPORTING
          iv_function_code    = p_ucomm
          iv_activity_type    = g_aktyp
          it_selected_objects = object_tab[].

    WHEN '+CUS1'.
*--- Customer enhancement, only active if BADI_EAM_RIIFLO20_FCODE_CUS1 is active
      PERFORM check_object_tab_marked_f14 USING p_ucomm
                                                p_selfield.

      GET BADI lb_badi_cus1.

      CALL BADI lb_badi_cus1->execute_function_code
        EXPORTING
          iv_function_code    = p_ucomm
          iv_activity_type    = g_aktyp
          it_selected_objects = object_tab[].

    WHEN 'EAML_MASS'.
      "mass change of LFE data
      IF cl_ops_switch_check=>eam_sfws_lfe( ) = abap_true.
        CALL METHOD cl_eaml_mass_change=>call_lfe_mass_change
          EXPORTING
            iv_obart     = cl_eaml_util=>gc_obart-floc
            it_selection = object_tab[].
        "refresh
        PERFORM user_command_l USING 'AKTU' p_selfield.
      ENDIF.
    WHEN OTHERS.
ENHANCEMENT-POINT EHP_RIIFLO20_02 SPOTS ZES_RIIFLO20 .

ENHANCEMENT-SECTION USER_COMMAND_L_01 SPOTS ZES_RIIFLO20 .
*--- zentrale F-codes für alle Listen -----------------------------
      PERFORM user_command_f16 USING p_ucomm p_selfield.
END-ENHANCEMENT-SECTION.
  ENDCASE.

*--- If list is empty now - leave ALV
  IF object_tab[] IS INITIAL AND g_variant_save NE g_x.
    p_selfield-exit = g_x.
    MESSAGE s047(ih).
  ENDIF.

  INCLUDE EAMCC3_MIOLX_02 IF FOUND. "Customer Connect EAM 2014, IR 7135, note 2079318

ENDFORM.                    "user_command_l


*eject
*---------------------------------------------------------------------*
*       FORM FCODES_WITH_MARK_L                                       *
*---------------------------------------------------------------------*
*       FCodes, die auch im Loop verarbeitet werden können            *
*---------------------------------------------------------------------*
FORM fcodes_with_mark_l USING f_ucomm LIKE sy-ucomm
                              f_selfield TYPE slis_selfield.
  DATA:
     h_tabix LIKE sy-tabix,
     h_tplnr LIKE object_tab-tplnr_int,
     h_iflo  LIKE iflo,
     h_ucomm LIKE t185f-fcode,
     f_tcode LIKE sy-tcode,
     h_left_with_f15,
     h_update_success,
     h_rc LIKE sy-subrc,
     h_rilo0    LIKE rilo0,
     ls_rihbgmd TYPE rihbgmd.

  DATA: BEGIN OF h_equi_tab OCCURS 0.
          INCLUDE STRUCTURE rihequi.
  DATA: END OF h_equi_tab.

  DATA: BEGIN OF h_iflo_tab OCCURS 0.
          INCLUDE STRUCTURE rihiflo.
  DATA: END OF h_iflo_tab.

  DATA: BEGIN OF h_stpo_tab OCCURS 0.
          INCLUDE STRUCTURE rihstpx.
  DATA: END OF h_stpo_tab.

  MOVE-CORRESPONDING object_tab TO rihiflo.
  rihiflo_list = object_tab.

  CASE f_ucomm.
    WHEN 'HILI'.
      CALL FUNCTION 'PM_HIERARCHY_CALL'
        EXPORTING
          grafics        = ' '
          tplnr          = rihiflo_list-tplnr_int
          with_equi      = 'X'
          with_equi_hier = 'X'
          with_iflo_hier = 'X'
          with_mara      = 'X'
        TABLES
          equi_tab       = h_equi_tab
          iflo_tab       = h_iflo_tab
          stpo_tab       = h_stpo_tab
        EXCEPTIONS
          no_hierarchy   = 01
          no_selection   = 02.
      CASE sy-subrc.
        WHEN 01.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno.
        WHEN 02.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno.
      ENDCASE.
    WHEN 'HIGR'.
      CALL FUNCTION 'PM_HIERARCHY_CALL'
        EXPORTING
          grafics        = 'X'
          tplnr          = rihiflo_list-tplnr_int
          with_equi      = 'X'
          with_equi_hier = 'X'
          with_iflo_hier = 'X'
          with_mara      = 'X'
        TABLES
          equi_tab       = h_equi_tab
          iflo_tab       = h_iflo_tab
          stpo_tab       = h_stpo_tab
        EXCEPTIONS
          no_hierarchy   = 01
          no_selection   = 02.
      CASE sy-subrc.
        WHEN 01.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno.
        WHEN 02.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno.
      ENDCASE.
    WHEN 'LGTX'.
      PERFORM display_longtext_l.
    WHEN 'PHON'.
      PERFORM phon_f70 USING f_selfield.
    WHEN OTHERS.
      IF f_ucomm <> 'ISEL' AND
         f_ucomm <> 'IOBJ'.
        h_ucomm = f_ucomm.
      ENDIF.
      IF t370a-aktyp = 'V'.
        f_tcode = 'IL02'.
      ELSE.
        f_tcode = 'IL03'.
      ENDIF.
*--- Berechtigungsprüfung auf T-code -------------------------------*
      PERFORM auth_check_tcode_f16 USING f_tcode
                                   CHANGING h_rc.
      IF NOT h_rc IS INITIAL.
        EXIT.
      ENDIF.

      REFRESH iflo_u.
      EXPORT iflo_u TO MEMORY ID 'INH'.
*--- EhP605e Linear Asset Management
      IF cl_ops_switch_check=>eam_sfws_lfe( ) IS NOT INITIAL. "EhP605 EAML
        cl_eaml_reporting=>memory_export(
                           EXPORTING
                             iv_report      = sy-repid
                             is_output_line = object_tab ).
      ENDIF.

      CALL FUNCTION 'FUNC_LOCATION_CALL'
        EXPORTING
          fcode          = h_ucomm
          tcode          = f_tcode
          tplnr          = object_tab-tplnr_int
        IMPORTING
          iflo_ba        = h_iflo
          rilo0_wa       = h_rilo0
          left_with_f15  = h_left_with_f15
          update_success = h_update_success.

*--- Bei 'F15' RETURN_CODE auf 8 ------------------------------------*
      IF NOT h_left_with_f15 IS INITIAL.
        return_code = 8.
      ELSE.
        return_code = 0.
      ENDIF.

*--- Der Platz wurde direkt angezeigt -------------------------------*
      IF g_lines = 1.
        LEAVE.
      ENDIF.
*--- externe Statusbezeichung und ext. Arbeitplatzbez. --------------*
      rilo0 = h_rilo0.
*--- Objekttabelle aktualisieren ------------------------------------*
      IF NOT h_update_success IS INITIAL.
*--- update directly changed func.loc. at first
        iflo = h_iflo.
*--- save warranty data. overwrite them only if changed
        MOVE-CORRESPONDING object_tab TO ls_rihbgmd.
        PERFORM move_iflo_to_object_tab_l USING iflo object_tab.
        MOVE-CORRESPONDING ls_rihbgmd TO object_tab.

        PERFORM fill_status_arbpl_l.
        PERFORM fill_object_tab_late_l.
        MODIFY object_tab.

*--- update changes from inheritance
        IMPORT iflo_u FROM MEMORY ID 'INH'.
        IF sy-subrc = 0.
          LOOP AT iflo_u WHERE tplnr NE h_iflo-tplnr.
            h_tplnr = iflo_u-tplnr.
            READ TABLE object_tab WITH KEY tplnr_int = h_tplnr.
            h_tabix = sy-tabix.
            IF sy-subrc = 0.
              iflo = iflo_u.
*--- save warranty data. overwrite them only if changed
              MOVE-CORRESPONDING object_tab TO ls_rihbgmd.
              PERFORM move_iflo_to_object_tab_l USING iflo object_tab.
              MOVE-CORRESPONDING ls_rihbgmd TO object_tab.
              PERFORM fill_status_arbpl_l.
              PERFORM fill_object_tab_late_l.
              MODIFY object_tab INDEX h_tabix.
            ENDIF.
          ENDLOOP.
          IF sy-subrc = 0.
*--- read current changed functional location
            READ TABLE object_tab WITH KEY tplnr_int = h_iflo-tplnr.
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.

*--- Zeile als besucht markieren
  PERFORM mark_selected_f16 CHANGING object_tab-selected
                                     object_tab-pm_selected.

ENDFORM.                    "fcodes_with_mark_l

*---------------------------------------------------------------------*
*       FORM FILL_STATUS_ARBPL_L                                      *
*---------------------------------------------------------------------*
*    Status und Arbeitplatz RILO0-Stuktur aktualisieren               *
*    nicht für Plätze die über Datenweitergabe geändert wurden        *
*---------------------------------------------------------------------*
FORM fill_status_arbpl_l.

  IF iflo-tplnr = object_tab-tplnr_int.
    object_tab-sttxt = rilo0-sttxt.
    object_tab-ustxt = rilo0-sttxu.
    object_tab-gewrk = rilo0-gewrk.
  ENDIF.

ENDFORM.                    "fill_status_arbpl_l
*eject
*---------------------------------------------------------------------*
*       FORM SELECTION_L                                              *
*---------------------------------------------------------------------*
*       Technische Plätze selektieren                               *
*---------------------------------------------------------------------*
FORM selection_l.

  DATA: h_iflo_tab TYPE TABLE OF iflo.

  DATA: l_use_tplnr TYPE flag VALUE 'X',
        l_use_fae   TYPE flag.

  RANGES: lr_tplnr      FOR iflo-tplnr,
          lr_tplnr_save FOR iflo-tplnr,
          lr_dummy      FOR crhd-werks.

  FIELD-SYMBOLS: <ls_iflo> TYPE iflo.

  IF g_selmod <> selmod_d.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = 'Selektion Technischer Platz'(201).
  ENDIF.

  CLEAR: g_sttxt_flag,
         g_stasl_flag,
         g_arbpl_flag,
         g_gewrk_flag,
         g_adres_flag,
         g_konvr_flag,
         g_mganr_flag,
         g_submt_flag.

  CLEAR   object_tab.                                       "N1025866
  REFRESH object_tab.                                       "N1025866

*--- Save original selection criteria
  lr_tplnr_save[] = tplnr[].

*--- Adressen selektieren ----------------------------------------
  IF NOT dy_adrfl IS INITIAL.
    PERFORM fill_adrnr(sapdbtpi) TABLES tplnr[] strno[].
*--- Selektion nach Adressen -> keine Adresssen gefunden -> Exit --
    DESCRIBE TABLE tplnr LINES sy-tabix.
    IF sy-tabix = 0.
      MESSAGE s047.
      EXIT.
    ENDIF.
  ENDIF.
*--- Konvertierungsexit T.P. berücksichtigen ------------------------*
  PERFORM check_tplnr_f16 TABLES strno
                                 lr_tplnr
                          USING  ' '.
  IF  NOT strno[] IS INITIAL                      "N1436283
  AND lr_tplnr[] IS INITIAL.                      "N1436283
    EXIT.                                                   "N1025866
  ENDIF.                                                    "N1025866

*--- Vorselektion über Adresse beide ranges abmischen
  IF NOT dy_adrfl IS INITIAL.
    LOOP AT tplnr.
      IF NOT tplnr-low IN lr_tplnr.
        DELETE tplnr.
      ENDIF.
    ENDLOOP.
    IF tplnr[] IS INITIAL.
*--- Keine Plätze über Adresse gefunden
      MESSAGE s047.
      EXIT.
    ENDIF.
  ELSE.
    tplnr[] = lr_tplnr[].
  ENDIF.

  CLEAR lr_tplnr. REFRESH lr_tplnr.

  REFRESH g_tplnr_md.
  REFRESH g_tplnr_cl.

*--- Shall selection be done via linear data ------------------------*
  IF cl_ops_switch_check=>eam_sfws_lfe( ) IS NOT INITIAL. "EHP605 EAML
    CALL METHOD cl_eaml_reporting=>get_range_tables
      EXPORTING
        iv_report        = sy-repid
        iv_tcode         = sy-tcode
        ir_lrpid         = gr_lrpid
        iv_linear_unit   = gv_uom
        ir_start_point   = gr_start
        ir_end_point     = gr_end
        ir_linear_length = gr_len
      CHANGING
        cr_range         = tplnr[].
  ENDIF.

  IF NOT g_clsd_strategy IS INITIAL.
    IF  NOT dy_klart IS INITIAL                             " 1117008
    AND NOT dy_class IS INITIAL.                            " 1117008
      PERFORM class_search_iflo_early_f78 USING tplnr[]
                                                g_tplnr_cl[].
    ENDIF.                                                  " 1117008
  ENDIF.

  CLEAR object_tab.
  REFRESH object_tab.

  CLEAR g_adrnr_sel_tab.
  REFRESH g_adrnr_sel_tab.
*--- Soll u.a. über Genehmigungen selektiert werden ---------------*
  PERFORM select_via_sogen_f18 USING '2'.
  PERFORM fill_tplnr_from_sogen_l.
*--- Soll über verantw. Arbeitsplatz selektiert werden ---------------*
  PERFORM check_sel_workcenter_f66 TABLES gewrk lr_dummy i_gewrk
                                   USING  g_crhd_flag space.
*--- Soll über PPS-Arbeitsplatz selektiert werden ---------------*
  PERFORM check_sel_workcenter_f66 TABLES arbpl swerk i_ppsid
                                   USING  g_crhd_flag space.

*--- selection witn WBS element -> use internal numbers
  PERFORM check_sel_proid_f24 TABLES proid_e proid gr_proid_i_all
                              USING  'PROID'.

*--- Soll über zusätzliche Statuseingrenzung selektiert werden -----
  PERFORM check_sel_stati_l USING g_answer.
  IF g_answer = no.
    EXIT.
  ENDIF.

*--- Groß und Kleinschreibung bei Kurztext ignorieren ----------------
  SET LOCALE LANGUAGE sy-langu.
  LOOP AT pltxt.
    TRANSLATE pltxt-low  TO UPPER CASE.                  "#EC TRANSLANG
    TRANSLATE pltxt-high TO UPPER CASE.                  "#EC TRANSLANG
    MODIFY pltxt.
  ENDLOOP.
*--- puffer Status initialisieren -----------------------------------
  CALL FUNCTION 'STATUS_BUFFER_REFRESH'.

  IF NOT dy_parnr IS INITIAL.
*--- Technische Plätze lesen über Partner -----------------------
    PERFORM get_tplnr_from_ihpa_l.
  ENDIF.

*--- Wenn Primärrangetab zu groß -> select ändern -------------------*
  DESCRIBE TABLE tplnr LINES sy-tabix.
  IF sy-tabix > 50.
    l_use_fae = 'X'.
*--- keine generisches Eingrenzen erlaubt ----------------------------
    LOOP AT tplnr TRANSPORTING NO FIELDS
                  WHERE sign <> 'I' OR option <> 'EQ'.
      CLEAR l_use_fae.
      CLEAR l_use_tplnr.
      lr_tplnr[] = tplnr[].
      CLEAR tplnr. REFRESH tplnr.
      EXIT.
    ENDLOOP.
  ENDIF.
*--- Rangetab zu groß -> for all entries benützen -------------------*
  IF l_use_fae = 'X'.
    SELECT * FROM iflo INTO TABLE h_iflo_tab
             FOR ALL ENTRIES IN tplnr
                         WHERE tplnr =  tplnr-low
                         AND   pltxu IN pltxt
                         AND   owner =  '2'
                         AND   iwerk IN iwerk
                         AND   kostl IN kostl
                         AND   ingrp IN ingrp
                         AND   submt IN submt
                         AND   fltyp IN fltyp
                         AND   swerk IN swerk
                         AND   msgrp IN msgrp
                         AND   rbnr  IN rbnr
                         AND   mapar IN mapar
                         AND   stort IN stort
                         AND   bukrs IN bukrs
                         AND   trpnr IN trpnr
                         AND   begru IN begru
                         AND   abckz IN abckz
                         AND   anlnr IN anlnr
                         AND   kokrs IN kokrs
                         AND   beber IN beber
                         AND   gsber IN gsber
                         AND   daufn IN daufn
                         AND   aufnr IN aufnr
                         AND   erdat IN erdat
                         AND   ernam IN ernam
                         AND   aedat IN aedat
                         AND   aenam IN aenam
                         AND   eqfnr IN eqfnr
                         AND   proid IN proid
                         AND   vkorg IN vkorg
                         AND   vtweg IN vtweg
                         AND   spart IN spart
                         AND   eqart IN eqart
                         AND   invnr IN invnr
                         AND   groes IN groes
                         AND   brgew IN brgew
                         AND   gewei IN gewei
                         AND   ansdt IN ansdt
                         AND   answt IN answt
                         AND   waers IN waers
                         AND   herst IN herst
                         AND   herld IN herld
                         AND   baujj IN baujj
                         AND   typbz IN typbz
                         AND   serge IN serge
                         AND   lgwid IN i_gewrk
                         AND   ppsid IN i_ppsid.
  ELSE.
*--- normale selektion über view -------------------------------------*
    SELECT * FROM iflo INTO TABLE h_iflo_tab
                         WHERE tplnr IN tplnr
                         AND   pltxu IN pltxt
                         AND   owner =  '2'
                         AND   iwerk IN iwerk
                         AND   kostl IN kostl
                         AND   ingrp IN ingrp
                         AND   submt IN submt
                         AND   fltyp IN fltyp
                         AND   swerk IN swerk
                         AND   msgrp IN msgrp
                         AND   rbnr  IN rbnr
                         AND   mapar IN mapar
                         AND   stort IN stort
                         AND   bukrs IN bukrs
                         AND   trpnr IN trpnr
                         AND   begru IN begru
                         AND   abckz IN abckz
                         AND   anlnr IN anlnr
                         AND   kokrs IN kokrs
                         AND   beber IN beber
                         AND   gsber IN gsber
                         AND   daufn IN daufn
                         AND   aufnr IN aufnr
                         AND   erdat IN erdat
                         AND   ernam IN ernam
                         AND   aedat IN aedat
                         AND   aenam IN aenam
                         AND   eqfnr IN eqfnr
                         AND   proid IN proid
                         AND   vkorg IN vkorg
                         AND   vtweg IN vtweg
                         AND   spart IN spart
                         AND   eqart IN eqart
                         AND   invnr IN invnr
                         AND   groes IN groes
                         AND   brgew IN brgew
                         AND   gewei IN gewei
                         AND   ansdt IN ansdt
                         AND   answt IN answt
                         AND   waers IN waers
                         AND   herst IN herst
                         AND   herld IN herld
                         AND   baujj IN baujj
                         AND   typbz IN typbz
                         AND   serge IN serge
                         AND   lgwid IN i_gewrk
                         AND   ppsid IN i_ppsid.
  ENDIF.

  LOOP AT h_iflo_tab ASSIGNING <ls_iflo>.
*--- If TPLNR is not used for selection, filter out now
    IF l_use_tplnr IS INITIAL.
      CHECK <ls_iflo>-tplnr IN lr_tplnr.
    ENDIF.
*--- if no preselection for WBS possible -> check now
    CHECK <ls_iflo>-proid IN gr_proid_i_all.
*--- check language
    CHECK <ls_iflo>-spras EQ <ls_iflo>-mlang OR
          <ls_iflo>-spras EQ sy-langu.
    PERFORM move_iflo_to_object_tab_l USING <ls_iflo> object_tab.
    APPEND object_tab-tplnr_int TO g_tplnr_md.
    APPEND object_tab.
  ENDLOOP.
*--- restore TPLNR if not used for selection
  IF l_use_tplnr IS INITIAL.
    tplnr[] = lr_tplnr[].
  ENDIF.
  FREE h_iflo_tab.

* Klassensuche spät, object_tab zur Einschränkung mitgeben
  IF g_clsd_strategy IS INITIAL.
    IF NOT dy_klart IS INITIAL
      AND NOT dy_class IS INITIAL.
      PERFORM class_search_iflo_late_f78 USING g_tplnr_md[]
                                               g_tplnr_cl[].
      IF g_tplnr_cl[] IS INITIAL.
        REFRESH object_tab.
        CLEAR object_tab.
        STOP.
      ENDIF.
    ENDIF.
  ENDIF.

* Abmischen notwendig ?
  IF NOT g_tplnr_cl[] IS INITIAL.
    PERFORM validate_objects_iflo_f78 USING g_tplnr_cl[].
  ENDIF.
  DESCRIBE TABLE object_tab LINES sy-tabix.
  CHECK NOT sy-tabix IS INITIAL.

  PERFORM delete_wrong_lang_l.
  PERFORM status_check_f16 USING selschem.
  PERFORM authority_check_l.
  PERFORM fill_object_tab_l.
* EhP605e merge LFE data for Linear Asset Objects
  IF cl_ops_switch_check=>eam_sfws_lfe( ) IS NOT INITIAL. "EhP605 EAML
    PERFORM eaml_merge_object_tab USING    sy-repid
                                  CHANGING object_tab[].
  ENDIF.

*--- Defaultsortiertung wenn nichts über SALV eingestellt -----------
  IF g_sortfields_tab[] IS INITIAL.
    SORT object_tab BY tplnr.
  ENDIF.

*--- Restore original selection criteria
  tplnr[] = lr_tplnr_save[].

ENDFORM.                    "selection_l

*eject
*---------------------------------------------------------------------*
*       FORM FILL_OBJECT_TAB_LATE_L                                   *
*---------------------------------------------------------------------*
*       Sonderbehandlung Felder nach Ausflug                          *
*---------------------------------------------------------------------*
FORM fill_object_tab_late_l.

  DATA: ls_bgmkobj_out TYPE bgmkobj,
        ls_bgmkobj_in  TYPE bgmkobj.

*--- Statusleiste füllen ---------------------------------------------*
  IF NOT object_tab-objnr IS INITIAL.
    IF g_sttxt_flag = yes OR g_sttxt_flag = ok.
*--- puffer löschen damit neuerster Stand ermittelt wird -------------*
      CALL FUNCTION 'STATUS_BUFFER_REFRESH'.
      CALL FUNCTION 'STATUS_TEXT_EDIT'
        EXPORTING
          objnr            = object_tab-objnr
          spras            = sy-langu
          flg_user_stat    = 'X'
        IMPORTING
          line             = object_tab-sttxt
          user_line        = object_tab-ustxt
        EXCEPTIONS
          object_not_found = 01.
      IF sy-subrc <> 0.
        CLEAR  object_tab-sttxt.
        CLEAR  object_tab-ustxt.
      ENDIF.
    ENDIF.
  ENDIF.

*--- Externe Arbeitsplatznummer bestimmen ----------------------------*
  IF g_arbpl_flag = yes OR g_arbpl_flag = ok.
    IF NOT object_tab-ppsid IS INITIAL.
      CALL FUNCTION 'CR_WORKSTATION_READ'
        EXPORTING
          id        = object_tab-ppsid
          msgty     = 'S'
        IMPORTING
          arbpl     = object_tab-arbpl
        EXCEPTIONS
          not_found = 01.
      IF sy-subrc <> 0.
        CLEAR object_tab-arbpl.
      ENDIF.
    ELSE.
      CLEAR object_tab-arbpl.
    ENDIF.
  ENDIF.
*--- Externes Leitgewerk bestimmen -----------------------------------*
  IF g_gewrk_flag = yes OR g_arbpl_flag = ok.
    IF NOT object_tab-igewrk IS INITIAL.
      CALL FUNCTION 'CR_WORKSTATION_READ'
        EXPORTING
          id        = object_tab-igewrk
          msgty     = 'S'
        IMPORTING
          arbpl     = object_tab-gewrk
        EXCEPTIONS
          not_found = 01.
      IF sy-subrc <> 0.
        CLEAR object_tab-gewrk.
      ENDIF.
    ELSE.
      CLEAR object_tab-gewrk.
    ENDIF.
  ENDIF.

*--- ADRESSE BESTIMMEN -----------------------------------------------*
  IF g_adres_flag = yes OR g_adres_flag = ok.
    IF NOT object_tab-adrnr IS INITIAL.
      PERFORM read_adress_new_f17 USING object_tab-adrnr
                                        object_tab-tel_number
                                        object_tab-name_list
                                        object_tab-post_code1
                                        object_tab-city1
                                        object_tab-city2
                                        object_tab-country
                                        object_tab-region
                                        object_tab-street.
    ENDIF.
  ENDIF.
*--- Bezeichung Bautyp übernehmen
  IF g_submt_flag = yes OR g_submt_flag = ok.
    IF NOT object_tab-submt IS INITIAL.
      CALL FUNCTION 'IREP1_MATERIAL_TEXT_READ'
        EXPORTING
          i_matnr       = object_tab-submt
        IMPORTING
          e_maktx       = object_tab-submtktx
        EXCEPTIONS
          no_text_found = 1
          OTHERS        = 2.
      IF NOT sy-subrc IS INITIAL.
        CLEAR object_tab-submtktx.
      ENDIF.
    ENDIF.
  ENDIF.
*--- get warranty data
  IF g_mganr_flag = yes OR g_mganr_flag = ok.
*--- If warranty changed -> exported in BG00
    IMPORT bgmkobj_out = ls_bgmkobj_out
           text_out    = object_tab-gartx_k
           bgmkobj_in  = ls_bgmkobj_in
           text_in     = object_tab-gartx_l
           FROM MEMORY ID 'BGMKOBJ'.
    IF sy-subrc IS INITIAL.
*--- warranty data have changed -> take from memory
*--- otherwise use old values
      FREE MEMORY ID 'BGMKOBJ'.
      object_tab-gwldt_k = ls_bgmkobj_out-gwldt.
      object_tab-gwlen_k = ls_bgmkobj_out-gwlen.
      object_tab-mganr_k = ls_bgmkobj_out-mganr.
      object_tab-gwldt_l = ls_bgmkobj_in-gwldt.
      object_tab-gwlen_l = ls_bgmkobj_in-gwlen.
      object_tab-mganr_l = ls_bgmkobj_in-mganr.
    ENDIF.
  ENDIF.

*--- weitere Platzkennzeichungen übernehmen
  PERFORM fill_add_tplnrx_l CHANGING object_tab.
*--- EhP605e Linear Asset Management merge linear data
  IF cl_ops_switch_check=>eam_sfws_lfe( ) IS NOT INITIAL. "EhP605 EAML
    cl_eaml_reporting=>memory_import(
                       EXPORTING
                         iv_key1        = object_tab-tplnr_int
                       CHANGING
                         cs_output_line = object_tab ).
  ENDIF.
*--- fill additional field with BAdI BADI_EAM_SINGLELEVEL_LIST
  PERFORM fill_additional_fields_f16 USING 'RIHIFLO_LIST'
                                     CHANGING object_tab.

ENDFORM.                    "fill_object_tab_late_l

*---------------------------------------------------------------------*
*       FORM DELETE_WRONG_LANG_L                                      *
*---------------------------------------------------------------------*
*       Einträge in überflüssigen Sprachen entfernen                  *
*---------------------------------------------------------------------*
FORM delete_wrong_lang_l.

*### Datenvereinbarungen ############################################*
  DATA: h_tabix LIKE sy-tabix,
        h_tplnr LIKE rihiflo-tplnr.

*### Verarbeitung ###################################################*
  SORT object_tab BY tplnr.
  LOOP AT object_tab WHERE spras <> sy-langu.
    h_tplnr = object_tab-tplnr_int.
    h_tabix = sy-tabix + 1.
    READ TABLE object_tab INDEX h_tabix.
    IF h_tplnr = object_tab-tplnr_int AND sy-subrc = 0.
      DELETE object_tab.
    ELSE.
      h_tabix = h_tabix - 2.
      IF h_tabix > 0.
        READ TABLE object_tab INDEX h_tabix.
        IF h_tplnr = object_tab-tplnr_int.
          DELETE object_tab.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "delete_wrong_lang_l

*eject
*---------------------------------------------------------------------*
*       FORM MOVE_IFLO_TO_OBJECT_TAB_L                            *
*---------------------------------------------------------------------*
*       Move von IFLO nach OBJECT_TAB                             *
*       mit versorgen aller besonders zu ermittelnden Felder          *
*---------------------------------------------------------------------*
*  -->  PS_IFLO        selected structure                             *
*  -->  PS_OBJECT_TAB  output structure                               *
*---------------------------------------------------------------------*
FORM move_iflo_to_object_tab_l USING ps_iflo       TYPE iflo
                                     ps_object_tab LIKE object_tab.

  CLEAR ps_object_tab.

  MOVE-CORRESPONDING ps_iflo TO ps_object_tab.
*--- Konvertierungsexit berücksichtigen
  MOVE ps_iflo-tplnr  TO ps_object_tab-tplnr_int.
  MOVE ps_iflo-tplma  TO ps_object_tab-tplma_int.

  WRITE ps_iflo-tplnr TO ps_object_tab-tplnr.
  WRITE ps_iflo-tplma TO ps_object_tab-tplma.
  WRITE ps_iflo-proid TO ps_object_tab-proid.
*--- Hilfstabelle für Platzkennzeichung füllen
  APPEND ps_iflo-tplnr TO g_tplnr_tab2.

  IF NOT ps_object_tab-objnr IS INITIAL.
    l_jsto_pre_tab = ps_object_tab-objnr.
    APPEND l_jsto_pre_tab.
  ENDIF.

  ps_object_tab-ppsid = ps_iflo-ppsid.
  IF NOT ps_iflo-ppsid IS INITIAL.
    l_tarbid-mandt = ps_iflo-mandt.
    l_tarbid-objty = 'A '.
    l_tarbid-objid = ps_iflo-ppsid.
    COLLECT l_tarbid.
  ENDIF.

  ps_object_tab-igewrk = ps_iflo-lgwid.
  IF NOT ps_iflo-lgwid IS INITIAL.
    l_tarbid-mandt = ps_iflo-mandt.
    l_tarbid-objty = 'A '.
    l_tarbid-objid = ps_iflo-lgwid.
    COLLECT l_tarbid.
  ENDIF.

  IF NOT ps_iflo-adrnr IS INITIAL.
    g_adrnr_sel_tab-addrnumber = ps_iflo-adrnr.
    APPEND g_adrnr_sel_tab.
  ENDIF.

  IF NOT ps_iflo-submt IS INITIAL.
    g_matnr_tab-matnr = ps_iflo-submt.
    APPEND g_matnr_tab.
  ENDIF.

ENDFORM.                    "move_iflo_to_object_tab_l

*eject
*---------------------------------------------------------------------*
*       FORM FILL_OBJECT_TAB_L                                        *
*---------------------------------------------------------------------*
*       Statustexte und Arbeitsplätze nachlesen                       *
*---------------------------------------------------------------------*
FORM fill_object_tab_l.

  DATA: h_rihbgmd LIKE rihbgmd.

* DFPS Datendeklaration für BADI Aufruf
  DATA: lo_dfps_badi01 TYPE REF TO dfps_badi_pm_is_sap.

  FIELD-SYMBOLS: <ls_object_tab> LIKE LINE OF object_tab.

*--- nur weiter wenn object_tab gefüllt
  CHECK NOT object_tab[] IS INITIAL.
*--- Sortieren wegen binary search ----------------------------------*
  SORT g_fieldcat_tab BY fieldname.
*--- Statuszeile nachlesen ? ----------------------------------------*
  IF g_grstat IS INITIAL.
    PERFORM check_field_display_f14 USING 'STTXT' g_sttxt_flag.
    IF g_sttxt_flag <> yes.
      PERFORM check_field_display_f14 USING 'USTXT' g_sttxt_flag.
    ENDIF.
  ELSE.
    g_sttxt_flag = yes.
  ENDIF.
*--- Garantie nachlesen ?---------------------------------------------*
  PERFORM check_warranty_l USING g_mganr_flag.
*--- PPS-Arbeitsplatz anzeigen? ------------------------------------*
  PERFORM check_field_display_f14 USING 'ARBPL' g_arbpl_flag.
  IF g_crhd_flag = yes. g_arbpl_flag = yes. ENDIF.
*--- Verantw. Arbeitsplatz anzeigen ? -------------------------------*
  PERFORM check_field_display_f14 USING 'GEWRK' g_gewrk_flag.
  IF g_crhd_flag = yes. g_gewrk_flag = yes. ENDIF.
*--- Bezeichung Bautyp anzeigen? -----------------------------------*
  PERFORM check_field_display_f14 USING 'SUBMTKTX' g_submt_flag.
*--- zusätzliche Kennzeichungen ausgeben? --------------------------*
  PERFORM check_konvers_l USING g_konvr_flag.

  DESCRIBE TABLE stai1 LINES g_stai1_lines.
  DESCRIBE TABLE stae1 LINES g_stae1_lines.
  IF g_stai1_lines IS INITIAL AND
     g_stae1_lines IS INITIAL.
    g_stasl_flag = no.
  ELSE.
    IF g_stasl_flag <> ok.
      g_stasl_flag = yes.
    ENDIF.
  ENDIF.
*--- Adresse erforderlich ? ----------------------------------------
  PERFORM check_adress_sel_necc_17.

*--- wird report dunkel aufgerufen -> alle Flags setzten -----------*
  PERFORM check_flags_with_selmod_l.

* DFPS Start
  BREAK-POINT ID dfps_pm_is.
  TRY.
      GET BADI lo_dfps_badi01.

    CATCH cx_badi_not_implemented.                      "#EC NO_HANDLER
    CATCH cx_badi_multiply_implemented.                 "#EC NO_HANDLER
    CATCH cx_badi_initial_context.                      "#EC NO_HANDLER
  ENDTRY.

  IF NOT lo_dfps_badi01 IS INITIAL.
    TRY.
        CALL BADI lo_dfps_badi01->set_riiflo20_fld_flag
          CHANGING
            pt_fieldcat = g_fieldcat_tab[].

      CATCH cx_badi_initial_reference.                  "#EC NO_HANDLER
      CATCH cx_sy_dyn_call_illegal_method.              "#EC NO_HANDLER
    ENDTRY.
  ENDIF.
* DFPS End

  IF g_adres_flag = yes.
    PERFORM pre_read_adrnr_f17.
  ENDIF.

  IF g_arbpl_flag = yes OR g_gewrk_flag = yes.
    CALL FUNCTION 'CR_WORKCENTER_PRE_READ'
      TABLES
        tarbid = l_tarbid.
    FREE l_tarbid.
  ENDIF.

  IF g_stasl_flag = yes OR g_sttxt_flag = yes.
    CALL FUNCTION 'STATUS_PRE_READ'
      TABLES
        jsto_pre_tab = l_jsto_pre_tab.
  ENDIF.
*--- Materialbezeichnungen werden gelesen ----------------------------*
  IF g_submt_flag = yes.
    CALL FUNCTION 'IREP1_MATERIAL_TEXT_PRE_FETCH'
      TABLES
        matnr_tab     = g_matnr_tab
      EXCEPTIONS
        no_text_found = 1
        OTHERS        = 2.
  ENDIF.
*--- zusätzliche Platzkennzeichnungen lesen --------------------------*
  IF g_konvr_flag = yes.
    IF NOT g_tplnr_tab2[] IS INITIAL.
      CALL FUNCTION 'ILOX_IFLOS_READ_BY_TPLNR'
        EXPORTING
          it_tplnr  = g_tplnr_tab2
        IMPORTING
          et_iflos  = g_iflos_tab
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      SORT g_iflos_tab BY tplnr actvs alkey.
    ENDIF.
  ENDIF.
  IF g_mganr_flag = yes.
    CALL FUNCTION 'IREP1_WARRANTY_PRE_READ'
      TABLES
        ti_objnr         = l_jsto_pre_tab
      EXCEPTIONS
        no_data_selected = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      CLEAR g_mganr_flag.
    ENDIF.
  ENDIF.

* DFPS Start
  IF NOT lo_dfps_badi01 IS INITIAL.
    BREAK-POINT ID dfps_pm_is.
    TRY.
        CALL BADI lo_dfps_badi01->pre_fetch_riiflo20_data
          EXPORTING
            object_tab = object_tab[].

      CATCH cx_badi_initial_reference.                  "#EC NO_HANDLER
      CATCH cx_sy_dyn_call_illegal_method.              "#EC NO_HANDLER
    ENDTRY.
  ENDIF.
* DFPS End

  LOOP AT object_tab ASSIGNING <ls_object_tab>.
*--- Status prüfen ---------------------------------------------------*
    IF g_stasl_flag = yes.
      PERFORM status_proof_l USING <ls_object_tab>-objnr g_answer.
      IF g_answer = no.
        DELETE object_tab. CONTINUE.
      ENDIF.
    ENDIF.
*--- Statusleiste füllen ---------------------------------------------*
    IF g_sttxt_flag = yes AND NOT <ls_object_tab>-objnr IS INITIAL.
      CALL FUNCTION 'STATUS_TEXT_EDIT'
        EXPORTING
          objnr            = <ls_object_tab>-objnr
          spras            = sy-langu
          flg_user_stat    = 'X'
        IMPORTING
          line             = <ls_object_tab>-sttxt
          user_line        = <ls_object_tab>-ustxt
        EXCEPTIONS
          object_not_found = 01.
      IF sy-subrc <> 0.
        CLEAR <ls_object_tab>-sttxt.
        CLEAR <ls_object_tab>-ustxt.
      ENDIF.
    ENDIF.
*--- Externe Arbeitsplatznummer bestimmen ----------------------------*
    IF g_arbpl_flag = yes.
      IF NOT <ls_object_tab>-ppsid IS INITIAL.
        CALL FUNCTION 'CR_WORKSTATION_READ'
          EXPORTING
            id        = <ls_object_tab>-ppsid
          IMPORTING
            arbpl     = <ls_object_tab>-arbpl
          EXCEPTIONS
            not_found = 01.
        IF sy-subrc <> 0.
          CLEAR <ls_object_tab>-arbpl.
        ENDIF.
      ELSE.
        CLEAR <ls_object_tab>-arbpl.
      ENDIF.
      IF NOT arbpl[] IS INITIAL.
        IF NOT <ls_object_tab>-arbpl IN arbpl.
          DELETE object_tab. CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.
*--- Externes Leitgewerk bestimmen -----------------------------------*
    IF g_gewrk_flag = yes.
      IF NOT <ls_object_tab>-igewrk IS INITIAL.
        CALL FUNCTION 'CR_WORKSTATION_READ'
          EXPORTING
            id        = <ls_object_tab>-igewrk
          IMPORTING
            arbpl     = <ls_object_tab>-gewrk
          EXCEPTIONS
            not_found = 01.
        IF sy-subrc <> 0.
          CLEAR <ls_object_tab>-gewrk.
        ENDIF.
      ELSE.
        CLEAR <ls_object_tab>-gewrk.
      ENDIF.
      IF NOT gewrk[] IS INITIAL.
        IF NOT <ls_object_tab>-gewrk IN gewrk.
          DELETE object_tab. CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.
*--- Adresse bestimmen -----------------------------------------------*
    IF g_adres_flag = yes OR g_adres_flag = ok.
      IF NOT <ls_object_tab>-adrnr IS INITIAL.
        PERFORM get_adress_f17 USING <ls_object_tab>-adrnr
                                     <ls_object_tab>-tel_number
                                     <ls_object_tab>-name_list
                                     <ls_object_tab>-post_code1
                                     <ls_object_tab>-city1
                                     <ls_object_tab>-city2
                                     <ls_object_tab>-country
                                     <ls_object_tab>-region
                                     <ls_object_tab>-street.
      ENDIF.
    ENDIF.
*--- Bautypenbezeichung lesen
    IF NOT <ls_object_tab>-submt IS INITIAL AND g_submt_flag = yes.
      CALL FUNCTION 'IREP1_MATERIAL_TEXT_READ'
        EXPORTING
          i_matnr       = <ls_object_tab>-submt
        IMPORTING
          e_maktx       = <ls_object_tab>-submtktx
        EXCEPTIONS
          no_text_found = 1
          OTHERS        = 2.
      IF NOT sy-subrc IS INITIAL.
        CLEAR <ls_object_tab>-submtktx.
      ENDIF.
    ENDIF.
*--- Mustergantietext nachlesen -------------------------------------*
    IF g_mganr_flag = yes.
      CALL FUNCTION 'IREP1_WARRANTY_READ'
        EXPORTING
          i_objnr   = <ls_object_tab>-objnr
        IMPORTING
          e_rihbgmd = h_rihbgmd.
      MOVE-CORRESPONDING h_rihbgmd TO <ls_object_tab>.
    ENDIF.
    IF g_konvr_flag = yes.
      PERFORM fill_add_tplnrx_l CHANGING <ls_object_tab>.
    ENDIF.

* DFPS Start
    IF NOT lo_dfps_badi01 IS INITIAL.
      BREAK-POINT ID dfps_pm_is.
      TRY.
          CALL BADI lo_dfps_badi01->get_riiflo20_data
            CHANGING
              object_rec = <ls_object_tab>.

        CATCH cx_badi_initial_reference.                "#EC NO_HANDLER
        CATCH cx_sy_dyn_call_illegal_method.            "#EC NO_HANDLER
      ENDTRY.
    ENDIF.
* DFPS End

*--- fill additional field with BAdI BADI_EAM_SINGLELEVEL_LIST
    PERFORM fill_additional_fields_f16 USING 'RIHIFLO_LIST'
                                       CHANGING <ls_object_tab>.

  ENDLOOP.

ENDFORM.                    "fill_object_tab_l

*eject
*---------------------------------------------------------------------*
*       FORM STATUS_PROOF_L                                           *
*---------------------------------------------------------------------*
*       Statusbedingungen überprüfen                                  *
*---------------------------------------------------------------------*
*  -->  I_OBJNR object number
*  -->  F_ANSWER                                                      *
*---------------------------------------------------------------------*
FORM status_proof_l USING i_objnr  TYPE j_objnr
                          f_answer TYPE char01.

*### Datenvereinbarungen #############################################*
  DATA: BEGIN OF h_status_tab OCCURS 20.
          INCLUDE STRUCTURE jstat.
  DATA: END OF h_status_tab.

  DATA: BEGIN OF h_status_text_tab OCCURS 20,
          txt04 LIKE tj02t-txt04.
  DATA: END OF h_status_text_tab.
  DATA: h_stat_flag.

*### Verarbeitung ###################################################*
  IF i_objnr IS INITIAL.
    f_answer = no.
    EXIT.
  ENDIF.

  REFRESH: h_status_tab,
           h_status_text_tab.

  CALL FUNCTION 'STATUS_READ'
    EXPORTING
      objnr       = i_objnr
      only_active = 'X'
    TABLES
      status      = h_status_tab
    EXCEPTIONS
      OTHERS      = 01.

*--- Texte zur Tabelle besorgen -------------------------------------
  LOOP AT h_status_tab.

    CALL FUNCTION 'STATUS_NUMBER_CONVERSION'
      EXPORTING
        language      = sy-langu
        objnr         = i_objnr
        status_number = h_status_tab-stat
      IMPORTING
        txt04         = h_status_text_tab-txt04
      EXCEPTIONS
        OTHERS        = 01.
    IF sy-subrc = 0.
      APPEND h_status_text_tab.
    ENDIF.
  ENDLOOP.

  f_answer = no.

*--- 1. Status inclusiv ---------------------------------------------
  IF NOT g_stai1_lines IS INITIAL.
    h_stat_flag = ' '.
    LOOP AT h_status_text_tab.
      CHECK h_status_text_tab-txt04 IN stai1.
      h_stat_flag = 'X'.
      EXIT.
    ENDLOOP.
    IF h_stat_flag = ' '.
      EXIT.
    ENDIF.
  ENDIF.

*--- 1. Status exclusiv ---------------------------------------------
  IF NOT g_stae1_lines IS INITIAL.
    h_stat_flag = ' '.
    LOOP AT h_status_text_tab.
      CHECK h_status_text_tab-txt04 IN stae1.
      h_stat_flag = 'X'.
      EXIT.
    ENDLOOP.
    IF h_stat_flag = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  f_answer = yes.

ENDFORM.                    "status_proof_l

*eject
*---------------------------------------------------------------------*
*       FORM SUBMIT_L                                                 *
*---------------------------------------------------------------------*
*       Report mit allen bereits gesetzten Sel-opts wieder aufrufen   *
*---------------------------------------------------------------------*
FORM submit_l.

  SUBMIT riiflo20 VIA SELECTION-SCREEN
                 WITH  tplnr IN tplnr
                 WITH  pltxt IN pltxt
                 WITH  iwerk IN iwerk
                 WITH  kostl IN kostl
                 WITH  ingrp IN ingrp
                 WITH  submt IN submt
                 WITH  fltyp IN fltyp
                 WITH  swerk IN swerk
                 WITH  msgrp IN msgrp
                 WITH  rbnr  IN rbnr
                 WITH  stort IN stort
                 WITH  bukrs IN bukrs
                 WITH  trpnr IN trpnr
                 WITH  begru IN begru
                 WITH  abckz IN abckz
                 WITH  anlnr IN anlnr
                 WITH  kokrs IN kokrs
                 WITH  beber IN beber
                 WITH  gsber IN gsber
                 WITH  daufn IN daufn
                 WITH  erdat IN erdat
                 WITH  ernam IN ernam
                 WITH  aedat IN aedat
                 WITH  aenam IN aenam
                 WITH  eqfnr IN eqfnr
                 WITH  arbpl IN arbpl
                 WITH  gewrk IN gewrk
                 WITH  stai1 IN stai1
                 WITH  stae1 IN stae1
                 WITH  dy_selm  EQ dy_selm
                 WITH  dy_tcode EQ dy_tcode.

ENDFORM.                    "submit_l

*eject
*---------------------------------------------------------------------*
*       FORM AUTHORITY-CHECK_L                                        *
*---------------------------------------------------------------------*
*       Berechtigungen prüfen                                         *
*---------------------------------------------------------------------*
FORM authority_check_l.

*### Datenvereinbarungen #############################################*
  DATA: h_no_auth.
  DATA: f_tcode LIKE sy-tcode VALUE 'IL03'.
  Data: mtext type string. "N1597610

  FIELD-SYMBOLS: <ls_object_tab> LIKE LINE OF object_tab.

*### Verarbeitung ####################################################*
  h_no_auth = no.
  LOOP AT object_tab ASSIGNING <ls_object_tab>.
    CLEAR iflo.
    MOVE-CORRESPONDING <ls_object_tab> TO iflo.
    WRITE <ls_object_tab>-proid TO iflo-proid.
    CALL FUNCTION 'AUTHORITY_CHECK_IFLO'
      EXPORTING
        iflo_wa                        = iflo
        tcode                          = f_tcode
        i_tcode_check                  = ' '
      EXCEPTIONS
        no_authority_tcode             = 01
        no_authority_begrp             = 02
        no_authority_iwerk             = 03
        no_authority_ingrp             = 04
        no_authority_swerk             = 05
        no_authority_kostl             = 06
        no_profile_in_user_master_data = 07
        error_in_user_master_data      = 08
        no_authority_badi              = 09
        OTHERS                         = 10.

    IF sy-subrc <> 0.
      h_no_auth = yes.
      DELETE object_tab.
    ENDIF.

  ENDLOOP.

* start of note 1597610
  IF h_no_auth = yes AND dy_selm = 'D' AND NOT dy_msgty IS INITIAL.
    MESSAGE ID 'IH' TYPE dy_msgty NUMBER '046' INTO mtext.
  ELSEIF h_no_auth = yes AND NOT dy_msgty IS INITIAL.
    MESSAGE ID 'IH' TYPE dy_msgty NUMBER '046'.
  ENDIF.
* End of note 1597610

ENDFORM.                    "authority_check_l

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_NET1_L                                           *
*---------------------------------------------------------------------*
*       Verbindg. von/nach                                            *
*---------------------------------------------------------------------*
FORM display_net1_l.

*### Datenvereinbarungen #############################################*
  DATA: f_tcode LIKE sy-tcode.
  DATA: f_retc LIKE sy-subrc.

*### Verarbeitung ####################################################*
  f_tcode = 'IN16'.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING f_tcode
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
    PERFORM create_range_l.
    IF NOT object IS INITIAL.
      SUBMIT rinet0t0 WITH s_tpvon IN object
                      WITH dy_tcode =  f_tcode
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    "display_net1_l

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_NET2_L                                           *
*---------------------------------------------------------------------*
*       VerbindObjekt                                                 *
*---------------------------------------------------------------------*
FORM display_net2_l.

*### Datenvereinbarungen #############################################*
  DATA: f_tcode LIKE sy-tcode.
  DATA: f_retc LIKE sy-subrc.

*### Verarbeitung ####################################################*
  f_tcode = 'IN16'.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING f_tcode
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
    PERFORM create_range_l.
    IF NOT object IS INITIAL.
      SUBMIT rinet0t0 WITH s_tpkant IN object
                      WITH dy_tcode = f_tcode
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    "display_net2_l

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_QMEL_L                                           *
*---------------------------------------------------------------------*
*       Meldungen                                                     *
*---------------------------------------------------------------------*
FORM display_qmel_l.

*### Datenvereinbarungen #############################################*
  DATA: f_tcode LIKE sy-tcode.
  DATA: f_retc LIKE sy-subrc.

*### Verarbeitung ####################################################*
*### Wenn in Serviceliste (T.P) dann in Serviceliste (Meld) springen #*

  IF dy_tcode = 'IL20' OR dy_tcode = 'IH11'.
    f_tcode = 'IW59'.
  ELSE.
    f_tcode = 'IW29'.
  ENDIF.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING f_tcode
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
    PERFORM create_range_l.
    DESCRIBE TABLE object LINES sy-tfill.
    IF sy-tfill > 200.
*--- Zuviel für Selektion
      MESSAGE e103.
      EXIT.
    ELSEIF NOT object IS INITIAL.
      EXPORT f_tcode TO MEMORY ID 'RIQMEL20'.
      SUBMIT riqmel20 VIA SELECTION-SCREEN
                      WITH tplnr IN object
                      WITH dy_ofn   = 'X'
                      WITH dy_rst   = 'X'
                      WITH dy_iar   = 'X'
                      WITH dy_mab   = 'X'
                      WITH dy_tcode = f_tcode
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    "display_qmel_l

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_AUFK_L                                           *
*---------------------------------------------------------------------*
*       Aufträge                                                      *
*---------------------------------------------------------------------*
FORM display_aufk_l.

*### Datenvereinbarungen #############################################*
  DATA: f_tcode LIKE sy-tcode.
  DATA: f_retc LIKE sy-subrc.

*### Verarbeitung ####################################################*
*### Wenn in Serviceliste (T.P) dann in Serviceliste (Auft) springen #*

  IF dy_tcode = 'IL20' OR dy_tcode = 'IH11'.
    f_tcode = 'IW73'.
  ELSE.
    f_tcode = 'IW39'.
  ENDIF.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING f_tcode
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
    PERFORM create_range_l.
    DESCRIBE TABLE object LINES sy-tfill.
    IF sy-tfill > 200.
*--- Zuviel für Selektion
      MESSAGE e103.
      EXIT.
    ELSEIF NOT object IS INITIAL.
      EXPORT f_tcode TO MEMORY ID 'RIAUFK20'.
      SUBMIT riaufk20 VIA SELECTION-SCREEN
                      WITH tplnr IN object
                      WITH dy_ofn   = 'X'
                      WITH dy_iar   = 'X'
                      WITH dy_mab   = 'X'
                      WITH dy_his   = 'X'
                      WITH dy_tcode = f_tcode
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    "display_aufk_l

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_MPTS_L                                           *
*---------------------------------------------------------------------*
*       Meßpunkte                                                     *
*---------------------------------------------------------------------*
FORM display_mpts_l.

*### Datenvereinbarungen #############################################*
  DATA: f_tcode LIKE sy-tcode.
  DATA: f_retc LIKE sy-subrc.

*### Verarbeitung ####################################################*
  f_tcode = 'IK07'.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING f_tcode
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
    PERFORM create_range_l.
    IF NOT object IS INITIAL.
      SUBMIT riimpt20 WITH tplnr IN object
                      WITH dy_tcode = f_tcode
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    "display_mpts_l
*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_MHIS_L                                           *
*---------------------------------------------------------------------*
*       Wartungsplanvorschau                                          *
*---------------------------------------------------------------------*
FORM display_mhis_l.

*### Datenvereinbarungen #############################################*
  DATA: f_retc LIKE sy-subrc.

*### Verarbeitung ####################################################*

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING 'IP19'
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
    PERFORM create_range_l.
    DESCRIBE TABLE object LINES sy-tfill.
    IF sy-tfill > 200.
*--- Zuviel für Selektion
      MESSAGE e103.
      EXIT.
    ELSEIF NOT object IS INITIAL.

      SUBMIT rimhis00 WITH tplnr IN object
             VIA SELECTION-SCREEN
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    "display_mhis_l

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_MPOS_L                                           *
*---------------------------------------------------------------------*
*       Wartungspositionen                                            *
*---------------------------------------------------------------------*
FORM display_mpos_l.

*### Datenvereinbarungen #############################################*
  DATA: f_tcode LIKE sy-tcode.
  DATA: f_retc LIKE sy-subrc.

*### Verarbeitung ####################################################*
  f_tcode = 'IP18'.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING f_tcode
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
    PERFORM create_range_l.
    DESCRIBE TABLE object LINES sy-tfill.
    IF sy-tfill > 200.
*--- Zuviel für Selektion
      MESSAGE e103.
      EXIT.
    ELSEIF NOT object IS INITIAL.
      SUBMIT rimpos00 VIA SELECTION-SCREEN
                      WITH tplnr IN object
                      WITH dy_tcode = f_tcode
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    "display_mpos_l
*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_VERT_L                                           *
*---------------------------------------------------------------------*
*       Verträge                                                      *
*---------------------------------------------------------------------*
FORM display_vert_l.

*### Datenvereinbarungen #############################################*
  DATA: f_tcode LIKE sy-tcode.
  DATA: f_retc LIKE sy-subrc.

*### Verarbeitung ####################################################*
  f_tcode = 'IW75'.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING f_tcode
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
    PERFORM create_range_l.
    IF NOT object IS INITIAL.
      SUBMIT riveda20 WITH tplnr IN object
                      WITH dy_tcode = f_tcode
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    "display_vert_l

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_EQUI_L                                           *
*---------------------------------------------------------------------*
*       Equipments                                                    *
*---------------------------------------------------------------------*
FORM display_equi_l.

*### Datenvereinbarungen #############################################*
  DATA: f_tcode LIKE sy-tcode.
  DATA: f_retc LIKE sy-subrc.

*### Verarbeitung ####################################################*
*### Wenn in Serviceliste (T.P) dann in Serviceliste (Equi) springen #*

  IF dy_tcode = 'IL20' OR dy_tcode = 'IH11'.
    f_tcode = 'IH10'.
  ELSE.
    f_tcode = 'IH08'.
  ENDIF.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING f_tcode
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
    PERFORM create_range_l.
    DESCRIBE TABLE object LINES sy-tfill.
    IF sy-tfill > 200.
*--- Zuviel für Selektion
      MESSAGE e103.
      EXIT.
    ELSEIF NOT object IS INITIAL.
      EXPORT f_tcode TO MEMORY ID 'RIEQUI20'.
      SUBMIT riequi20 WITH tplnr IN object
                      WITH dy_tcode = f_tcode
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    "display_equi_l

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_PLAN_L                                           *
*---------------------------------------------------------------------*
*       Arbeitsanleitungen                                            *
*---------------------------------------------------------------------*
FORM display_plan_l.

*### Datenvereinbarungen #############################################*
  DATA: f_tcode LIKE sy-tcode.
  DATA: f_retc LIKE sy-subrc.

*### Verarbeitung ####################################################*
  f_tcode = 'IA09'.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING f_tcode
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
    PERFORM create_range_submt_l.
    IF NOT r_submt IS INITIAL.
      SUBMIT riplko10 WITH istru IN r_submt
                      WITH pn_equi = ' '
                      WITH pn_iflo = ' '
                      WITH pn_ihan = 'X'
                      WITH dy_tcode = f_tcode
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    "display_plan_l

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_TPPL_L                                           *
*---------------------------------------------------------------------*
*       Arbeitsanleitungen zum Platz                                  *
*---------------------------------------------------------------------*
FORM display_tppl_l.

*### Datenvereinbarungen #############################################*
  DATA: f_tcode LIKE sy-tcode.
  DATA: f_retc LIKE sy-subrc.

*### Verarbeitung ####################################################*
  f_tcode = 'IA09'.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING f_tcode
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
    PERFORM create_range_l.
    IF NOT object IS INITIAL.
      SUBMIT riplko10 WITH pn_tplnr IN object
                      WITH pn_equi = ' '
                      WITH pn_iflo = 'X'
                      WITH pn_ihan = ' '
                      WITH dy_tcode = f_tcode
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    "display_tppl_l

*eject
*---------------------------------------------------------------------*
*       FORM CREATE_RANGE_L                                           *
*---------------------------------------------------------------------*
*       Range mit selektierten Objekten erstellen                     *
*---------------------------------------------------------------------*
FORM create_range_l.

  CLEAR object.
  REFRESH object.
  LOOP AT object_tab WHERE selected = g_x.
    PERFORM mark_selected_f16 CHANGING object_tab-selected
                                       object_tab-pm_selected.
    CLEAR object.
    object-option = 'EQ'.
    object-sign   = 'I'.
    MOVE  object_tab-tplnr_int TO object-low.
*   write object_tab-tplnr_int to object-low.
    APPEND object.
  ENDLOOP.
  IF object IS INITIAL.
    MESSAGE i011.
  ENDIF.

ENDFORM.                    "create_range_l

*eject
*---------------------------------------------------------------------*
*       FORM CREATE_RANGE_SUBMT_L                                     *
*---------------------------------------------------------------------*
*       Range mit selektierten Objekten erstellen                     *
*---------------------------------------------------------------------*
FORM create_range_submt_l.

  CLEAR r_submt.
  REFRESH r_submt.
  LOOP AT object_tab WHERE selected = g_x.
    PERFORM mark_selected_f16 CHANGING object_tab-selected
                                       object_tab-pm_selected.
    IF NOT object_tab-submt IS INITIAL.
      CLEAR r_submt.
      r_submt-option =  'EQ'.
      r_submt-sign   = 'I'.
      r_submt-low    = object_tab-submt.
      COLLECT r_submt.
    ENDIF.
  ENDLOOP.
  IF r_submt IS INITIAL.
    MESSAGE s047.
  ENDIF.


ENDFORM.                    "create_range_submt_l

*eject
*---------------------------------------------------------------------*
*       FORM MULTI_OBJECT_l                                           *
*---------------------------------------------------------------------*
*       Equipments mehrstufig                                         *
*---------------------------------------------------------------------*
FORM multi_object_l.

  DATA: f_retc LIKE sy-subrc.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING 'IL07'
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
    PERFORM create_range_l.
    IF NOT object IS INITIAL.
      SUBMIT riiflo30 WITH tplnr IN object
             VIA SELECTION-SCREEN
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    "multi_object_l

*eject
*---------------------------------------------------------------------*
*       FORM MULTI_QMEL_l                                             *
*---------------------------------------------------------------------*
*       Meldungen mehrstufig                                          *
*---------------------------------------------------------------------*
FORM multi_qmel_l.

  DATA: f_retc LIKE sy-subrc.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING 'IW30'
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
    PERFORM create_range_l.
    DESCRIBE TABLE object LINES sy-tfill.
    IF sy-tfill > 200.
*--- Zuviel für Selektion
      MESSAGE e103.
      EXIT.
    ELSEIF NOT object IS INITIAL.
      SUBMIT riqmel10 WITH tplnr IN object
             VIA SELECTION-SCREEN
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    "multi_qmel_l

*---------------------------------------------------------------------*
*       FORM MULTI_AUFK_l                                             *
*---------------------------------------------------------------------*
*       Aufträge mehrstufig                                           *
*---------------------------------------------------------------------*
FORM multi_aufk_l.

  DATA: f_retc LIKE sy-subrc.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING 'IW40'
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
    PERFORM create_range_l.
    DESCRIBE TABLE object LINES sy-tfill.
    IF sy-tfill > 200.
*--- Zuviel für Selektion
      MESSAGE e103.
      EXIT.
    ELSEIF NOT object IS INITIAL.
      SUBMIT riaufk10 WITH tplnr IN object
             VIA SELECTION-SCREEN
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    "multi_aufk_l


*&---------------------------------------------------------------------*
*&      Form  EXECUTE_TCODE_L
*&---------------------------------------------------------------------*
*       Meldung/Auftrag aus T.P-liste anlegen                          *
*----------------------------------------------------------------------*
FORM execute_tcode_l USING f_ucomm LIKE sy-ucomm
                           f_selfield TYPE slis_selfield.

  DATA h_tcode LIKE sy-tcode.

  h_tcode = f_ucomm.

  PERFORM check_object_tab_marked_f14 USING f_ucomm
                                            f_selfield.

*--- für mehrere objecte können Meldungen angelegt werden ------------*
  LOOP AT object_tab WHERE selected = g_x.
    PERFORM mark_selected_f16 CHANGING object_tab-selected
                                       object_tab-pm_selected.
*--- ins Anlegen Aufträge oder Meldungen springen --------------------*
    IF h_tcode = 'IW31'.
      PERFORM create_orde_l.
    ELSE.
      PERFORM create_qmel_l USING h_tcode.
    ENDIF.
  ENDLOOP.
*--- Es wurde kein object markiert/ausgewählt  -----------------------*
  IF NOT sy-subrc IS INITIAL.
    MESSAGE s011.
  ENDIF.

ENDFORM.                               " EXECUTE_TCODE_QMEL_L

*&---------------------------------------------------------------------*
*&      Form  CREATE_QMEL_L using f_tcode
*&---------------------------------------------------------------------*
*       Eine Meldung wird angelegt                                     *
*----------------------------------------------------------------------*
*  -->  f_tcode   Transaction die aufgerufen wird
*----------------------------------------------------------------------*
FORM create_qmel_l USING f_tcode LIKE sy-tcode.

  TABLES t370f.

  DATA: f_tcode_l LIKE sy-tcode.
  DATA: f_retc LIKE sy-subrc.

  SET PARAMETER ID 'IFL' FIELD object_tab-tplnr_int.
  SET PARAMETER ID 'EQN' FIELD space.
  SET PARAMETER ID 'MAT' FIELD space.

  f_tcode_l =  f_tcode.

*--- FCODE verbiegen bei Service wird aus IW24 -> IW54            --*
*---                             und aus  IW25 -> IW55            --*
*--- Kriterium ist der T.P.-typ  (Vertriebsrelevant ja/nein)      --*

  SELECT SINGLE * FROM t370f WHERE fltyp = object_tab-fltyp.
  IF t370f-sales = 'X'.
    CASE f_tcode_l.
      WHEN 'IW21'.
        f_tcode_l = 'IW51'.
      WHEN 'IW24'.
        f_tcode_l = 'IW54'.
      WHEN 'IW25'.
        f_tcode_l = 'IW55'.
      WHEN 'IW26'.
        f_tcode_l = 'IW56'.
    ENDCASE.
  ENDIF.
*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING f_tcode_l
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
    CALL TRANSACTION f_tcode_l.
  ENDIF.

ENDFORM.                               " CREATE_QMEL_L

*---------------------------------------------------------------------*
*       FORM CREATE_ORDE_L                                            *
*---------------------------------------------------------------------*
*       IH-Aufträge anlegen                                           *
*---------------------------------------------------------------------*
FORM create_orde_l.

  DATA: f_retc LIKE sy-subrc.

  SET PARAMETER ID 'IFL' FIELD object_tab-tplnr_int.
  SET PARAMETER ID 'EQN' FIELD space.
  SET PARAMETER ID 'MAT' FIELD space.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM auth_check_tcode_f16 USING 'IW31'
                               CHANGING f_retc.
  IF f_retc IS INITIAL.
*--- IW31 aufrufen                                              -----*
    CALL TRANSACTION 'IW31'.
  ENDIF.

ENDFORM.                    "create_orde_l

*---------------------------------------------------------------------*
*       FORM GET_TABLE_L                                              *
*---------------------------------------------------------------------*
*       Tabelle für Klassifizierung                                  *
*---------------------------------------------------------------------*
*  -->  F_TABLE                                                       *
*---------------------------------------------------------------------*
FORM get_table_l USING f_table LIKE dfies-tabname.

  f_table = 'IFLOT'.

ENDFORM.                    "get_table_l

*---------------------------------------------------------------------*
*       FORM GET_OBJECT_L                                             *
*---------------------------------------------------------------------*
*       Objekt für Klassifizierung setzen                             *
*---------------------------------------------------------------------*
*  -->  F_OBJECT                                                      *
*---------------------------------------------------------------------*
FORM get_object_l USING f_object LIKE ausp-objek.

  DATA: BEGIN OF h_object1,
         tplnr LIKE iflo-tplnr,
        END OF h_object1.

  h_object1-tplnr = object_tab-tplnr_int.
  f_object = h_object1.

ENDFORM.                    "get_object_l


*&---------------------------------------------------------------------*
*&      Form  DETERMINE_ACTTYPE_IFLO_L
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM determine_acttype_iflo_l.

  SELECT SINGLE * FROM t370a WHERE tcode = g_tcode.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM t370a WHERE tcode = 'IH06'.
    IF sy-subrc <> 0.
      MESSAGE x160 WITH 'IH06'.
    ENDIF.
  ENDIF.
  g_aktyp = t370a-aktyp.

ENDFORM.                               " DETERMINE_ACTTYPE_IFLO_L
*&---------------------------------------------------------------------*
*&      Form  GET_TPLNR_FROM_IHPA_L
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_tplnr_from_ihpa_l.

  DATA: BEGIN OF h_objnr_tab OCCURS 0,
          objnr LIKE aufk-objnr,
        END OF h_objnr_tab.

  DATA: BEGIN OF h_ionra.
          INCLUDE STRUCTURE ionra.
  DATA: END OF h_ionra.

  RANGES: lr_tplnr FOR iflot-tplnr.

*--- sichern der Technischen Plätze
  lr_tplnr[] = tplnr[].
  REFRESH tplnr. CLEAR tplnr.
  tplnr-sign   = 'I'.
  tplnr-option = 'EQ'.

  SELECT objnr FROM ihpa INTO TABLE h_objnr_tab
                     WHERE parnr = dy_parnr
                     AND   parvw = dy_parvw
                     AND   obtyp = 'IFL'
                     AND kzloesch = ' '.

  LOOP AT h_objnr_tab.
*--- aus der Objectnummer wird die Equinummer ermittelt -------------*
    CALL FUNCTION 'OBJECT_KEY_GET'
      EXPORTING
        i_objnr = h_objnr_tab-objnr
      IMPORTING
        e_ionra = h_ionra
      EXCEPTIONS
        OTHERS  = 1.
    IF sy-subrc IS INITIAL.
*--- Selektionsergebnis mit select option abmischen -----------------*
      CHECK h_ionra-tplnr IN lr_tplnr.
      tplnr-low = h_ionra-tplnr.
      APPEND tplnr.
    ENDIF.
  ENDLOOP.

  IF tplnr[] IS INITIAL.
*--- es wurde nicht selektiert
    MESSAGE s047.
    STOP.
  ENDIF.

ENDFORM.                               " GET_TPLNR_FROM_IHPA_L

*---------------------------------------------------------------------*
*       FORM DISPLAY_LONGTEXT_L       new with  P30K093741            *
*---------------------------------------------------------------------*
*       Langtextanzeige aus liste                                     *
*---------------------------------------------------------------------*
FORM display_longtext_l.

  DATA h_object LIKE  ttxob-tdobject.
  DATA h_object_nr LIKE  rm63t-objnr.
  DATA h_spras     LIKE  sy-langu.
  DATA h_txtid LIKE  ttxid-tdid.
  DATA h_count_inline.
  DATA h_save_mode.
  DATA h_text_history.
  DATA h_text_property.

  h_object    = 'IFLOT'.
  h_object_nr = object_tab-tplnr_int.
  h_spras     = object_tab-spras.
  h_txtid     = 'LTXT'.

  PERFORM display_longtext_f16 USING h_object
                                     h_object_nr
                                     h_spras
                                     h_txtid
                                     h_count_inline
                                     h_save_mode
                                     h_text_history
                                     h_text_property.

ENDFORM.                    "display_longtext_l
*&---------------------------------------------------------------------*
*&      Form  CHECK_FLAGS_WITH_SELMOD_L
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_flags_with_selmod_l.

*--- wurde Feldcatalog importiert, Flags nicht automatisch setzen
  CHECK g_fieldcat_imp IS INITIAL.

  IF g_selmod = selmod_d.
    g_sttxt_flag = yes.
    g_arbpl_flag = yes.
    g_gewrk_flag = yes.
    g_adres_flag = yes.
    g_submt_flag = yes.
    g_konvr_flag = yes.
  ENDIF.

ENDFORM.                               " CHECK_FLAGS_WITH_SELMOD_L

*&---------------------------------------------------------------------*
*&      Form  VARIANT_INIT_L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM variant_init_l.
*--- Liste im PM oder SM aufgerufen ? ------------------------------*
  CASE g_tcode.
    WHEN 'IL05'.
      PERFORM variant_init_f14 USING 'INST'
                                     'INST'
                                     'INST'.
    WHEN 'IH06'.
      PERFORM variant_init_f14 USING 'INST'
                                     'INST'
                                     'INST'.
    WHEN 'IL20'.
      PERFORM variant_init_f14 USING 'SERV'
                                     'INST'
                                     'INST'.
    WHEN 'IH11'.
      PERFORM variant_init_f14 USING 'SERV'
                                     'INST'
                                     'INST'.
    WHEN OTHERS.
      PERFORM variant_init_f14 USING 'INST'
                                     'INST'
                                     'INST'.
  ENDCASE.

ENDFORM.                               " VARIANT_INIT_L
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELDGROUPS_L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_fieldgroups_l.

  DATA h_fieldgroups TYPE slis_sp_group_alv.

  FIELD-SYMBOLS: <ls_fieldcat> TYPE slis_fieldcat_alv.

*--- create fieldgroups ---------------------------------------------*
*--- Instandhaltungsdaten
  h_fieldgroups-sp_group = 'A'.
  h_fieldgroups-text     = text-fg1.
  APPEND h_fieldgroups TO g_fieldgroups_tab.
*--- Standort und Kontierung ----------------------------------------*
  h_fieldgroups-sp_group = 'B'.
  h_fieldgroups-text     = text-fg2.
  APPEND h_fieldgroups TO g_fieldgroups_tab.
*--- adressdaten ----------------------------------------------------*
  h_fieldgroups-sp_group = 'C'.
  h_fieldgroups-text     = text-fg3.
  APPEND h_fieldgroups TO g_fieldgroups_tab.
*--- kennzeichung technischer platz----------------------------------*
  h_fieldgroups-sp_group = 'D'.
  h_fieldgroups-text     = text-fg4.
  APPEND h_fieldgroups TO g_fieldgroups_tab.
*--- Allegmeine Daten -----------------------------------------------*
  h_fieldgroups-sp_group = 'E'.
  h_fieldgroups-text     = text-fg5.
  APPEND h_fieldgroups TO g_fieldgroups_tab.
*--- Garantiedaten --------------------------------------------------*
  h_fieldgroups-sp_group = 'G'.
  h_fieldgroups-text     = text-fg6.
  APPEND h_fieldgroups TO g_fieldgroups_tab.
*--- EAML: Linear Feature Extension  'L' ----------------------------*
  IF cl_ops_switch_check=>eam_sfws_lfe( ) IS NOT INITIAL. "EhP605 EAML
    cl_eaml_reporting=>add_fieldgroups(
                         EXPORTING iv_report      = sy-repid
                                   iv_tcode       = sy-tcode
                         CHANGING  ct_fieldgroups = g_fieldgroups_tab ).
  ENDIF.
*--- assign fields in g_fieldcat_tab to fieldgroups -----------------*
  LOOP AT g_fieldcat_tab ASSIGNING <ls_fieldcat>.
    CLEAR <ls_fieldcat>-sp_group.
    CASE <ls_fieldcat>-fieldname.
      WHEN 'MANDT'.
        <ls_fieldcat>-sp_group = 'A'.
        <ls_fieldcat>-tech     = g_x.
      WHEN 'TPLNR'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'PLTXT'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'FLTYP'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'BEGRU'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'USTXT'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'STTXT'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'SPRAS'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'OBJNR'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'SUBMT'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'SUBMTI'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'SUBMTKTX'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'IWERK'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'IWERKI'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'INGRP'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'INGRPI'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'GEWRK'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'LGWIDI'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'RBNR'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'RBNR_I'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'ERDAT'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'ERNAM'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'AEDAT'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'AENAM'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'TRPNR'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'TPLKZ'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'TPLMA'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'DATAB'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'IEQUI'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'IEQUII'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'EINZL'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'EINZLI'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'POSNR'.
        <ls_fieldcat>-sp_group = 'A'.
      WHEN 'KZLTX'.
        <ls_fieldcat>-sp_group = 'A'.

      WHEN 'SWERK'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'SWERKI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'STORT'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'STORTI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'MSGRP'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'MSGRPI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'BEBER'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'BEBERI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'ARBPL'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'PPSIDI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'ABCKZ'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'ABCKZI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'EQFNR'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'EQFNRI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'BUKRS'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'BUKRSI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'KOKRS'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'KOKRSI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'ANLNR'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'ANLNRI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'ANLUN'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'ANLUNI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'PROID'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'PROIDI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'DAUFN'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'DAUFNI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'AUFNR'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'AUFNRI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'GSBER'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'GSBERI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'KOSTL'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'KOSTLI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'ILOAN'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'VKORG'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'VKORGI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'VTWEG'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'VTWEGI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'SPART'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'SPARTI'.
        <ls_fieldcat>-sp_group = 'B'.
      WHEN 'RKEOBJNR'.
        <ls_fieldcat>-sp_group = 'B'.

      WHEN 'ADRNR'.
        <ls_fieldcat>-sp_group = 'C'.
      WHEN 'ADRNRI'.
        <ls_fieldcat>-sp_group = 'C'.
      WHEN 'NAME_LIST'.
        <ls_fieldcat>-sp_group = 'C'.
      WHEN 'TEL_NUMBER'.
        <ls_fieldcat>-sp_group = 'C'.
      WHEN 'POST_CODE1'.
        <ls_fieldcat>-sp_group = 'C'.
      WHEN 'CITY1'.
        <ls_fieldcat>-sp_group = 'C'.
      WHEN 'CITY2'.
        <ls_fieldcat>-sp_group = 'C'.
      WHEN 'COUNTRY'.
        <ls_fieldcat>-sp_group = 'C'.
      WHEN 'REGION'.
        <ls_fieldcat>-sp_group = 'C'.
      WHEN 'STREET'.
        <ls_fieldcat>-sp_group = 'C'.

      WHEN 'TPLNR_0'.
        <ls_fieldcat>-sp_group = 'D'.
        PERFORM get_konvr_text_l USING '0' <ls_fieldcat>.
      WHEN 'TPLNR_1'.
        <ls_fieldcat>-sp_group = 'D'.
        PERFORM get_konvr_text_l USING '1' <ls_fieldcat>.
      WHEN 'TPLNR_2'.
        <ls_fieldcat>-sp_group = 'D'.
        PERFORM get_konvr_text_l USING '2' <ls_fieldcat>.
      WHEN 'TPLNR_3'.
        <ls_fieldcat>-sp_group = 'D'.
        PERFORM get_konvr_text_l USING '3' <ls_fieldcat>.
      WHEN 'ALKEY'.
        <ls_fieldcat>-sp_group = 'D'.

      WHEN 'EQART'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'INVNR'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'BRGEW'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'GEWEI'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'GROES'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'ANSWT'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'WAERS'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'ANSDT'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'HERST'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'HERLD'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'TYPBZ'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'BAUJJ'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'BAUMM'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'SERGE'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'VKBUR'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'VKGRP'.
        <ls_fieldcat>-sp_group = 'E'.
      WHEN 'MAPAR'.
        <ls_fieldcat>-sp_group = 'E'.

      WHEN 'GWLDT_K'.
        <ls_fieldcat>-sp_group = 'G'.
      WHEN 'GWLEN_K'.
        <ls_fieldcat>-sp_group = 'G'.
      WHEN 'MGANR_K'.
        <ls_fieldcat>-sp_group = 'G'.
      WHEN 'GARTX_K'.
        <ls_fieldcat>-sp_group = 'G'.
      WHEN 'GWLDT_L'.
        <ls_fieldcat>-sp_group = 'G'.
      WHEN 'GWLEN_L'.
        <ls_fieldcat>-sp_group = 'G'.
      WHEN 'MGANR_L'.
        <ls_fieldcat>-sp_group = 'G'.
      WHEN 'GARTX_L'.
        <ls_fieldcat>-sp_group = 'G'.
*--- Fields, not displayed on list
      WHEN 'MAPARI'.
        <ls_fieldcat>-tech     = g_x.
      WHEN 'LVORM'.
        <ls_fieldcat>-tech     = g_x.
      WHEN 'TPLNR_INT'.
        <ls_fieldcat>-tech     = g_x.
      WHEN OTHERS.

*--- Process additional fields with BAdI BADI_EAM_SINGLELEVEL_LIST
        PERFORM create_fieldgroups_badi_f14 CHANGING <ls_fieldcat>
                                                     g_fieldgroups_tab.

        IF cl_ops_switch_check=>eam_sfws_lfe( ) IS NOT INITIAL. "EHP605 EAML
          "EAML fields are only in field catalog if switch is on
          cl_eaml_reporting=>set_fieldgroup( CHANGING cs_fieldcat = <ls_fieldcat> ).
        ENDIF.

    ENDCASE.
  ENDLOOP.

ENDFORM.                               " CREATE_FIELDGROUPS_L

*&---------------------------------------------------------------------*
*&      Form  CHECK_FIELDCAT_VARIANT_L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_fieldcat_variant_l.

  DATA h_index       LIKE sy-tabix.

  FIELD-SYMBOLS: <ls_fieldcat> TYPE slis_fieldcat_alv.

  h_index = 1.

  DESCRIBE TABLE g_selfields_tab LINES sy-tabix.
  IF sy-tabix IS INITIAL.
    LOOP AT g_fieldcat_tab ASSIGNING <ls_fieldcat>.
      CASE <ls_fieldcat>-fieldname.
        WHEN 'PM_SELECTED'.
          <ls_fieldcat>-no_out  = space.
          <ls_fieldcat>-col_pos = 1.
        WHEN 'TPLNR'.
          <ls_fieldcat>-no_out  = space.
          <ls_fieldcat>-col_pos = 2.
        WHEN 'PLTXT'.
          <ls_fieldcat>-no_out  = space.
          <ls_fieldcat>-col_pos = 3.
        WHEN 'IWERK'.
          <ls_fieldcat>-no_out = space.
          <ls_fieldcat>-col_pos = 4.
        WHEN OTHERS.
          <ls_fieldcat>-no_out = g_x.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDFORM.                               " CHECK_FIELDCAT_VARIANT_L
*&---------------------------------------------------------------------*
*&      Form  REFRESH_L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_l USING p_selfield TYPE slis_selfield.

  DATA h_change LIKE sy-ucomm.
* EAM CC 200                                                "1656798
  data: lv_eam_cc_processed type xfeld.                     "1656798

  IF p_selfield-before_action = g_x.
*--- interaktion vor SALV-Aktion (Klassifizierung ausblenden) ------
    IF g_class_on = yes.
* begin of note 1656798
* EAM CC 200

*     PERFORM classification_off_f50. "(old form call)
      clear lv_eam_cc_processed.
      include eamcc_riiflo20_41 if found.

       if lv_eam_cc_processed is INITIAL.
        PERFORM classification_off_f50.
       endif.
* end of note 1656798

      g_class_on = yes.
    ENDIF.
  ELSEIF p_selfield-after_action = g_x.
*--- interaktion nach SALV-Aktion (Klassifizierung wieder einblenden)
    PERFORM get_fieldcat_actual_f14 USING h_change.
*--- Nachselektion nur wenn Feldkatalog verändert -------------------
    IF h_change = yes.
      PERFORM fill_object_tab_l.
    ENDIF.
    IF g_class_on = yes.
* begin of note 1656798
* EAM CC 200
*     PERFORM classification_on_f50. (old form call)
      clear lv_eam_cc_processed.
      include eamcc_riiflo20_42 if found.

       if lv_eam_cc_processed is INITIAL.
        PERFORM classification_on_f50.
       endif.
* end of note 1656798

    ENDIF.
  ENDIF.
*--- liste neu sortiert ausgeben
  p_selfield-refresh = g_x.

ENDFORM.                               " REFRESH_L
*---------------------------------------------------------------------*
*       FORM GET_PHONE_DATA_L                                         *
*---------------------------------------------------------------------*
*       Supply Phone number for outgoing call                         *
*---------------------------------------------------------------------*
*  -->  P_SELFIELD                                                    *
*---------------------------------------------------------------------*
FORM get_phone_data_l USING p_selfield TYPE slis_selfield.

  CASE p_selfield-sel_tab_field.
    WHEN trans_struc-ernam.
      PERFORM get_user_f16 USING object_tab-ernam.
    WHEN trans_struc-aenam.
      PERFORM get_user_f16 USING object_tab-aenam.
  ENDCASE.

ENDFORM.                    "get_phone_data_l
*&---------------------------------------------------------------------*
*&      Form  FILL_TPLNR_FROM_SOGEN_L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_tplnr_from_sogen_l.

  DATA    h_ionra LIKE ionra.
  RANGES  h_tplnr FOR iflo-tplnr.

*--- Über Genehmigungen Aufträge gefunden ?
  CHECK NOT g_sogen_object[] IS INITIAL.

*--- Aktuelle Selektionseinschränkung sichern
  h_tplnr[] = tplnr[].

  REFRESH tplnr.
  CLEAR   tplnr.
  tplnr-sign = 'I'.
  tplnr-option = 'EQ'.

*--- Select-option aufbauen, mit vorh. Einschränkung abmischen
  LOOP AT g_sogen_object.
    CALL FUNCTION 'OBJECT_KEY_GET'
      EXPORTING
        i_objnr = g_sogen_object-objnr
      IMPORTING
        e_ionra = h_ionra
      EXCEPTIONS
        OTHERS  = 1.
    CHECK sy-subrc IS INITIAL AND h_ionra-tplnr IN h_tplnr.
    tplnr-low = h_ionra-tplnr.
    APPEND tplnr.
  ENDLOOP.
  IF tplnr[] IS INITIAL.
*--- es wurde nicht selektiert
    MESSAGE s047.
    STOP.
  ENDIF.

ENDFORM.                               " FILL_TPLNR_FROM_SOGEN_L
*&---------------------------------------------------------------------*
*&      Form  SET_PROFILE_STRNO_L
*&---------------------------------------------------------------------*
*       Einstellung Kennzeichung Technischer Platz ändern
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_profile_strno_l USING p_selfield TYPE slis_selfield.

  DATA h_iflousrd    LIKE iflousrd.
  DATA h_flg_changes LIKE iref-iind.

  CALL FUNCTION 'ILOY_FUNC_LOC_USR_PROF_DIALOG'
    IMPORTING
      e_iflousrd    = h_iflousrd
      e_flg_changes = h_flg_changes.
*--- Wenn Einstellung geändert Liste neu ausgeben wegen Konvertierung
  IF NOT h_flg_changes IS INITIAL.
    LOOP AT object_tab.
*--- Konvertierung
      WRITE object_tab-tplnr_int TO object_tab-tplnr.
      WRITE object_tab-tplma_int TO object_tab-tplma.
      MODIFY object_tab.
    ENDLOOP.
  ENDIF.

ENDFORM.                               " SET_PROFILE_STRNO_L
*&---------------------------------------------------------------------*
*&      Form  GET_KONVR_TEXT_L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*
*----------------------------------------------------------------------*
FORM get_konvr_text_l USING p_key         LIKE iflos-alkey
                            p_fieldcat_wa TYPE slis_fieldcat_alv.

  DATA h_ifloalt  LIKE v_ifloalt.
  DATA h_dd04v_wa LIKE dd04v.

  IF p_key = '0'.
*--- Primärsicht -> Text immer aus Datenelement
    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = 'TPLNR_0'
        langu         = sy-langu
      IMPORTING
        dd04v_wa      = h_dd04v_wa
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    p_fieldcat_wa-seltext_s    = h_dd04v_wa-scrtext_s.
    p_fieldcat_wa-seltext_m    = h_dd04v_wa-scrtext_m.
    p_fieldcat_wa-seltext_l    = h_dd04v_wa-scrtext_l.
    p_fieldcat_wa-reptext_ddic = h_dd04v_wa-reptext.
  ELSE.
    CALL FUNCTION 'ILOX_IFLOALT_READ'
      EXPORTING
        i_alkey        = p_key
      IMPORTING
        e_v_ifloalt    = h_ifloalt
      EXCEPTIONS
        not_maintained = 1
        OTHERS         = 2.
    IF sy-subrc IS INITIAL.
*--- Text in Feldkatalog übernehmen
      p_fieldcat_wa-seltext_s    = h_ifloalt-altxt.
      p_fieldcat_wa-seltext_m    = h_ifloalt-altxt.
      p_fieldcat_wa-seltext_l    = h_ifloalt-altxt.
      p_fieldcat_wa-reptext_ddic = h_ifloalt-altxt.
    ELSE.
*--- Feld ausblenden
      p_fieldcat_wa-tech = g_x.
    ENDIF.
  ENDIF.

ENDFORM.                               " GET_KONVR_TEXT_L
*&---------------------------------------------------------------------*
*&      Form  CHECK_KONVERS_L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_KONVR_FLAG  text
*----------------------------------------------------------------------*
FORM check_konvers_l USING p_konvr_flag TYPE c.

  DATA h_itobcust   LIKE itobcust.
*--- Ist Kennzeichnung übehaupt aktiv?
  CALL FUNCTION 'ILOX_ITOBCUST_READ'
    IMPORTING
      e_itobcust = h_itobcust
    EXCEPTIONS
      OTHERS     = 1.
  IF h_itobcust-state = '1' OR sy-subrc <> 0.
*--- keine Kennzeichung aktiv
    g_konvr_flag = no. EXIT.
  ENDIF.
*--- Sind Kennzeichungsfelder in Feldauswahl ausgewählt
  PERFORM check_field_display_f14 USING 'TPLNR_0' g_konvr_flag.
  IF g_konvr_flag = yes. EXIT. ENDIF.
  PERFORM check_field_display_f14 USING 'TPLNR_1' g_konvr_flag.
  IF g_konvr_flag = yes. EXIT. ENDIF.
  PERFORM check_field_display_f14 USING 'TPLNR_2' g_konvr_flag.
  IF g_konvr_flag = yes. EXIT. ENDIF.
  PERFORM check_field_display_f14 USING 'TPLNR_3' g_konvr_flag.
  IF g_konvr_flag = yes. EXIT. ENDIF.
  PERFORM check_field_display_f14 USING 'ALKEY' g_konvr_flag.
  IF g_konvr_flag = yes. EXIT. ENDIF.

ENDFORM.                               " CHECK_KONVERS_L
*&---------------------------------------------------------------------*
*&      Form  FILL_ADD_TPLNRX_L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_add_tplnrx_l CHANGING cs_object_tab LIKE LINE OF object_tab.

  DATA h_iflos_wa TYPE ilox_iflos.
*--- Primärkennzeichung
  READ TABLE g_iflos_tab WITH KEY tplnr = cs_object_tab-tplnr_int
                                  actvs = g_x
                                  prkey = g_x
             INTO h_iflos_wa.
  IF sy-subrc IS INITIAL.
    cs_object_tab-tplnr_0 = h_iflos_wa-strno.
    cs_object_tab-alkey   = h_iflos_wa-alkey.
  ENDIF.
*---- Alternative 1
  READ TABLE g_iflos_tab WITH KEY tplnr = cs_object_tab-tplnr_int
                                  actvs = g_x
                                  alkey = '1'  BINARY SEARCH
             INTO h_iflos_wa.
  IF sy-subrc IS INITIAL.
    cs_object_tab-tplnr_1 = h_iflos_wa-strno.
  ENDIF.
*--- Alternative 2
  READ TABLE g_iflos_tab WITH KEY tplnr = cs_object_tab-tplnr_int
                                  actvs = g_x
                                  alkey = '2' BINARY SEARCH
             INTO h_iflos_wa.
  IF sy-subrc IS INITIAL.
    cs_object_tab-tplnr_2 = h_iflos_wa-strno.
  ENDIF.
*---- Alternative 3
  READ TABLE g_iflos_tab WITH KEY tplnr = cs_object_tab-tplnr_int
                                  actvs = g_x
                                  alkey = '3' BINARY SEARCH
             INTO h_iflos_wa.
  IF sy-subrc IS INITIAL.
    cs_object_tab-tplnr_3 = h_iflos_wa-strno.
  ENDIF.

ENDFORM.                               " FILL_ADD_TPLNRX_L
*&---------------------------------------------------------------------*
*&      Form  CHECK_SEL_STATI_L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  <--  E_OBJ_FOUND Object for status found? Yes/No
*----------------------------------------------------------------------*
FORM check_sel_stati_l USING e_obj_found TYPE c.

  RANGES: h_tplnr  FOR  iflo-tplnr,
          h_dummy  FOR  jest-stat.

  STATICS: h_once.

  STATICS: BEGIN OF h_tplnr2 OCCURS 0,
            sign(1),
            option(2),
            low  LIKE iflo-tplnr,
            high LIKE iflo-tplnr,
           END OF h_tplnr2.

  CLEAR e_obj_found.

  CHECK NOT stai1[] IS INITIAL.

  IF h_once IS INITIAL.
    h_tplnr2[] = tplnr[].
    h_once     = g_x.
  ENDIF.

  PERFORM preselect_status_f22 TABLES stai1
                                      h_dummy
                                      h_tplnr
                               USING  'IF'
                                      e_obj_found.

*--- nothing found or too many entries
  IF e_obj_found NE yes.
    EXIT.
  ENDIF.

*--- Select-option für tplnr füllen, vorhanden Eingrenzungen
*--- berücksichtigen, Mehrfacheinträge löschen
  CLEAR tplnr. REFRESH tplnr.

  LOOP AT h_tplnr WHERE low IN h_tplnr2.
    APPEND h_tplnr TO tplnr.
  ENDLOOP.

  IF tplnr[] IS INITIAL.
*--- es wurde nicht selektiert
    MESSAGE s047(ih).
    e_obj_found = no.
    EXIT.
  ELSE.
    e_obj_found = yes.
  ENDIF.

  SORT tplnr.
  DELETE ADJACENT DUPLICATES FROM tplnr.

ENDFORM.                               " CHECK_SEL_STATI_L

*&---------------------------------------------------------------------*
*&      Form  check_warranty_l
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_MGANR_FLAG  text
*----------------------------------------------------------------------*
FORM check_warranty_l USING p_mganr_flag LIKE riwo00-selec.

  PERFORM check_field_display_f14 USING 'GWLDT_K' p_mganr_flag.
  IF p_mganr_flag = yes. EXIT. ENDIF.
  PERFORM check_field_display_f14 USING 'GWLEN_K' p_mganr_flag.
  IF p_mganr_flag = yes. EXIT. ENDIF.
  PERFORM check_field_display_f14 USING 'MGANR_K' p_mganr_flag.
  IF p_mganr_flag = yes. EXIT. ENDIF.
  PERFORM check_field_display_f14 USING 'GARTX_K' p_mganr_flag.
  IF p_mganr_flag = yes. EXIT. ENDIF.
  PERFORM check_field_display_f14 USING 'GWLDT_L' p_mganr_flag.
  IF p_mganr_flag = yes. EXIT. ENDIF.
  PERFORM check_field_display_f14 USING 'GWLEN_L' p_mganr_flag.
  IF p_mganr_flag = yes. EXIT. ENDIF.
  PERFORM check_field_display_f14 USING 'MGANR_L' p_mganr_flag.
  IF p_mganr_flag = yes. EXIT. ENDIF.
  PERFORM check_field_display_f14 USING 'GARTX_L' p_mganr_flag.
  IF p_mganr_flag = yes. EXIT. ENDIF.

ENDFORM.                               " check_warranty_l
*eject
*&---------------------------------------------------------------------*
*&      Form  GET_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_path .

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    IMPORTING
      serverfile       = p_path
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " GET_PATH
*-------------------------------------------------------------------*
* INCLUDES                                                          *
*-------------------------------------------------------------------*
INCLUDE miolxf14.
INCLUDE miolxf16.
INCLUDE miolxf17.
INCLUDE miolxf18.
INCLUDE miolxf22.
INCLUDE miolxf24.
INCLUDE miolxf25.
INCLUDE miolxf50.
INCLUDE miolxf66.
INCLUDE miolxf70.
INCLUDE miolxf76.
INCLUDE miolxf78.
INCLUDE miolxf79.
INCLUDE miolxf82.
INCLUDE dbqmif01.
ENHANCEMENT-POINT RIIFLO20_01 SPOTS ZES_RIIFLO20 STATIC .

INCLUDE eaml_list_edit_reports.
*&---------------------------------------------------------------------*
*&      Form  WRITE_APPL_SERVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_APPL_SERVER .
  DATA: lv_path      TYPE string,
        lv_fieldname TYPE string,
        lv_line      TYPE string,
        lv_text      TYPE char50,
        lv_msg       TYPE text80,
        ls_fieldcat_tab LIKE LINE OF g_fieldcat_tab,
        ls_object_tab LIKE line of object_tab.
  FIELD-SYMBOLS: <fs1>.
  CLEAR : g_fieldcat_tab,lv_fieldname,lv_line,lv_text.

  lv_path = p_path.
  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT MESSAGE lv_msg.
  IF sy-subrc = 0.
    LOOP AT g_fieldcat_tab  INTO ls_fieldcat_tab.
      IF lv_fieldname IS INITIAL.
        lv_fieldname = ls_fieldcat_tab-seltext_l.
      ELSE.
        CONCATENATE lv_fieldname ',' ls_fieldcat_tab-seltext_l INTO lv_fieldname.
      ENDIF.
    ENDLOOP.

    TRANSFER lv_fieldname TO lv_path.

    LOOP AT object_tab INTO ls_object_tab.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE ls_object_tab TO <fs1>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        IF lv_line IS INITIAL.
          lv_line = <fs1> .
        ELSE.
          lv_text = <fs1>.
          CONCATENATE lv_line ',' lv_text INTO lv_line.
        ENDIF.
        CLEAR : lv_text.
      ENDDO.
      TRANSFER lv_line TO lv_path.
      CLEAR: lv_line.
    ENDLOOP.

    CLOSE DATASET lv_path.
    MESSAGE text-t04 TYPE 'S'.
  ELSE.
    WRITE: text-t03, lv_msg.
    STOP.
  ENDIF.

ENDFORM.                    " WRITE_APPL_SERVER
