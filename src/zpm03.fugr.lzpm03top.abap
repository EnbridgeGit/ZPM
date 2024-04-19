FUNCTION-POOL zpm03.                        "MESSAGE-ID ..

* INCLUDE LZPM03D...                         " Local class definition

TYPES: BEGIN OF gty_station,
        tplnr    TYPE iflot-tplnr,
        pltxt    TYPE iflotx-pltxt,
        tplma    TYPE iflot-tplma,
        erdat    TYPE erdat,
        aedat    TYPE aedat,
        pm_objty TYPE pm_objty,
        lgwid    TYPE lgwid,
        objnr    TYPE iflo-objnr,
        stort    TYPE pmloc,
        msgrp    TYPE raumnr,
      END OF gty_station,

      BEGIN OF gty_jest,
        objnr    TYPE jest-objnr,
      END OF gty_jest,

      BEGIN OF gty_crhd,
        objty TYPE cr_objty,
        objid TYPE cr_objid,
        arbpl TYPE arbpl,
        ktext TYPE cr_ktext,
      END OF gty_crhd.

TYPES: BEGIN OF gty_branch,
        tplnr    TYPE iflot-tplnr,
        pltxt    TYPE iflotx-pltxt,
        tplma    TYPE iflot-tplma,
      END OF gty_branch,

      BEGIN OF gty_district,
        tplnr    TYPE iflot-tplnr,
        pltxt    TYPE iflotx-pltxt,
      END OF gty_district,

      BEGIN OF gty_cabn,
        atinn TYPE cabn-atinn,
        atnam TYPE cabn-atnam,
        atfor TYPE cabn-atfor,
      END OF gty_cabn,

      BEGIN OF gty_objek,
        objek TYPE ausp-objek,
      END OF gty_objek,

      BEGIN OF gty_inob,
        cuobj TYPE inob-cuobj,
        objek TYPE inob-objek,
      END OF gty_inob,

      BEGIN OF gty_ausp,
        objek TYPE ausp-objek,
        atinn TYPE ausp-atinn,
        atwrt TYPE ausp-atwrt,
        atflv TYPE ausp-atflv,
      END OF gty_ausp,

      BEGIN OF gty_cawnt,
        atwrt TYPE atwrt,
        atwtb TYPE atwtb,
      END OF gty_cawnt.

DATA: git_station  TYPE SORTED TABLE OF gty_station  WITH UNIQUE KEY tplnr WITH NON-UNIQUE SORTED KEY k1 COMPONENTS tplma ##needed,
      git_crhd     TYPE SORTED TABLE OF gty_crhd     WITH NON-UNIQUE KEY objty objid ##needed,
      git_district TYPE SORTED TABLE OF gty_district WITH UNIQUE KEY tplnr ##needed,
      git_jest     TYPE SORTED TABLE OF gty_jest     WITH UNIQUE KEY objnr ##needed,
      git_branch   TYPE SORTED TABLE OF gty_branch   WITH UNIQUE KEY tplnr WITH NON-UNIQUE SORTED KEY k1 COMPONENTS tplma ##needed,
      git_ausp     TYPE SORTED TABLE OF gty_ausp     WITH NON-UNIQUE KEY objek atinn ##needed,
      git_objek    TYPE SORTED TABLE OF gty_objek    WITH NON-UNIQUE KEY objek ##needed,
      git_cabn     TYPE SORTED TABLE OF gty_cabn     WITH NON-UNIQUE KEY atnam ##needed,
      git_cawnt    TYPE STANDARD TABLE OF gty_cawnt,
      git_inob     TYPE STANDARD TABLE OF gty_inob,

      gv_purps_code TYPE atinn,
      gv_class_code TYPE atinn.
