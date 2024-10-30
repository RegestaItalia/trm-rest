CLASS zcl_trm_rest_bulk DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_tadir,
             pgmid    TYPE tadir-pgmid,
             object   TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             devclass TYPE tadir-devclass,
           END OF ty_tadir,
           tyt_tadir TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY.

    CLASS-METHODS get_transport_objs
      IMPORTING iv_trkorr       TYPE trkorr
      RETURNING VALUE(rt_tadir) TYPE tyt_tadir.

    CLASS-METHODS get_existing_objs
      IMPORTING it_tadir        TYPE tyt_tadir
      RETURNING VALUE(rt_tadir) TYPE tyt_tadir.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_rest_bulk IMPLEMENTATION.

  METHOD get_transport_objs.
    CHECK iv_trkorr IS NOT INITIAL.
    SELECT tadir~pgmid, tadir~object, tadir~obj_name, tadir~devclass FROM e071
    INNER JOIN tadir ON e071~pgmid = tadir~pgmid AND
                        e071~object = tadir~object AND
                        e071~obj_name = tadir~obj_name
    INTO CORRESPONDING FIELDS OF TABLE @rt_tadir
    WHERE trkorr EQ @iv_trkorr AND e071~pgmid EQ 'R3TR'.
  ENDMETHOD.

  METHOD get_existing_objs.
    CHECK it_tadir[] IS NOT INITIAL.
    SELECT tadir~pgmid, tadir~object, tadir~obj_name, tadir~devclass FROM tadir
    FOR ALL ENTRIES IN @it_tadir
    WHERE tadir~pgmid EQ @it_tadir-pgmid AND tadir~object EQ @it_tadir-object AND tadir~obj_name EQ @it_tadir-obj_name
    INTO CORRESPONDING FIELDS OF TABLE @rt_tadir.
  ENDMETHOD.

ENDCLASS.
