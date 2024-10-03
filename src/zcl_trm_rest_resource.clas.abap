CLASS zcl_trm_rest_resource DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS if_rest_resource~post REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_request_json
      RETURNING VALUE(rv_json) TYPE string.

    METHODS add_lang_tr
      EXPORTING ev_status TYPE i
                ev_reason TYPE string.
ENDCLASS.



CLASS zcl_trm_rest_resource IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.

  METHOD if_rest_resource~post.
    DATA: lv_method   TYPE seocpdname,
          lv_status   TYPE i,
          lv_reason   TYPE string,
          lo_response TYPE REF TO if_rest_entity,
          ls_message  TYPE symsg.
    lv_method = mo_request->get_uri_attribute( iv_name = 'METH' ).
    TRY.
        CALL METHOD me->(lv_method)
          IMPORTING
            ev_status = lv_status
            ev_reason = lv_reason.
      CATCH cx_root.
        lv_status = cl_rest_status_code=>gc_server_error_internal.
        lv_reason = 'Method call exception'.
    ENDTRY.
    IF lv_status <> cl_rest_status_code=>gc_success_ok.
      lo_response = mo_response->create_entity( ).
      lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      MOVE-CORRESPONDING sy TO ls_message.
      lo_response->set_string_data( /ui2/cl_json=>serialize( ls_message ) ).
    ENDIF.
    mo_response->set_status( lv_status ).
    mo_response->set_reason( lv_reason ).
  ENDMETHOD.

  METHOD get_request_json.
    rv_json = mo_request->get_entity( )->get_string_data( ).
  ENDMETHOD.

  METHOD add_lang_tr.
    TYPES: BEGIN OF ty_request,
             trkorr   TYPE trkorr,
             devclass TYPE lxe_tt_packg,
           END OF ty_request.
    DATA: lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).

    CALL FUNCTION 'ZTRM_ADD_LANG_TR'
      EXPORTING
        iv_trkorr            = ls_request-trkorr
      TABLES
        it_devclass          = ls_request-devclass
      EXCEPTIONS
        trm_rfc_unauthorized = 1
        invalid_input        = 2
        generic              = 3
        OTHERS               = 4.
    IF sy-subrc EQ 0.
      ev_status = 200.
    ELSE.
      ev_status = 500.
      IF sy-subrc EQ 1.
        ev_reason = 'trm_rfc_unauthorized'.
      ELSEIF sy-subrc EQ 2.
        ev_reason = 'invalid_input'.
      ELSEIF sy-subrc EQ 3.
        ev_reason = 'generic'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
