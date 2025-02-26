CLASS zcl_trm_rest_resource DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS if_rest_resource~get REDEFINITION.
    METHODS if_rest_resource~post REDEFINITION.
    METHODS if_rest_resource~put REDEFINITION.
    METHODS if_rest_resource~delete REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_message_response,
             message TYPE symsg,
             log     TYPE zcx_trm_exception=>tyt_log,
           END OF ty_message_response.
    TYPES: tyt_et071 TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
           tyt_senvi TYPE STANDARD TABLE OF senvi WITH DEFAULT KEY.

    METHODS handle_request.
    METHODS get_request_json
      RETURNING VALUE(rv_json) TYPE string.
    METHODS get_request_rfcdest
      RETURNING VALUE(rv_rfcdest) TYPE rfcdest.

    METHODS read_table
      EXPORTING ev_status TYPE i
                ev_reason TYPE string.
    METHODS repository_environment
      EXPORTING ev_status TYPE i
                ev_reason TYPE string.
    METHODS get_dest
      EXPORTING ev_status TYPE i
                ev_reason TYPE string.

    METHODS add_lang_tr
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS add_objs_tr
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS add_skip_trkorr
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS add_src_trkorr
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS create_import_tr
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS create_package
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS create_toc
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS delete_transport
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS dequeue_tr
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS forward_tr
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS get_binary_file
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS get_devclass_objs
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS get_dir_trans
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS get_file_sys
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS get_obj_lock_tr
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS get_transport_layer
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS import_tr
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS list_object_types
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS ping
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS read_tms_queue
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS release_tr
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS rename_transport_request
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS set_install_devc
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS set_integrity
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS set_transport_doc
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS tadir_interface
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS tdevc_interface
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS tr_copy
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS version
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS write_binary_file
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS add_namespace
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS get_r3trans_info
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS remove_tr_comments
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS migrate_transport
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS delete_tms_transport
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.
    METHODS refresh_tms_transport_txt
      EXPORTING ev_status TYPE i
                ev_reason TYPE string
      RAISING   zcx_trm_exception.

    METHODS get_transport_objs_bulk
      EXPORTING ev_status TYPE i
                ev_reason TYPE string.
    METHODS get_existing_objs_bulk
      EXPORTING ev_status TYPE i
                ev_reason TYPE string.
ENDCLASS.



CLASS zcl_trm_rest_resource IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.

  METHOD if_rest_resource~get.
    CHECK mo_request->get_uri_path( ) NE '/'.
    handle_request( ).
  ENDMETHOD.

  METHOD if_rest_resource~post.
    handle_request( ).
  ENDMETHOD.

  METHOD if_rest_resource~put.
    handle_request( ).
  ENDMETHOD.

  METHOD if_rest_resource~delete.
    handle_request( ).
  ENDMETHOD.

  METHOD handle_request.
    DATA: lv_method           TYPE seocpdname,
          lv_status           TYPE i,
          lv_reason           TYPE string,
          lo_trm_exception    TYPE REF TO zcx_trm_exception,
          lo_response         TYPE REF TO if_rest_entity,
          ls_message_response TYPE ty_message_response.
    lv_method = mo_request->get_uri_attribute( iv_name = 'METH' ).
    CONDENSE lv_method.
    TRANSLATE lv_method TO UPPER CASE.
    IF lv_method <> 'VERSION'.
      CALL FUNCTION 'ZTRM_CHECK_AUTH'
        EXCEPTIONS
          trm_rfc_unauthorized = 1.
    ENDIF.
    IF sy-subrc <> 0.
      lv_status = cl_rest_status_code=>gc_client_error_unauthorized.
    ELSE.
      TRY.
          CALL METHOD me->(lv_method)
            IMPORTING
              ev_status = lv_status
              ev_reason = lv_reason.
          IF lv_status IS INITIAL.
            lv_status = cl_rest_status_code=>gc_success_ok.
          ENDIF.
        CATCH zcx_trm_exception INTO lo_trm_exception.
          lv_status = cl_rest_status_code=>gc_server_error_internal.
          lv_reason = lo_trm_exception->reason( ).
        CATCH cx_root.
          lv_status = cl_rest_status_code=>gc_server_error_internal.
          lv_reason = 'Method call exception'.
      ENDTRY.
    ENDIF.
    IF lv_status <> cl_rest_status_code=>gc_success_ok.
      lo_response = mo_response->create_entity( ).
      lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      IF lo_trm_exception IS BOUND.
        ls_message_response-message = lo_trm_exception->message.
        ls_message_response-log = lo_trm_exception->log( ).
      ELSE.
        MOVE-CORRESPONDING sy TO ls_message_response-message.
      ENDIF.
      lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_message_response pretty_name = 'X' ) ).
    ENDIF.
    mo_response->set_status( lv_status ).
    mo_response->set_reason( lv_reason ).
  ENDMETHOD.

  METHOD get_request_json.
    rv_json = mo_request->get_entity( )->get_string_data( ).
  ENDMETHOD.

  METHOD get_request_rfcdest.
    rv_rfcdest = mo_request->get_uri_query_parameter( 'rfcdest' ).
    IF rv_rfcdest IS INITIAL.
      rv_rfcdest = 'NONE'.
    ENDIF.
  ENDMETHOD.

  METHOD read_table.
    TYPES: BEGIN OF ty_request,
             query_table TYPE tabname,
             delimiter   TYPE so_text001,
             options     TYPE esh_t_co_rfcrt_options,
             fields      TYPE esh_t_co_rfcrt_fields,
           END OF ty_request.
    DATA: lv_request_json TYPE string,
          ls_request      TYPE ty_request,
          lv_destination  TYPE rfcdest,
          lt_data         TYPE STANDARD TABLE OF tab512,
          lo_response     TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).
    lv_destination = get_request_rfcdest( ).

    CALL FUNCTION 'RFC_READ_TABLE' DESTINATION lv_destination
      EXPORTING
        query_table = ls_request-query_table
        delimiter   = ls_request-delimiter
      TABLES
        options     = ls_request-options
        fields      = ls_request-fields
        data        = lt_data
      EXCEPTIONS
        table_without_data = 0
        OTHERS      = 1.
    IF sy-subrc <> 0.
      ev_status = cl_rest_status_code=>gc_server_error_internal.
    ELSE.
      lo_response = mo_response->create_entity( ).
      lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_response->set_string_data( /ui2/cl_json=>serialize( data = lt_data pretty_name = 'X' ) ).
    ENDIF.
  ENDMETHOD.

  METHOD repository_environment.
    TYPES: BEGIN OF ty_request,
             obj_type    TYPE seu_obj,
             object_name TYPE sobj_name,
           END OF ty_request,
           BEGIN OF ty_response,
             environment_tab TYPE tyt_senvi,
           END OF ty_response.
    DATA: lv_request_json TYPE string,
          ls_request      TYPE ty_request,
          lv_destination  TYPE rfcdest,
          ls_response     TYPE ty_response,
          lo_response     TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).
    lv_destination = get_request_rfcdest( ).

    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_RFC' DESTINATION lv_destination
      EXPORTING
        obj_type        = ls_request-obj_type
        object_name     = ls_request-object_name
      TABLES
        environment_tab = ls_response-environment_tab
      EXCEPTIONS
        OTHERS          = 1.
    IF sy-subrc <> 0.
      ev_status = cl_rest_status_code=>gc_server_error_internal.
    ELSE.
      lo_response = mo_response->create_entity( ).
      lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
    ENDIF.
  ENDMETHOD.

  METHOD get_dest.
    TYPES: BEGIN OF ty_response,
             dest TYPE sy-sysid,
           END OF ty_response.
    DATA: lv_destination TYPE rfcdest,
          ls_rfcsi       TYPE rfcsi,
          ls_response    TYPE ty_response,
          lo_response    TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_destination = get_request_rfcdest( ).

    CALL FUNCTION 'RFC_SYSTEM_INFO' DESTINATION lv_destination
      IMPORTING
        rfcsi_export = ls_rfcsi
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0.
      ev_status = cl_rest_status_code=>gc_server_error_internal.
    ELSE.
      ls_response = ls_rfcsi-rfcsysid.
      lo_response = mo_response->create_entity( ).
      lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
    ENDIF.
  ENDMETHOD.

  METHOD add_lang_tr.
    TYPES: BEGIN OF ty_request,
             trkorr   TYPE trkorr,
             devclass TYPE lxe_tt_packg,
           END OF ty_request.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_put.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).

    CREATE OBJECT lo_transport EXPORTING iv_trkorr = ls_request-trkorr.
    lo_transport->add_translations(
      EXPORTING
        it_devclass = ls_request-devclass
    ).
  ENDMETHOD.

  METHOD add_objs_tr.
    TYPES: BEGIN OF ty_request,
             trkorr TYPE trkorr,
             lock   TYPE flag,
             e071   TYPE tyt_et071,
           END OF ty_request,
           BEGIN OF ty_response,
             log TYPE sprot_u_tab,
           END OF ty_response.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request,
          ls_response     TYPE ty_response,
          lo_response     TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_put.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).

    CREATE OBJECT lo_transport EXPORTING iv_trkorr = ls_request-trkorr.
    lo_transport->add_objects(
      EXPORTING
        iv_lock   = ls_request-lock
        it_e071   = ls_request-e071
      IMPORTING
        et_log    = ls_response-log
    ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD add_skip_trkorr.
    TYPES: BEGIN OF ty_request,
             trkorr TYPE trkorr,
           END OF ty_request.
    DATA: lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_put.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    zcl_trm_utility=>add_skip_trkorr(
      EXPORTING
        iv_trkorr = ls_request-trkorr
    ).
  ENDMETHOD.

  METHOD add_src_trkorr.
    TYPES: BEGIN OF ty_request,
             trkorr TYPE trkorr,
           END OF ty_request.
    DATA: lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_put.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    zcl_trm_utility=>add_source_trkorr(
      EXPORTING
        iv_trkorr = ls_request-trkorr
    ).
  ENDMETHOD.

  METHOD create_import_tr.
    TYPES: BEGIN OF ty_request,
             text   TYPE as4text,
             target TYPE tr_target,
           END OF ty_request,
           BEGIN OF ty_response,
             trkorr TYPE trkorr,
           END OF ty_response.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request,
          ls_response     TYPE ty_response,
          lo_response     TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_post.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    zcl_trm_transport=>create_workbench(
      EXPORTING
        iv_text   = ls_request-text
        iv_target = ls_request-target
      RECEIVING
        ro_transport = lo_transport
    ).
    ls_response-trkorr = lo_transport->get_trkorr( ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD create_package.
    DATA: lv_request_json TYPE string,
          ls_request      TYPE scompkdtln.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_post.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    zcl_trm_package=>create(
      EXPORTING
        is_data = ls_request
    ).
  ENDMETHOD.

  METHOD create_toc.
    TYPES: BEGIN OF ty_request,
             text   TYPE as4text,
             target TYPE tr_target,
           END OF ty_request,
           BEGIN OF ty_response,
             trkorr TYPE trkorr,
           END OF ty_response.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request,
          ls_response     TYPE ty_response,
          lo_response     TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_post.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    zcl_trm_transport=>create_transport_of_copies(
      EXPORTING
        iv_text   = ls_request-text
        iv_target = ls_request-target
      RECEIVING
        ro_transport = lo_transport
    ).
    ls_response-trkorr = lo_transport->get_trkorr( ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD delete_transport.
    TYPES: BEGIN OF ty_request,
             trkorr TYPE trkorr,
           END OF ty_request.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_delete.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    CREATE OBJECT lo_transport EXPORTING iv_trkorr = ls_request-trkorr.
    lo_transport->delete( ).
  ENDMETHOD.

  METHOD dequeue_tr.
    TYPES: BEGIN OF ty_request,
             trkorr TYPE trkorr,
           END OF ty_request.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_post.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    CREATE OBJECT lo_transport EXPORTING iv_trkorr = ls_request-trkorr.
    lo_transport->dequeue( ).
  ENDMETHOD.

  METHOD forward_tr.
    TYPES: BEGIN OF ty_request,
             trkorr       TYPE trkorr,
             target       TYPE tmssysnam,
             source       TYPE tmssysnam,
             import_again TYPE flag,
           END OF ty_request.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_post.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    CREATE OBJECT lo_transport EXPORTING iv_trkorr = ls_request-trkorr.
    lo_transport->forward(
      EXPORTING
        iv_target       = ls_request-target
        iv_source       = ls_request-source
        iv_import_again = ls_request-import_again
    ).
  ENDMETHOD.

  METHOD get_binary_file.
    TYPES: BEGIN OF ty_request,
             file_path TYPE string,
           END OF ty_request.
    DATA: lv_request_json TYPE string,
          ls_request      TYPE ty_request,
          lv_response     TYPE xstring,
          lo_response     TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    zcl_trm_utility=>get_binary_file(
      EXPORTING
        iv_file_path = ls_request-file_path
      IMPORTING
        ev_file      = lv_response
    ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_octet_stream ).
    lo_response->set_binary_data( lv_response ).
  ENDMETHOD.

  METHOD get_devclass_objs.
    TYPES: BEGIN OF ty_request,
             devclass TYPE devclass,
           END OF ty_request,
           BEGIN OF ty_response,
             tadir TYPE scts_tadir,
           END OF ty_response.
    DATA: lo_package      TYPE REF TO zcl_trm_package,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request,
          ls_response     TYPE ty_response,
          lo_response     TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    CREATE OBJECT lo_package EXPORTING iv_devclass = ls_request-devclass.
    lo_package->get_objects(
      IMPORTING
        et_tadir    = ls_response-tadir
    ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD get_dir_trans.
    TYPES: BEGIN OF ty_response,
             dir_trans TYPE pfevalue,
           END OF ty_response.
    DATA: ls_response TYPE ty_response,
          lo_response TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.


    zcl_trm_utility=>get_dir_trans(
      IMPORTING
        ev_dir_trans = ls_response-dir_trans
    ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD get_file_sys.
    TYPES: BEGIN OF ty_response,
             file_sys TYPE filesys,
           END OF ty_response.
    DATA: ls_response TYPE ty_response,
          lo_response TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.


    zcl_trm_utility=>get_file_sys(
      IMPORTING
        ev_file_sys = ls_response-file_sys
    ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD get_obj_lock_tr.
    TYPES: BEGIN OF ty_request,
             pgmid    TYPE pgmid,
             object   TYPE trobjtype,
             obj_name TYPE trobj_name,
           END OF ty_request,
           BEGIN OF ty_response,
             trkorr TYPE trkorr,
           END OF ty_response.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request,
          ls_response     TYPE ty_response,
          lo_response     TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    zcl_trm_transport=>find_object_lock(
      EXPORTING
        iv_pgmid    = ls_request-pgmid
        iv_object   = ls_request-object
        iv_obj_name = ls_request-obj_name
      RECEIVING
        ro_transport = lo_transport
    ).
    ls_response-trkorr = lo_transport->get_trkorr( ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD get_transport_layer.
    TYPES: BEGIN OF ty_response,
             layer TYPE devlayer,
           END OF ty_response.
    DATA: ls_response TYPE ty_response,
          lo_response TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.


    zcl_trm_utility=>get_default_transport_layer(
      IMPORTING
        ev_layer = ls_response-layer
    ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD import_tr.
    TYPES: BEGIN OF ty_request,
             trkorr TYPE trkorr,
             system TYPE tmssysnam,
           END OF ty_request.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_post.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    CREATE OBJECT lo_transport EXPORTING iv_trkorr = ls_request-trkorr.
    lo_transport->import(
      EXPORTING
        iv_system = ls_request-system
    ).
  ENDMETHOD.

  METHOD list_object_types.
    TYPES: BEGIN OF ty_response,
             object_text TYPE tr_object_texts,
           END OF ty_response.
    DATA: ls_response TYPE ty_response,
          lo_response TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.


    zcl_trm_utility=>get_supported_object_types(
      IMPORTING
        et_object_text = ls_response-object_text
    ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD ping.
    TYPES: BEGIN OF ty_response,
             return TYPE string,
           END OF ty_response.
    DATA: ls_response TYPE ty_response,
          lo_response TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.


    ls_response-return = 'PONG'.

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD read_tms_queue.
    TYPES: BEGIN OF ty_request,
             target TYPE tmssysnam,
           END OF ty_request,
           BEGIN OF ty_response,
             requests TYPE tmsiqreqs,
           END OF ty_response.
    DATA: lv_request_json TYPE string,
          ls_request      TYPE ty_request,
          ls_response     TYPE ty_response,
          lo_response     TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    zcl_trm_transport=>read_queue(
      EXPORTING
        iv_target   = ls_request-target
      IMPORTING
        et_requests = ls_response-requests
    ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD release_tr.
    TYPES: BEGIN OF ty_request,
             trkorr TYPE trkorr,
             lock   TYPE flag,
           END OF ty_request,
           BEGIN OF ty_response,
             messages TYPE ctsgerrmsgs,
           END OF ty_response.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request,
          ls_response     TYPE ty_response,
          lo_response     TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_post.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    CREATE OBJECT lo_transport EXPORTING iv_trkorr = ls_request-trkorr.
    lo_transport->release(
      EXPORTING
        iv_lock     = ls_request-lock
      IMPORTING
        et_messages = ls_response-messages
    ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD rename_transport_request.
    TYPES: BEGIN OF ty_request,
             trkorr  TYPE trkorr,
             as4text TYPE as4text,
           END OF ty_request.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_post.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    CREATE OBJECT lo_transport EXPORTING iv_trkorr = ls_request-trkorr.
    lo_transport->rename(
      EXPORTING
        iv_as4text = ls_request-as4text
    ).
  ENDMETHOD.

  METHOD set_install_devc.
    TYPES: BEGIN OF ty_request,
             installdevc TYPE zcl_trm_utility=>tyt_installdevc,
           END OF ty_request.
    DATA: lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_put.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    zcl_trm_utility=>add_install_devclass(
      EXPORTING
        it_installdevc = ls_request-installdevc
    ).
  ENDMETHOD.

  METHOD set_integrity.
    TYPES: BEGIN OF ty_request,
             integrity TYPE ztrm_integrity,
           END OF ty_request.
    DATA: lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_put.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    zcl_trm_utility=>add_package_integrity(
      EXPORTING
        is_integrity = ls_request-integrity
    ).
  ENDMETHOD.

  METHOD set_transport_doc.
    TYPES: BEGIN OF ty_request,
             trkorr TYPE trkorr,
             doc    TYPE zcl_trm_transport=>tyt_tline,
           END OF ty_request.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_put.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    CREATE OBJECT lo_transport EXPORTING iv_trkorr = ls_request-trkorr.
    lo_transport->set_documentation(
      EXPORTING
        it_doc    = ls_request-doc
    ).
  ENDMETHOD.

  METHOD tadir_interface.
    TYPES: BEGIN OF ty_request,
             pgmid       TYPE pgmid,
             object      TYPE trobjtype,
             obj_name    TYPE sobj_name,
             devclass    TYPE devclass,
             srcsystem   TYPE srcsystem,
             author      TYPE responsibl,
             set_genflag TYPE genflag,
           END OF ty_request.
    DATA: lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_post.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    zcl_trm_utility=>tadir_interface(
      EXPORTING
        iv_pgmid     = ls_request-pgmid
        iv_object    = ls_request-object
        iv_objname   = ls_request-obj_name
        iv_devclass  = ls_request-devclass
        iv_srcsystem = ls_request-srcsystem
        iv_author    = ls_request-author
        iv_genflag   = ls_request-set_genflag
    ).
  ENDMETHOD.

  METHOD tdevc_interface.
    TYPES: BEGIN OF ty_request,
             devclass    TYPE devclass,
             parentcl    TYPE devclass,
             rm_parentcl TYPE flag,
             devlayer    TYPE devlayer,
           END OF ty_request.
    DATA: lo_package      TYPE REF TO zcl_trm_package,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_post.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    CREATE OBJECT lo_package EXPORTING iv_devclass = ls_request-devclass.
    lo_package->interface(
      EXPORTING
        iv_parentcl    = ls_request-parentcl
        iv_rm_parentcl = ls_request-rm_parentcl
        iv_devlayer    = ls_request-devlayer
    ).
  ENDMETHOD.

  METHOD tr_copy.
    TYPES: BEGIN OF ty_request,
             from TYPE trkorr,
             to   TYPE trkorr,
             doc  TYPE trparflag,
           END OF ty_request.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_post.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    CREATE OBJECT lo_transport EXPORTING iv_trkorr = ls_request-to.
    lo_transport->copy(
      EXPORTING
        iv_trkorr = ls_request-from
        iv_doc    = ls_request-doc
    ).
  ENDMETHOD.

  METHOD version.
    TYPES: BEGIN OF ty_response,
             server_version TYPE string,
             rest_version   TYPE string,
           END OF ty_response.
    DATA: ls_response TYPE ty_response,
          lo_response TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    ls_response-server_version = zif_trm=>version.
    ls_response-rest_version = zif_trm_rest=>version.

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD write_binary_file.
    DATA: lv_file_path  TYPE string,
          lv_xfile_path TYPE xstring,
          lv_file       TYPE xstring.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_post.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    DATA(lo_entity) = NEW cl_rest_multipart_form_data( mo_request->get_entity( ) ).
    lv_file_path = lo_entity->get_form_field( 'file_path' ).
    IF lv_file_path IS INITIAL.
      lo_entity->get_file(
        EXPORTING
          iv_name = 'file_path'
        IMPORTING
          ev_data = lv_xfile_path
      ).
      lv_file_path = cl_bcs_convert=>xstring_to_string(
        EXPORTING
          iv_xstr   = lv_xfile_path
          iv_cp     = 1100
      ).
    ENDIF.
    lo_entity->get_file(
      EXPORTING
        iv_name = 'file'
      IMPORTING
        ev_data = lv_file
    ).

    zcl_trm_utility=>write_binary_file(
      EXPORTING
        iv_file_path = lv_file_path
        iv_file      = lv_file
    ).
  ENDMETHOD.

  METHOD add_namespace.
    TYPES: BEGIN OF ty_request,
             namespace  TYPE namespace,
             replicense TYPE trnlicense,
             texts      TYPE zcl_trm_utility=>tyt_trnspacett,
           END OF ty_request.
    DATA: lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_put.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    zcl_trm_utility=>add_namespace(
      EXPORTING
        iv_namespace  = ls_request-namespace
        iv_replicense = ls_request-replicense
        it_texts      = ls_request-texts
    ).
  ENDMETHOD.

  METHOD get_transport_objs_bulk.
    TYPES: BEGIN OF ty_request,
             trkorr TYPE trkorr,
           END OF ty_request,
           BEGIN OF ty_response,
             tadir TYPE zcl_trm_rest_bulk=>tyt_tadir,
           END OF ty_response.
    DATA: lv_request_json TYPE string,
          ls_request      TYPE ty_request,
          ls_response     TYPE ty_response,
          lo_response     TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    ls_response-tadir = zcl_trm_rest_bulk=>get_transport_objs( iv_trkorr = ls_request-trkorr ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD get_existing_objs_bulk.
    TYPES: BEGIN OF ty_request,
             objects TYPE zcl_trm_rest_bulk=>tyt_tadir,
           END OF ty_request,
           BEGIN OF ty_response,
             tadir TYPE zcl_trm_rest_bulk=>tyt_tadir,
           END OF ty_response.
    DATA: lv_request_json TYPE string,
          ls_request      TYPE ty_request,
          ls_response     TYPE ty_response,
          lo_response     TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    ls_response-tadir = zcl_trm_rest_bulk=>get_existing_objs( it_tadir = ls_request-objects ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD get_r3trans_info.
    TYPES: BEGIN OF ty_response,
             log TYPE string,
           END OF ty_response.
    DATA: ls_response TYPE ty_response,
          lo_response TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_get.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.


    ls_response-log = zcl_trm_utility=>get_r3trans_info( ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD migrate_transport.
    TYPES: BEGIN OF ty_request,
             trkorr TYPE trkorr,
           END OF ty_request,
           BEGIN OF ty_response,
             trm_trkorr TYPE ztrm_trkorr,
           END OF ty_response.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request,
          ls_response     TYPE ty_response,
          lo_response     TYPE REF TO if_rest_entity.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_post.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    CREATE OBJECT lo_transport EXPORTING iv_trkorr = ls_request-trkorr.
    lo_transport->migrate(
      IMPORTING
        ev_trm_trkorr = ls_response-trm_trkorr
    ).

    lo_response = mo_response->create_entity( ).
    lo_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response->set_string_data( /ui2/cl_json=>serialize( data = ls_response pretty_name = 'X' ) ).
  ENDMETHOD.

  METHOD delete_tms_transport.
    TYPES: BEGIN OF ty_request,
             trkorr TYPE trkorr,
             system TYPE tmssysnam,
           END OF ty_request.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_delete.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    CREATE OBJECT lo_transport EXPORTING iv_trkorr = ls_request-trkorr.
    lo_transport->delete_from_tms_queue( iv_system = ls_request-system ).
  ENDMETHOD.

  METHOD refresh_tms_transport_txt.
    TYPES: BEGIN OF ty_request,
             trkorr TYPE trkorr,
           END OF ty_request.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_delete.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    CREATE OBJECT lo_transport EXPORTING iv_trkorr = ls_request-trkorr.
    lo_transport->refresh_tms_txt( ).
  ENDMETHOD.

  METHOD remove_tr_comments.
    TYPES: BEGIN OF ty_request,
             trkorr TYPE trkorr,
             object TYPE trobjtype,
           END OF ty_request.
    DATA: lo_transport    TYPE REF TO zcl_trm_transport,
          lv_request_json TYPE string,
          ls_request      TYPE ty_request.

    IF mo_request->get_method( ) <> if_rest_message=>gc_method_delete.
      ev_status = cl_rest_status_code=>gc_client_error_meth_not_allwd.
      RETURN.
    ENDIF.

    lv_request_json = get_request_json( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_json CHANGING data = ls_request ).


    CREATE OBJECT lo_transport EXPORTING iv_trkorr = ls_request-trkorr.
    lo_transport->remove_comments( iv_object = ls_request-object ).
  ENDMETHOD.

ENDCLASS.
