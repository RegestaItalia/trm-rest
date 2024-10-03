CLASS zcl_trm_rest_handler DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_http_handler
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS if_rest_application~get_root_handler REDEFINITION.
  PROTECTED SECTION.
    METHODS handle_csrf_token REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_rest_handler IMPLEMENTATION.

  METHOD if_rest_application~get_root_handler.
    DATA: lo_handler       TYPE REF TO cl_rest_router,
          lt_parambind_tab TYPE abap_parmbind_tab.

    CREATE OBJECT lo_handler.

    lo_handler->attach(
      EXPORTING
        iv_template      = '/{METH:.*}'
        iv_handler_class = 'ZCL_TRM_REST_RESOURCE'
    ).

    ro_root_handler = lo_handler.
  ENDMETHOD.

  METHOD handle_csrf_token.
    " disabled
  ENDMETHOD.

ENDCLASS.
