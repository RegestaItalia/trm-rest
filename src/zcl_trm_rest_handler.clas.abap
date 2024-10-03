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
    DATA: lo_handler         TYPE REF TO cl_rest_router.
    CREATE OBJECT lo_handler.
  ENDMETHOD.

  METHOD handle_csrf_token.
    " disabled
  ENDMETHOD.

ENDCLASS.
