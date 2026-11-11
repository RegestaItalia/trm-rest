REPORT /atrm/rest_info.

START-OF-SELECTION.
  TRY.
      DATA(lv_endpoint) = /atrm/cl_utilities=>get_http_base_url( service = 'ztrmserver' ).
      WRITE / 'Use this endpoint to connect a client via REST'.
      WRITE / lv_endpoint.
    CATCH /atrm/cx_exception INTO DATA(lo_exc).
      WRITE / lo_exc->get_text( ).
  ENDTRY.
