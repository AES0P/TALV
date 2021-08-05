interface ZIF_TALV_GENERATE_IMP
  public .


  methods GENERATE_TALV
    importing
      value(KEY) type ZSTALV_KEY
    returning
      value(TALV) type ref to ZCL_TALV_PARENT .
endinterface.
