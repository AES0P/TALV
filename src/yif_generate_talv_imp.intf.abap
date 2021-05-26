interface YIF_GENERATE_TALV_IMP
  public .


  methods GENERATE_TALV
    importing
      value(KEY) type ZSTALV_KEY
    returning
      value(TALV) type ref to YCL_TALV_PARENT .
endinterface.
