interface YIF_TALV_JSON_HANDLER
  public .


  interfaces YIF_SINGLETON .

  types:
    BEGIN OF ty_kv.
             INCLUDE TYPE lvc_s_fcat.
    TYPES:   value TYPE string,
           END OF ty_kv .
  types:
    tty_kv TYPE STANDARD TABLE OF ty_kv WITH KEY fieldname .

  data FIELDCATS type TTY_KV .

  methods JSON_TO_TALV
    importing
      value(JSON) type STRING
      value(SPLIT_SYMBOL) type C default ','
      value(TALV_TYPE) type ZDETALV_TYPE optional
    returning
      value(TALV) type ref to YCL_TALV_PARENT .
  methods TALV_TO_JSON
    importing
      value(TALV) type ref to YCL_TALV_PARENT
      value(SPLIT_SYMBOL) type C
    returning
      value(JSON) type STRING .
endinterface.
