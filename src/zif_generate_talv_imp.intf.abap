INTERFACE zif_generate_talv_imp
  PUBLIC .


  METHODS generate_talv
    IMPORTING
      VALUE(key)  TYPE zstalv_key
    RETURNING
      VALUE(talv) TYPE REF TO zcl_talv_parent .

ENDINTERFACE.
