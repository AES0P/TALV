INTERFACE zif_talv_json_handler
  PUBLIC .


  INTERFACES zif_singleton .

  TYPES:
    BEGIN OF ty_kv.
      INCLUDE TYPE lvc_s_fcat.
  TYPES: value TYPE string,
    END OF ty_kv .
  TYPES:
    tty_kv TYPE STANDARD TABLE OF ty_kv WITH KEY fieldname .

  DATA fieldcats TYPE tty_kv .

  METHODS json_to_talv
    IMPORTING
      !json         TYPE string
      !split_symbol TYPE c DEFAULT ','
      !talv_type    TYPE zdetalv_type OPTIONAL
    RETURNING
      VALUE(talv)   TYPE REF TO zcl_talv_parent .
  METHODS talv_to_json
    IMPORTING
      VALUE(talv)         TYPE REF TO zcl_talv_parent
      VALUE(split_symbol) TYPE c
    RETURNING
      VALUE(json)         TYPE string .
ENDINTERFACE.
