CLASS zcl_talv_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get_talv
      IMPORTING
        VALUE(key)  TYPE zstalv_key
      RETURNING
        VALUE(talv) TYPE REF TO zcl_talv_parent .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TALV_FACTORY IMPLEMENTATION.


  METHOD get_talv.

    DATA: generate_talv_service TYPE REF TO zif_generate_talv_imp.
    DATA: service TYPE seoclsname.

    SELECT SINGLE clsname
      FROM ztalv_service
      INTO service
     WHERE type = key-type.

    CREATE OBJECT generate_talv_service TYPE (service).

    talv = generate_talv_service->generate_talv( key ).

  ENDMETHOD.
ENDCLASS.
