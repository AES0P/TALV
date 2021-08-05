CLASS zcl_talv_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_talv_generate_imp .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TALV_SERVICE IMPLEMENTATION.


  METHOD zif_talv_generate_imp~generate_talv.

    DATA dynnr(4) TYPE n VALUE '8999'.

    IF key-dynnr IS INITIAL.

      DATA container_name(132) TYPE c VALUE 'CON_FULL00'.

      IMPORT dynnr = dynnr FROM MEMORY ID 'TALV_FULL_DYNNR'.

      ADD 1 TO dynnr.
      container_name+8(2) = dynnr+2(2).

      EXPORT dynnr = dynnr TO MEMORY ID 'TALV_FULL_DYNNR'.

      key-dynnr          = dynnr.
      key-container_name = container_name.

    ELSE.

      dynnr = key-dynnr.
      EXPORT dynnr = dynnr TO MEMORY ID 'TALV_FULL_DYNNR'.

      container_name+8(2) = dynnr+2(2).
      key-container_name  = container_name.

    ENDIF.

    CLEAR key-container_position.

    CREATE OBJECT talv TYPE zcl_talv
      EXPORTING
        talv_key = key.

  ENDMETHOD.
ENDCLASS.
