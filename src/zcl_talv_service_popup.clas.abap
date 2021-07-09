CLASS zcl_talv_service_popup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_generate_talv_imp .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TALV_SERVICE_POPUP IMPLEMENTATION.


  METHOD zif_generate_talv_imp~generate_talv.

    DATA dynnr(4) TYPE n VALUE '9899'.

    IF key-dynnr IS INITIAL.

      DATA container_name(132) TYPE c VALUE 'CON_POPUP00'.

      IMPORT dynnr = dynnr FROM MEMORY ID 'TALV_POP_DYNNR'.

      ADD 1 TO dynnr.
      container_name+9(2) = dynnr+2(2).

      EXPORT dynnr = dynnr TO MEMORY ID 'TALV_POP_DYNNR'.

      key-dynnr          = dynnr.
      key-container_name = container_name.

    ELSE.

      dynnr = key-dynnr.
      EXPORT dynnr = dynnr TO MEMORY ID 'TALV_POP_DYNNR'.

      container_name+9(2) = dynnr+2(2).
      key-container_name  = container_name.

    ENDIF.

    CLEAR key-container_position.

    CREATE OBJECT talv TYPE zcl_talv
      EXPORTING
        talv_key = key.

  ENDMETHOD.
ENDCLASS.
