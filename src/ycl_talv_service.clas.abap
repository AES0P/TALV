class YCL_TALV_SERVICE definition
  public
  final
  create public .

public section.

  interfaces YIF_GENERATE_TALV_IMP .
protected section.
private section.
ENDCLASS.



CLASS YCL_TALV_SERVICE IMPLEMENTATION.


  METHOD yif_generate_talv_imp~generate_talv.

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

    CREATE OBJECT talv TYPE ycl_talv
      EXPORTING
        talv_key = key.

  ENDMETHOD.
ENDCLASS.
