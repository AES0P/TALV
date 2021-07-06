class YCL_TALV_SERVICE_CUSTOMER definition
  public
  final
  create public .

public section.

  interfaces YIF_GENERATE_TALV_IMP .
protected section.
private section.
ENDCLASS.



CLASS YCL_TALV_SERVICE_CUSTOMER IMPLEMENTATION.


  METHOD yif_generate_talv_imp~generate_talv.

    IF key-container IS NOT BOUND OR key-dynnr IS INITIAL OR key-container_position = '00'.
      MESSAGE '必须传递容器、容器位置和屏幕编号！' TYPE 'E'.
    ENDIF.

    IF key-container_position > '36'.
      MESSAGE '不能超过创建上限36' TYPE 'E'.
    ENDIF.

    CREATE OBJECT talv TYPE ycl_talv
      EXPORTING
        talv_key = key.

  ENDMETHOD.
ENDCLASS.
