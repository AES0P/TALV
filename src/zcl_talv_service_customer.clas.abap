CLASS zcl_talv_service_customer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_talv_generate_imp .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TALV_SERVICE_CUSTOMER IMPLEMENTATION.


  METHOD zif_talv_generate_imp~generate_talv.

    IF key-container IS NOT BOUND OR key-dynnr IS INITIAL OR key-container_position = '00'.
      MESSAGE '必须传递容器、容器位置和屏幕编号！' TYPE 'E'.
    ENDIF.

    "上限提升至81，由于最多两位数，故此处无需再手动判断是否超过9*9 20210805
*    IF key-container_position > '36'.
*      MESSAGE '不能超过创建上限36' TYPE 'E'.
*    ENDIF.

    CREATE OBJECT talv TYPE zcl_talv
      EXPORTING
        talv_key = key.

    CALL FUNCTION 'ZFM_TALV_MULTIPLE_ADD'
      EXPORTING
        talv = talv.

  ENDMETHOD.
ENDCLASS.
