*&---------------------------------------------------------------------*
*& Report ZAESOP_TALV_INITIALIZE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaesop_talv_initialize.

PARAMETERS p_tr AS CHECKBOX.

"服务配置初始化
DATA talv_setting TYPE STANDARD TABLE OF ztalv_service.

APPEND VALUE #( mandt = sy-mandt type = 'TALV'       clsname = 'ZCL_TALV_SERVICE'          description = 'SERVICE 4 FULL SCREEN TALV'     ) TO talv_setting.
APPEND VALUE #( mandt = sy-mandt type = 'TALV_CUS'   clsname = 'ZCL_TALV_SERVICE_CUSTOMER' description = 'SERVICE 4 CUSTOMER SCREEN TALV' ) TO talv_setting.
APPEND VALUE #( mandt = sy-mandt type = 'TALV_POPUP' clsname = 'ZCL_TALV_SERVICE_POPUP'    description = 'SERVICE 4 POPUP TALV'           ) TO talv_setting.

MODIFY ztalv_service FROM TABLE talv_setting.
CHECK sy-subrc = 0.
COMMIT WORK.

"将配置通过请求传输
CHECK p_tr = abap_true AND zcl_tr_table_entry=>get_instance( )->associate( ).

zcl_tr_table_entry=>get_instance( )->entrys( talv_setting )->commit( ).
