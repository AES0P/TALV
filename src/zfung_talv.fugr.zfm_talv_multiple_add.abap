FUNCTION zfm_talv_multiple_add .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(TALV) TYPE REF TO  ZCL_TALV_PARENT
*"----------------------------------------------------------------------

  APPEND talv TO talv_multi.

ENDFUNCTION.
