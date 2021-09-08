FUNCTION zfm_talv_multiple_free .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(DYNNR) TYPE  SY-DYNNR DEFAULT SY-DYNNR
*"----------------------------------------------------------------------

  ASSERT talv_multi IS NOT INITIAL.

  DATA lv_index TYPE i.
  LOOP AT talv_multi INTO talv.
    lv_index = sy-tabix.
    CHECK talv->get_key_info( 'DYNNR' ) = dynnr.
    talv->free( ).
    DELETE talv_multi INDEX lv_index.
  ENDLOOP.

ENDFUNCTION.
