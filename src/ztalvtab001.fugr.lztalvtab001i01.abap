*----------------------------------------------------------------------*
***INCLUDE LZTALVTAB001I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  GET_DESCRIPTION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_description INPUT.

  IF ztalv_service-clsname IS NOT INITIAL AND ztalv_service-description IS INITIAL.

    SELECT SINGLE descript
      FROM seoclasstx
      INTO ztalv_service-description
     WHERE clsname = ztalv_service-clsname
       AND langu   = sy-langu.

  ENDIF.

ENDMODULE.
