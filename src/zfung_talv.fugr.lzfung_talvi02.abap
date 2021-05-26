*&---------------------------------------------------------------------*
*& 包含               LZFUNG_TALVI02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai INPUT.

  READ TABLE talv_stack INTO talv INDEX lines( talv_stack ).
  talv->pai( CHANGING ucomm = ok_code ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

  READ TABLE talv_stack INTO talv INDEX lines( talv_stack ).
  talv->free( ).
  FREE talv.
  DELETE talv_stack INDEX lines( talv_stack ).
  LEAVE TO SCREEN 0.

ENDMODULE.
