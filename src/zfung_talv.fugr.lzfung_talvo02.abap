*----------------------------------------------------------------------*
***INCLUDE LZTALVO02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module PBO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo OUTPUT.

  READ TABLE talv_stack INTO talv INDEX lines( talv_stack ).
  talv->pbo( ).

ENDMODULE.
