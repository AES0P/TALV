CLASS zcl_talv DEFINITION
  PUBLIC
  INHERITING FROM zcl_talv_parent
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS display_popup
      IMPORTING
        !start_row    TYPE i OPTIONAL
        !start_column TYPE i OPTIONAL
        !end_row      TYPE i OPTIONAL
        !end_column   TYPE i OPTIONAL .

    METHODS display
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TALV IMPLEMENTATION.


  METHOD display.

    IF key-container IS BOUND.
      pbo( ).
    ELSE.
      CALL FUNCTION 'ZFM_TALV_DISPLAY'
        EXPORTING
          alv = me.
    ENDIF.
  ENDMETHOD.


  METHOD display_popup.

    IF key-container IS BOUND.
      pbo( ).
    ELSE.
      CALL FUNCTION 'ZFM_TALV_DISPLAY'
        EXPORTING
          alv          = me
          start_row    = start_row
          start_column = start_column
          end_row      = end_row
          end_column   = end_column.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
