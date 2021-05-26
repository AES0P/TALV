CLASS ycl_talv DEFINITION
  PUBLIC
  INHERITING FROM ycl_talv_parent
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS display_popup
      IMPORTING
        VALUE(start_row)    TYPE i OPTIONAL
        VALUE(start_column) TYPE i OPTIONAL
        VALUE(end_row)      TYPE i OPTIONAL
        VALUE(end_column)   TYPE i OPTIONAL .

    METHODS display
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_TALV IMPLEMENTATION.


  METHOD display.

    CALL FUNCTION 'ZFM_TALV_DISPLAY'
      EXPORTING
        alv = me.

  ENDMETHOD.


  METHOD display_popup.

    CALL FUNCTION 'ZFM_TALV_DISPLAY'
      EXPORTING
        alv          = me
        start_row    = start_row
        start_column = start_column
        end_row      = end_row
        end_column   = end_column.

  ENDMETHOD.
ENDCLASS.
