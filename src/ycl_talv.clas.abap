class YCL_TALV definition
  public
  inheriting from YCL_TALV_PARENT
  final
  create public .

public section.

  methods DISPLAY_POPUP
    importing
      value(START_ROW) type I optional
      value(START_COLUMN) type I optional
      value(END_ROW) type I optional
      value(END_COLUMN) type I optional .

  methods DISPLAY
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_TALV IMPLEMENTATION.


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
