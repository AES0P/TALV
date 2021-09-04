*----------------------------------------------------------------------*
***INCLUDE LZTALVTAB002I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHECK_REF_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_ref_info INPUT.

  IF ztalv_fields_set-ref_table IS INITIAL AND ztalv_fields_set-ref_field IS INITIAL.

    CHECK ztalv_fields_set-ddic_type IS NOT INITIAL.
    CHECK ztalv_fields_set-fieldname IS NOT INITIAL.

    ztalv_fields_set-ref_table = ztalv_fields_set-ddic_type.
    ztalv_fields_set-ref_field = ztalv_fields_set-fieldname.

  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_COLNAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_colname INPUT.

  IF ztalv_fields_set-ddic_type IS INITIAL AND ztalv_fields_set-fieldname IS NOT INITIAL.

    IF ztalv_fields_set-coltext IS INITIAL.
      MESSAGE 'coltext must input' TYPE 'E'.
    ENDIF.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_REF_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_ref_field INPUT.


  IF ztalv_fields_set-ref_table IS INITIAL AND ztalv_fields_set-ref_field IS NOT INITIAL.
    MESSAGE 'reference info must input in the same time' TYPE 'E'.
  ENDIF.

  IF ztalv_fields_set-ref_table IS NOT INITIAL AND ztalv_fields_set-ref_field IS INITIAL.
    MESSAGE 'reference info must input in the same time' TYPE 'E'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_COL_POS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_col_pos INPUT.

  IF ( ztalv_fields_set-ref_table IS NOT INITIAL OR ztalv_fields_set-ddic_type IS NOT INITIAL )
    AND ztalv_fields_set-col_pos IS INITIAL.

    SELECT SINGLE position
      FROM dd03l
      INTO ztalv_fields_set-col_pos
     WHERE tabname   IN ( ztalv_fields_set-ref_table,ztalv_fields_set-ddic_type )
       AND fieldname IN ( ztalv_fields_set-ref_field,ztalv_fields_set-fieldname ).

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_COLTEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_coltext INPUT.

  IF ( ztalv_fields_set-ref_table IS NOT INITIAL OR ztalv_fields_set-ddic_type IS NOT INITIAL )
    AND ( ztalv_fields_set-coltext IS INITIAL OR ztalv_fields_set-scrtext_l IS INITIAL ).

    SELECT SINGLE ddtext scrtext_l
      FROM dd03m
      INTO ( ztalv_fields_set-coltext , ztalv_fields_set-scrtext_l )
     WHERE tabname   IN ( ztalv_fields_set-ref_table,ztalv_fields_set-ddic_type )
       AND fieldname IN ( ztalv_fields_set-ref_field,ztalv_fields_set-fieldname ).

  ENDIF.

ENDMODULE.
