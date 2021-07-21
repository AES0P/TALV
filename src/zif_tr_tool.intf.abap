interface ZIF_TR_TOOL
  public .


  types TY_E071 type E071 .
  types:
    tty_e071 TYPE STANDARD TABLE OF ty_e071 WITH DEFAULT KEY .
  types TY_E071K type E071K .
  types:
    tty_e071k TYPE STANDARD TABLE OF ty_e071k WITH DEFAULT KEY .

  methods ASSOCIATE
    returning
      value(IS_OK) type ABAP_BOOL .
  methods ENTRYS
    importing
      !TABLE type TABLE
    returning
      value(TR_TOOL) type ref to ZIF_TR_TOOL .
  methods ENTRY
    importing
      value(OBJNAME) type TROBJ_NAME
      value(OBJFUNC) type OBJFUNC default 'K'
      value(TABKEY) type TROBJ_NAME .
  methods COMMIT
    returning
      value(IS_COMMITED) type ABAP_BOOL .
endinterface.
