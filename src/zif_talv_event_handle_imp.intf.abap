interface ZIF_TALV_EVENT_HANDLE_IMP
  public .


  methods ON_HANDLE_TOOLBAR
    for event TOOLBAR of ZCL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE .
  methods ON_HANDLE_USER_COMMAND
    for event USER_COMMAND of ZCL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods ON_HANDLE_HOTSPOT_CLICK
    for event HOTSPOT_CLICK of ZCL_GUI_ALV_GRID
    importing
      !E_ROW_ID
      !E_COLUMN_ID
      !ES_ROW_NO .
  methods ON_HANDLE_DOUBLE_CLICK
    for event DOUBLE_CLICK of ZCL_GUI_ALV_GRID
    importing
      !E_ROW
      !E_COLUMN .
  methods ON_HANDLE_DATA_CHANGED
    for event DATA_CHANGED of ZCL_GUI_ALV_GRID
    importing
      !ER_DATA_CHANGED
      !E_ONF4
      !E_ONF4_BEFORE
      !E_ONF4_AFTER
      !E_UCOMM .
  methods ON_HANDLE_CHANGED_FINISHED
    for event DATA_CHANGED_FINISHED of ZCL_GUI_ALV_GRID
    importing
      !E_MODIFIED
      !ET_GOOD_CELLS .
  methods ON_HANDLE_COUNTDOWN_FINISHED
    for event FINISHED of CL_GUI_TIMER .
  methods ON_HANDLE_TOP_OF_PAGE
    for event TOP_OF_PAGE of ZCL_GUI_ALV_GRID
    importing
      !E_DYNDOC_ID
      !TABLE_INDEX .
  methods ON_SET_PF_STATUS
    for event SET_PF_STATUS of ZCL_TALV_PARENT .
  methods ON_SET_TITLE
    for event SET_TITLE of ZCL_TALV_PARENT .
  methods ON_PBO
    for event ON_PBO of ZCL_TALV_PARENT .
  methods ON_PAI_COMMAND
    for event PAI_COMMAND of ZCL_TALV_PARENT
    importing
      !E_UCOMM .
  methods ON_EXIT
    for event ON_EXIT of ZCL_TALV_PARENT .
  methods ON_RETRIEVE
    for event RETRIEVE of ZCL_TALV_PARENT .
  methods ON_HANDLE_GRID_DISPATCH
    for event GRID_DISPATCH of ZCL_GUI_ALV_GRID
    importing
      !ACTION .
  methods ON_HANDLE_LINE_BUTTON_CLICK
    for event BUTTON_CLICK of ZCL_GUI_ALV_GRID
    importing
      !ES_COL_ID
      !ES_ROW_NO .
  methods ON_CHECK_LONG_TEXT
    for event CHECK_LONG_TEXT of ZCL_TALV_PARENT
    importing
      !FIELDNAME
      !ROW
      !LONG_TEXT .
endinterface.
