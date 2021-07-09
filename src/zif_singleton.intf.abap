INTERFACE zif_singleton
  PUBLIC .


  CLASS-METHODS get_instance
    RETURNING
      VALUE(instance) TYPE REF TO object .
ENDINTERFACE.
