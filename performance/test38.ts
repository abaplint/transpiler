export const test38 = `
FORM run.
  CONSTANTS c_max TYPE i VALUE 1000000.
  TYPES: BEGIN OF ty_struct,
           field1 TYPE i,
           field2 TYPE string,
           field3 TYPE c LENGTH 10,
         END OF ty_struct.
  DATA ls_source TYPE ty_struct.
  DATA ls_target TYPE ty_struct.
  DATA lt_tab TYPE STANDARD TABLE OF ty_struct WITH DEFAULT KEY.

  ls_source-field1 = 42.
  ls_source-field2 = 'hello'.
  ls_source-field3 = 'abc'.

  DO c_max TIMES.
    ls_target = ls_source.
    APPEND ls_target TO lt_tab.
    IF sy-index MOD 1000 = 0.
      CLEAR lt_tab.
    ENDIF.
  ENDDO.

  ASSERT ls_target-field1 = 42.
  ASSERT ls_target-field2 = 'hello'.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
