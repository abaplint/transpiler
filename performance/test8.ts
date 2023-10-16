export const test8 = `
FORM run.
  CONSTANTS c_max TYPE i VALUE 20000.
  DATA str TYPE string.
  DATA table TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  DO c_max TIMES.
    str = |foobar{ sy-index }|.
    APPEND str TO table.
  ENDDO.
  ASSERT lines( table ) = c_max.

  DO 5000 TIMES. " make sure READ TABLE takes the most time
    READ TABLE table WITH KEY table_line = str TRANSPORTING NO FIELDS BINARY SEARCH.
    ASSERT sy-tabix = c_max.
  ENDDO.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;