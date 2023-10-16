export const test1 = `
FORM run.
  CONSTANTS c_max TYPE i VALUE 2000000.
  DATA lv_index TYPE i.
  DATA table TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DO c_max TIMES.
    APPEND sy-index TO table.
  ENDDO.
  ASSERT lines( table ) = c_max.
  WHILE lines( table ) > 0.
    lv_index = lines( table ).
    DELETE table INDEX lv_index.
  ENDWHILE.
  ASSERT lines( table ) = 0.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;