export const test3 = `
FORM run.

  TYPES: BEGIN OF ty_node,
           path  TYPE string,
           name  TYPE string,
           index TYPE i,
           order TYPE i,
         END OF ty_node.

  TYPES:
    ty_nodes_ts TYPE SORTED TABLE OF ty_node
      WITH UNIQUE KEY path name
      WITH NON-UNIQUE SORTED KEY array_index COMPONENTS path index
      WITH NON-UNIQUE SORTED KEY item_order COMPONENTS path order.

  CONSTANTS lc_count TYPE i VALUE 500.
  DATA table TYPE ty_nodes_ts.
  FIELD-SYMBOLS <n> LIKE LINE OF table.
  DATA row LIKE LINE OF table.
  DATA lv_found TYPE i.

  DO 2000 TIMES.
    row-path = 'hello' && sy-index.
    INSERT row INTO TABLE table.
  ENDDO.

  DO lc_count TIMES.
    LOOP AT table ASSIGNING <n> USING KEY array_index WHERE path = 'hello1'.
      lv_found = lv_found + 1.
    ENDLOOP.
  ENDDO.

  ASSERT lv_found = lc_count.

ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;