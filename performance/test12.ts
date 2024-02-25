export const test12 = `
TYPES: BEGIN OF ty,
         field1 TYPE string,
         field2 TYPE i,
       END OF ty.
DATA tab TYPE HASHED TABLE OF ty WITH UNIQUE KEY field1 field2.
DATA row LIKE LINE OF tab.
FIELD-SYMBOLS <fs> LIKE LINE OF tab.


DO 1000 TIMES.
  row-field1 = |foo{ sy-index }|.
  row-field2 = sy-index.
  INSERT row INTO TABLE tab.
ENDDO.

DO 200 TIMES.
  DO 1000 TIMES.
    READ TABLE tab WITH KEY
      field1 = |foo{ sy-index }|
      field2 = sy-index
      ASSIGNING <fs>.
    ASSERT sy-subrc = 0.
  ENDDO.
ENDDO.`;