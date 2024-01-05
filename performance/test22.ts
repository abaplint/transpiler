export const test22 = `
TYPES: BEGIN OF ty_data,
         name      TYPE string,
         full_name TYPE string,
       END OF ty_data.

TYPES ty_data_tt TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY
  WITH UNIQUE SORTED KEY key_full_name COMPONENTS full_name.

DATA lt_data TYPE ty_data_tt.
DATA ls_data LIKE LINE OF lt_data.

FORM init.
  DO 10000 TIMES.
    ls_data-name = |a{ sy-index }|.
    ls_data-full_name = |b{ sy-index }|.
    INSERT ls_data INTO TABLE lt_data.
  ENDDO.
ENDFORM.

FORM search.
  DO 400 TIMES.
*    READ TABLE lt_data INTO ls_data WITH KEY full_name = |b9999|.
    READ TABLE lt_data INTO ls_data WITH KEY key_full_name COMPONENTS full_name = 'b9999'.
    ASSERT sy-subrc = 0.
  ENDDO.
ENDFORM.

START-OF-SELECTION.
  PERFORM init.
  PERFORM search.`;