export const test43 = `
DATA lt_tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lv_str TYPE string.

DO 1000 TIMES.
  APPEND 'foobar' TO lt_tab.
ENDDO.

DO 1000 TIMES.
  CONCATENATE LINES OF lt_tab INTO lv_str SEPARATED BY ','.
ENDDO.
`;
