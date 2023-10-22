export const test18 = `
DATA res TYPE string.
res = '456988745621'.
DO 500000 TIMES.
  IF res CO '0123456789'.
  ENDIF.
ENDDO.`;