// Append to an xstring in a loop with CONCATENATE IN BYTE MODE, then move the result.
export const test50 = `
DATA lv_xs TYPE xstring.
DATA lv_copy TYPE xstring.
DATA lv_x TYPE x LENGTH 1.

DO 100000 TIMES.
  lv_x = sy-index MOD 256.
  CONCATENATE lv_xs lv_x INTO lv_xs IN BYTE MODE.
ENDDO.

DO 1000 TIMES.
  lv_copy = lv_xs.
ENDDO.

ASSERT xstrlen( lv_xs ) = 100000.
ASSERT xstrlen( lv_copy ) = 100000.`;
