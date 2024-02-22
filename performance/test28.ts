export const test28 = `
DATA lv_hex32 TYPE x LENGTH 32.
DATA mv_linear TYPE xstring.
DATA gv_empty_page TYPE xstring.

lv_hex32 = '0000000000000000000000000000000000000000000000000000000000000000'.
DO 2048 TIMES.
  CONCATENATE gv_empty_page lv_hex32 INTO gv_empty_page IN BYTE MODE.
ENDDO.

DO 200 TIMES.
  CONCATENATE mv_linear gv_empty_page INTO mv_linear IN BYTE MODE.
ENDDO.`;