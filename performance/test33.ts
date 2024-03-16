export const test33 = `
DATA foo TYPE x LENGTH 8.
DATA bar TYPE x LENGTH 8.
DO 4000000 TIMES.
  IF foo = bar.
  ENDIF.
ENDDO.`;