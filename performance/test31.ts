export const test31 = `
DATA foo TYPE x LENGTH 8.
DO 2500000 TIMES.
  SET BIT 18 OF foo TO 1.
ENDDO.`;