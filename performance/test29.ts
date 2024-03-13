export const test29 = `
DATA foo TYPE x LENGTH 65536.
DATA val TYPE x LENGTH 2.
val = 'AABB'.
DO 25000 TIMES.
  foo+1000(2) = val.
ENDDO.`;