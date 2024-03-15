export const test30 = `
DATA foo TYPE x LENGTH 65536.
DATA val TYPE x LENGTH 2.
DO 50000 TIMES.
  val = foo+1000(2).
ENDDO.`;