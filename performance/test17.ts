export const test17 = `
DATA mv_stack_path TYPE string.
DATA res TYPE string.
mv_stack_path = '/foo/bar/moo'.
DO 500000 TIMES.
  res = substring(
    val = mv_stack_path
    len = find( val = mv_stack_path sub = '/' occ = -2 ) + 1 ).
ENDDO.`;