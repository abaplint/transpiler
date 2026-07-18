export const test42 = `
DATA int TYPE i.
DATA numc TYPE n LENGTH 10.
int = 42.
numc = 42.

DO 1000000 TIMES.
  IF int = numc.
  ENDIF.
ENDDO.
`;
