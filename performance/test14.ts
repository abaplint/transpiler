export const test14 = `
DATA: BEGIN OF data,
        field1 TYPE string,
        field2 TYPE string,
      END OF data.
DO 1000000 TIMES.
  data-field1 = 'FOO'.
  data-field2 = 'BAR'.
ENDDO.`;