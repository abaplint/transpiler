export const test20 = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty1,
             field1 TYPE c LENGTH 1,
             field2 TYPE c LENGTH 1,
             field3 TYPE c LENGTH 1,
             field4 TYPE c LENGTH 1,
             field5 TYPE c LENGTH 1,
           END OF ty1.
    CLASS-METHODS foo IMPORTING bar TYPE ty1.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA dat TYPE lcl=>ty1.
  DO 200000 TIMES.
    lcl=>foo( dat ).
  ENDDO.`;