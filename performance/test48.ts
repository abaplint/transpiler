// Repeatedly construct nested values with the same composite child type.
// Shared type factories add their calls to this loop's hot path.
export const test48 = `
TYPES: BEGIN OF ty_leaf,
         id TYPE i,
         value TYPE c LENGTH 40,
         extra1 TYPE c LENGTH 40,
         extra2 TYPE c LENGTH 40,
         extra3 TYPE c LENGTH 40,
         extra4 TYPE c LENGTH 40,
         extra5 TYPE c LENGTH 40,
         extra6 TYPE c LENGTH 40,
       END OF ty_leaf.
TYPES: BEGIN OF ty_parent,
         first TYPE ty_leaf,
         second TYPE ty_leaf,
       END OF ty_parent.
DATA result TYPE ty_parent.

DO 100000 TIMES.
  result = VALUE ty_parent(
    first = VALUE ty_leaf( id = sy-index value = 'first' )
    second = VALUE ty_leaf( id = sy-index value = 'second' ) ).
ENDDO.

ASSERT result-first-id = 100000.`;
