// Repeatedly allocate a deeper structure with a nested table of composite rows.
export const test49 = `
TYPES: BEGIN OF ty_payload,
         id TYPE i,
         value TYPE c LENGTH 40,
         extra1 TYPE c LENGTH 40,
         extra2 TYPE c LENGTH 40,
         extra3 TYPE c LENGTH 40,
       END OF ty_payload.
TYPES: BEGIN OF ty_branch,
         left TYPE ty_payload,
         right TYPE ty_payload,
       END OF ty_branch.
TYPES ty_branch_table TYPE STANDARD TABLE OF ty_branch WITH DEFAULT KEY.
TYPES: BEGIN OF ty_envelope,
         primary TYPE ty_branch,
         secondary TYPE ty_branch,
         branches TYPE ty_branch_table,
       END OF ty_envelope.
DATA result TYPE ty_envelope.
DATA result_branch TYPE ty_branch.

DO 10000 TIMES.
  result = VALUE ty_envelope(
    primary = VALUE ty_branch(
      left = VALUE ty_payload( id = sy-index value = 'primary-left' )
      right = VALUE ty_payload( id = sy-index value = 'primary-right' ) )
    secondary = VALUE ty_branch(
      left = VALUE ty_payload( id = sy-index value = 'secondary-left' )
      right = VALUE ty_payload( id = sy-index value = 'secondary-right' ) )
    branches = VALUE ty_branch_table(
      ( left = VALUE ty_payload( id = sy-index value = 'table-one-left' )
        right = VALUE ty_payload( id = sy-index value = 'table-one-right' ) )
      ( left = VALUE ty_payload( id = sy-index value = 'table-two-left' )
        right = VALUE ty_payload( id = sy-index value = 'table-two-right' ) ) ) ).
ENDDO.

ASSERT result-primary-left-id = 10000.
READ TABLE result-branches INDEX 1 INTO result_branch.
ASSERT result_branch-left-id = 10000.`;
