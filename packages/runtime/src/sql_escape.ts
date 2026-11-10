// A host variable inlined into SQL text becomes a string literal there, and a
// single quote in its value ends the literal early: 'a'b' is a syntax error,
// and 'x' OR '1'='1' is a condition the ABAP never wrote. A system binds host
// variables and never has this problem; here they travel as text, so the
// quote is doubled, which is how standard SQL spells a quote inside a literal
// (SQLite, PostgreSQL with standard_conforming_strings, HANA). A LIKE pattern
// keeps its % and _ because the ABAP meant them. Not covered: a dialect that
// reads backslash escapes in literals (Snowflake) needs its client to double
// the backslash too, or binding.
export function sqlEscape(value: any): string {
  return String(value).replace(/'/g, "''");
}
