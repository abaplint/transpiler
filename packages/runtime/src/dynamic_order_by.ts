// The text of a dynamic ORDER BY, as SQL takes it.
//
// Open SQL separates the columns of an ORDER BY with blanks or with commas:
// `ORDER BY (lv)` with lv = `s~so_id i~so_item_pos` or `a DESCENDING b` is
// legal, and so is `a, b`. SQL wants the commas. The static clause gets them
// from the transpiler (SQLOrderByTranspiler); a dynamic one is only known at
// runtime, so it is normalised here: every column is one item, an
// ASCENDING / DESCENDING (or ASC / DESC) and a NULLS FIRST / NULLS LAST
// belong to the column before them, and the items are joined with ", ".
// A text that is only PRIMARY KEY is left as it is, the database clients
// expand it. A text holding a parenthesis or a quote is not a plain column
// list and is passed on untouched.

const DIRECTIONS = new Set(["ASCENDING", "DESCENDING", "ASC", "DESC"]);
const NULLS = new Set(["FIRST", "LAST"]);

export function dynamicOrderBy(text: string): string {
  const trimmed = text.trim();
  if (trimmed === "" || /^PRIMARY\s+KEY$/i.test(trimmed) || /[()'"`]/.test(trimmed)) {
    return text;
  }
  const items: string[] = [];
  for (const group of trimmed.split(",")) {
    const tokens = group.trim().split(/\s+/).filter(token => token !== "");
    for (let i = 0; i < tokens.length; i++) {
      const upper = tokens[i].toUpperCase();
      if (items.length > 0 && DIRECTIONS.has(upper)) {
        items[items.length - 1] += " " + tokens[i];
      } else if (items.length > 0 && upper === "NULLS" && NULLS.has(tokens[i + 1]?.toUpperCase())) {
        items[items.length - 1] += " " + tokens[i] + " " + tokens[i + 1];
        i++;
      } else {
        items.push(tokens[i]);
      }
    }
  }
  return items.join(", ");
}
