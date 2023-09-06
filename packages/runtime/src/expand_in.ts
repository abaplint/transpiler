import {Table} from "./types";

// note: must always return an expression, never return empty string
// https://www.sqlite.org/lang_select.html
export function expandIN(fieldName: string, table: Table) {
  let ret = "";

  if (table.array().length === 0) {
    // " NOT IN ()" does not work on postgres
    // LIKE '%' does not work in snowflake with RTRIM collation
    ret = `true`;
  } else {
    ret = `"${fieldName}" IN (`;
    const values: string[] = [];
    for (const row of table.array()) {
      if (row.get().sign?.get() !== "I" || row.get().option?.get() !== "EQ") {
        throw "Error: IN, only I EQ supported for now";
      }
      values.push("'" + row.get().low?.get().replace(/'/g, "''") + "'");
    }
    ret += values.join(",") + ")";
  }

  return ret;
}