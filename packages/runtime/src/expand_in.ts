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
    ret = `(`;
    const values: string[] = [];
    fieldName = fieldName.replace("~", `"."`);
    for (const row of table.array()) {
      if (row.get().sign?.get() === "I" && row.get().option?.get() === "EQ") {
        values.push(`"${fieldName}" = '` + row.get().low?.get().replace(/'/g, "''") + "'");
      } else if (row.get().sign?.get() === "I" && row.get().option?.get() === "NE") {
        values.push(`"${fieldName}" <> '` + row.get().low?.get().replace(/'/g, "''") + "'");
      } else if (row.get().sign?.get() === "I" && row.get().option?.get() === "CP") {
        values.push(`"${fieldName}" LIKE '` + row.get().low?.get().trimEnd().replace(/'/g, "''").replace(/\*/g, "%") + "'");
      } else {
        throw new Error(`IN, ${row.get().sign?.get()} ${row.get().option?.get()} not supported`);
      }
    }
    ret += values.join(" OR ") + ")";
  }

  return ret;
}