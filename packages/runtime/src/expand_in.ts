import {Table} from "./types/table.js";

// note: must always return an expression, never return empty string
export function expandIN(fieldName: string, table: Table) {
  let ret = "";

  if (table.array().length === 0) {
    ret = fieldName + " NOT IN ()";
  } else {
    ret = fieldName + " IN (";
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