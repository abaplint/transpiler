import {Table} from "./types";

function toSQLValue(value: any): string {
  if (typeof value === "number") {
    return value.toString();
  }
  return `'` + String(value ?? "").replace(/'/g, "''") + "'";
}

// one row of a ranges table as a condition that is true when the row matches
function rowCondition(field: string, sign: string, option: string, low: any, high: any): string {
  const lowValue = toSQLValue(low);
  switch (option) {
    case "EQ":
      return `${field} = ${lowValue}`;
    case "NE":
      return `${field} <> ${lowValue}`;
    case "GT":
      return `${field} > ${lowValue}`;
    case "GE":
      return `${field} >= ${lowValue}`;
    case "LT":
      return `${field} < ${lowValue}`;
    case "LE":
      return `${field} <= ${lowValue}`;
    case "BT":
      return `(${field} >= ${lowValue} AND ${field} <= ${toSQLValue(high)})`;
    case "NB":
      return `NOT (${field} >= ${lowValue} AND ${field} <= ${toSQLValue(high)})`;
    case "CP":
    case "NP":
    {
      if (sign !== "I" || option !== "CP") {
        // the pattern rules come with a separate change; until then only I CP, as before
        throw new Error(`IN, ${sign} ${option} not supported`);
      }
      const like = `${field} LIKE '` + String(low ?? "").trimEnd().replace(/'/g, "''").replace(/\*/g, "%") + "'";
      return option === "CP" ? like : `NOT ${like}`;
    }
    default:
      throw new Error(`IN, ${sign} ${option} not supported`);
  }
}

// note: must always return an expression, never return empty string
// https://www.sqlite.org/lang_select.html
// The rows with SIGN I are ORed, AND NOT the OR of the rows with SIGN E; a
// table with E rows only excludes them from everything.
export function expandIN(fieldName: string, table: Table) {
  if (table.array().length === 0) {
    // " NOT IN ()" does not work on postgres
    // LIKE '%' does not work in snowflake with RTRIM collation
    return `true`;
  }

  const field = `"${fieldName.replace("~", `"."`)}"`;
  const include: string[] = [];
  const exclude: string[] = [];
  for (const row of table.array()) {
    const sign = row.get().sign?.get();
    const option = row.get().option?.get();
    if (sign !== "I" && sign !== "E") {
      throw new Error(`IN, ${sign} ${option} not supported`);
    }
    const cond = rowCondition(field, sign, option, row.get().low?.get(), row.get().high?.get());
    if (sign === "I") {
      include.push(cond);
    } else {
      exclude.push(cond);
    }
  }

  const parts: string[] = [];
  if (include.length > 0) {
    parts.push("(" + include.join(" OR ") + ")");
  }
  if (exclude.length > 0) {
    parts.push("NOT (" + exclude.join(" OR ") + ")");
  }
  return "(" + parts.join(" AND ") + ")";
}
