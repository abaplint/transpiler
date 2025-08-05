import {ABAP} from ".";

declare const abap: ABAP;

export function buildDbTableName(table: string) {
  let ret = `"${ abap.dbo.tablePrefix + table.trimEnd().toLowerCase()}"`;

  if (abap.dbo.schemaPrefix !== "") {
    ret = `"${abap.dbo.schemaPrefix}".` + ret;
  }
  return ret;
}