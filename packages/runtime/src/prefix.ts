export function buildDbTableName(table: string) {
  // @ts-ignore
  let ret = `"${ abap.dbo.tablePrefix + table.trimEnd().toLowerCase()}"`;

  // @ts-ignore
  if (abap.dbo.schemaPrefix !== "") {
    // @ts-ignore
    ret = `"${abap.dbo.schemaPrefix}".` + ret;
  }
  return ret;
}