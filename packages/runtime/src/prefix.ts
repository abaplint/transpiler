export function prefixDbTable(table: string) {
  // @ts-ignore
  const prefix = abap.dbo.schemaPrefix + abap.dbo.tablePrefix;

  return prefix + table;
}