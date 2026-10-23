/** one CREATE statement per dialect for one transparent table or database view */
export type DatabaseSetupTable = {
  /** upper case object name */
  name: string,
  type: "TABL" | "VIEW",
  sqlite: string,
  pg: string,
  hdb: string,
  snowflake: string,
};

/** one INSERT statement and the table it targets */
export type DatabaseSetupInsert = {
  /** upper case table name */
  table: string,
  sql: string,
};

export type DatabaseSetupResult = {
  schemas: {
    sqlite: string[],
    pg: string[],
    hdb: string[],
    snowflake: string[],
  },
  /** the statements of `schemas` per table and view, CDS views are only in `schemas` */
  tables: DatabaseSetupTable[],
  insert: string[],
  /** the statements of `insert` with the table each targets, same order */
  inserts: DatabaseSetupInsert[],
};
