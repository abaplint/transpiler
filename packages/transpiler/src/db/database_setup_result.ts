export type DatabaseSetupResult = {
  schemas: {
    sqlite: string[],
    pg: string[],
    hdb: string[],
    snowflake: string[],
  },
  insert: string[],
};