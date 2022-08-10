export type DatabaseSetupResult = {
  schemas: {
    sqlite: string[],
    pg: string[],
    hdb: string[],
  },
  insert: string[],
};