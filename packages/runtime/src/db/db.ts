import {QueryExecResult, Statement} from "sql.js";

export interface DatabaseClient {
  connect(): Promise<void>;
  initialize(sql?: string): Promise<void>;
  disconnect(): Promise<void>;
  exec(sql: string): QueryExecResult[]; // todo, refactor
  prepare(sql: string): Statement; // todo, refactor

  delete(table: string, where: string): {subrc: number, dbcnt: number};
}