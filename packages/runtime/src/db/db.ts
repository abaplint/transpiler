import {QueryExecResult, Statement} from "sql.js";

export interface DatabaseClient {
  initialize(sql?: string): Promise<void>;
  connect(): Promise<void>;
  disconnect(): Promise<void>;

  exec(sql: string): QueryExecResult[]; // todo, refactor
  prepare(sql: string): Statement; // todo, refactor

  delete(table: string, where: string): {subrc: number, dbcnt: number};
  update(table: string, where: string, set: string[]): {subrc: number, dbcnt: number};
}