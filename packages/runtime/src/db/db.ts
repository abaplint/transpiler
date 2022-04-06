import {QueryExecResult, Statement} from "sql.js";

// todo defined inputs and returns?

export interface DatabaseClient {
  initialize(sql?: string): Promise<void>;
  connect(): Promise<void>;
  disconnect(): Promise<void>;

  prepare(sql: string): Statement; // todo, refactor

  delete(table: string, where: string): {subrc: number, dbcnt: number};
  update(table: string, where: string, set: string[]): {subrc: number, dbcnt: number};
  insert(table: string, columns: string[], values: string[]): {subrc: number, dbcnt: number};
  select(select: string): {result: QueryExecResult[]};
}