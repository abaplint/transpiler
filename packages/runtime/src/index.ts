import * as types from "./types";
import * as builtin from "./builtin";
import * as compare from "./compare";
import * as statements from "./statements";
import initSqlJs from "sql.js";
import {Console} from "./console";
import {UnitTestResult} from "./unit_test";
import {OffsetLength} from "./offset_length";

const FunctionModules = {};

async function initDB(sql?: string) {
  const SQL = await initSqlJs();
  const db = new SQL.Database();
  if (sql) {
    db.run(sql);
  }
  return db;
}

export {
  types, statements, builtin, compare,
  FunctionModules, initDB,
  Console, UnitTestResult, OffsetLength};