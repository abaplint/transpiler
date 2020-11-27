import * as types from "./types";
import * as builtin from "./builtin";
import * as compare from "./compare";
import {Statements} from "./statements";
import initSqlJs from "sql.js";
import {Console} from "./console";
import {UnitTestResult} from "./unit_test";
import {OffsetLength} from "./offset_length";

export {UnitTestResult, OffsetLength};

export class ABAP {
  public statements;
  public types = types;
  public builtin = builtin;
  public compare = compare;
  public FunctionModules = {};
  public console: Console;
  public db: undefined | any;

  public constructor() {
    this.console = new Console();
    this.statements = new Statements(this.console);
  }

  public async initDB(sql?: string) {
    const SQL = await initSqlJs();
    this.db = new SQL.Database();
    if (sql) {
      this.db.run(sql);
    }
  }
}
