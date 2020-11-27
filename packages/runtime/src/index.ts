import * as types from "./types";
import * as builtin from "./builtin";
import * as compare from "./compare";
import {Statements} from "./statements";
import initSqlJs from "sql.js";
import {Console} from "./console";
import {UnitTestResult} from "./unit_test";
import {OffsetLength} from "./offset_length";

export {UnitTestResult};

export class ABAP {
  public statements;
  public types = types;
  public builtin = builtin;
  public compare = compare;
  public FunctionModules = {};
  public console: Console;
  public db: undefined | any;
  public OffsetLength = OffsetLength;

  public constructor() {
    this.console = new Console();
    this.statements = new Statements(this.console);
    builtin.sy.get().subrc.set(0); // todo, this should not be a singleton, it should be part of this instance
  }

  public async initDB(sql?: string) {
    const SQL = await initSqlJs();
    this.db = new SQL.Database();
    if (sql) {
      this.db.run(sql);
    }
    this.statements.setDb(this.db);
  }
}
