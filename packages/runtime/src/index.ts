import * as types from "./types";
import * as builtin from "./builtin";
import * as compare from "./compare";
import * as operators from "./operators";
import {Statements} from "./statements";
import initSqlJs from "sql.js";
import {Console} from "./console";
import {UnitTestResult} from "./unit_test";
import {OffsetLength} from "./offset_length";
import {templateFormatting} from "./template_formatting";

export {UnitTestResult};

export class ABAP {
// global objects
  public FunctionModules = {};
  public Classes = {};
  public Interfaces = {};
  public DDIC = {};

// stuff for runtime
  public statements;
  public types = types;
  public builtin = builtin;
  public operators = operators;
  public compare = compare;
  public console: Console;
  public db: undefined | any;
  public OffsetLength = OffsetLength;
  public templateFormatting = templateFormatting;

  public constructor() {
    this.console = new Console();
    this.statements = new Statements(this.console);

    // todo, this should not be a singleton, it should be part of this instance
    builtin.sy.get().subrc.set(0);
    builtin.sy.get().tabix.set(0);
    builtin.sy.get().index.set(0);
  }

  public async initDB(sql?: string) {
    const SQL = await initSqlJs();
    this.db = new SQL.Database();
    if (sql && sql !== "") {
      this.db.run(sql);
    }
    this.statements.setDb(this.db);
  }
}
