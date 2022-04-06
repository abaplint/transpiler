import * as types from "./types";
import * as builtin from "./builtin";
import * as compare from "./compare";
import * as operators from "./operators";
import {Statements} from "./statements";
import * as RFC from "./rfc";
import * as DB from "./db";
import {Console} from "./console";
import {UnitTestResult} from "./unit_test";
import {OffsetLength} from "./offset_length";
import {templateFormatting} from "./template_formatting";
import {SQLiteDatabaseClient} from "./sqlite";
import {Context} from "./context";

export {UnitTestResult, RFC, types, DB};

export class ABAP {
// global objects
  public FunctionModules: {[name: string]: any} = {};
  public Classes: {[name: string]: any} = {};
  public Interfaces: {[name: string]: any} = {};
  public DDIC: {[name: string]: any} = {};

// stuff for runtime
  public statements;
  public types = types;
  public builtin = builtin;
  public operators = operators;
  public compare = compare;

  public readonly console: Console;
  public OffsetLength = OffsetLength;
  public templateFormatting = templateFormatting;

  private context: Context;

  public constructor() {
    this.context = new Context();
    this.console = new Console();
    this.context.console = this.console;

    this.statements = new Statements(this.context);

    // todo, this should not be a singleton, it should be part of this instance
    // todo, move to context
    builtin.sy.get().subrc.set(0);
    builtin.sy.get().tabix.set(0);
    builtin.sy.get().index.set(0);
  }

  public async initDB(sql?: string) {
    this.context.db = new SQLiteDatabaseClient();
    await this.context.db.connect();
    await this.context.db.initialize(sql);
  }
}
