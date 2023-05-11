import {Context} from "./context";
import {OffsetLength} from "./offset_length";
import {Statements} from "./statements";
import {templateFormatting} from "./template_formatting";
import {UnitTestResult} from "./unit_test";
import * as builtin from "./builtin";
import * as compare from "./compare";
import * as DB from "./db/db";
import * as operators from "./operators";
import * as RFC from "./rfc";
import * as types from "./types";
import {expandIN} from "./expand_in";
import {expandDynamic} from "./expand_dynamic";
import {ClassicError} from "./classic_error";
import {Console} from "./console/console";
import {StandardOutConsole} from "./console/standard_out_console";
import {MemoryConsole} from "./console/memory_console";

export {UnitTestResult, RFC, types, DB, MemoryConsole};

export class ABAP {
// global objects
  public FunctionModules: {[name: string]: any} = {};
  public Classes: {[name: string]: any} = {};
  public Interfaces: {[name: string]: any} = {};
  public DDIC: {[name: string]: any} = {};
  public TypePools: {[name: string]: any} = {};
  public SMIM: {[name: string]: any} = {};
  public W3MI: {[name: string]: any} = {};

// stuff for runtime
  public statements;
  public types = types;
  public builtin = builtin;
  public operators = operators;
  public compare = compare;

  public readonly console: Console;
  public OffsetLength = OffsetLength;
  public templateFormatting = templateFormatting;
  public expandIN = expandIN;
  public expandDynamic = expandDynamic;
  public ClassicError = ClassicError;

  public readonly context: Context;

  public constructor(console?: Console) {
    this.context = new Context();
    this.console = console ? console : new StandardOutConsole();
    this.context.console = this.console;

    this.statements = new Statements(this.context);

    // todo, this should not be a singleton, it should be part of this instance
    // todo, move to context
    builtin.sy.get().subrc.set(0);
    builtin.sy.get().tabix.set(0);
    builtin.sy.get().index.set(0);
    this.statements.getTime({sy: builtin.sy});
  }
}
