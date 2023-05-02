import {Console} from "./console.js";
import {Context} from "./context.js";
import {OffsetLength} from "./offset_length.js";
import {Statements} from "./statements/index.js";
import {templateFormatting} from "./template_formatting.js";
import {UnitTestResult} from "./unit_test.js";
import * as builtin from "./builtin/index.js";
import * as compare from "./compare/index.js";
import * as DB from "./db/db.js";
import * as operators from "./operators/index.js";
import * as RFC from "./rfc.js";
import * as types from "./types/index.js";
import {expandIN} from "./expand_in.js";
import {expandDynamic} from "./expand_dynamic.js";
import {ClassicError} from "./classic_error.js";

export {UnitTestResult, RFC, types, DB};

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
    this.statements.getTime({sy: builtin.sy});
  }
}
