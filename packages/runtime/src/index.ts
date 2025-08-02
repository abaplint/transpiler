import {Context} from "./context";
import {OffsetLength} from "./offset_length";
import {Statements} from "./statements";
import {templateFormatting} from "./template_formatting";
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
import {buildDbTableName} from "./prefix";
import {IntegerFactory} from "./integer_factory";
import {dynamicCallLookup} from "./dynamic_call_lookup";
import {CharacterFactory} from "./character_factory";
import {ABAPEvent} from "./abap_event";

export {RFC, types, DB, MemoryConsole};

export type RuntimeDatabaseOptions = {
  /* prefix all operations with schema*/
  schemaPrefix?: string,
  /* prefix all database tables */
  tablePrefix?: string,
  /* map field names to case sensitive, postgres is case sensitive, and ABAP
     can have special characters in table names, "field" must be in lower case */
  // fieldNameMap?: {[table: string]: {[field: string]: string}},
};

export type RuntimeOptions = {
  console?: Console,
  database?: RuntimeDatabaseOptions,
};

export class ABAP {
// global objects
  public Classes: {[name: string]: any} = {};
  public Forms: {[name: string]: any} = {};
  public DDIC: {[name: string]: any} = {};
  public FunctionModules: {[name: string]: any} = {};
  public Interfaces: {[name: string]: any} = {};
  public MSAG: {[name: string]: any} = {};
  public OA2P: {[name: string]: any} = {};
  public SMIM: {[name: string]: any} = {};
  public TypePools: {[name: string]: any} = {};
  public W3MI: {[name: string]: any} = {};

  public internalIdCounter = 1;

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
  public buildDbTableName = buildDbTableName;
  public ClassicError = ClassicError;
  public dynamicCallLookup = dynamicCallLookup;
  public ABAPEvent = ABAPEvent;

  public IntegerFactory = IntegerFactory;
  public CharacterFactory = CharacterFactory;

  public readonly context: Context;
  public readonly dbo: RuntimeDatabaseOptions;

  public constructor(input?: RuntimeOptions) {
    this.context = new Context();
    this.console = input?.console ? input?.console : new StandardOutConsole();
    this.context.console = this.console;

    this.dbo = input?.database || {schemaPrefix: "", tablePrefix: ""};
    if (this.dbo.schemaPrefix === undefined) {
      this.dbo.schemaPrefix = "";
    }
    if (this.dbo.tablePrefix === undefined) {
      this.dbo.tablePrefix = "";
    }

    this.statements = new Statements(this.context);

    // todo, this should not be a singleton, it should be part of this instance
    // todo, move to context
    builtin.sy.get().subrc.set(0);
    builtin.sy.get().tabix.set(0);
    builtin.sy.get().index.set(0);
    this.statements.getTime({sy: builtin.sy});
  }
}
