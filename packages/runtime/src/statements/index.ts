import {append} from "./append.js";
import {assert} from "./assert.js";
import {assign} from "./assign.js";
import {clear} from "./clear.js";
import {commit} from "./commit.js";
import {concatenate} from "./concatenate.js";
import {condense} from "./condense.js";
import {convert} from "./convert.js";
import {createData} from "./create_data.js";
import {deleteInternal} from "./delete_internal.js";
import {describe} from "./describe.js";
import {find} from "./find.js";
import {collect} from "./collect.js";
import {overlay} from "./overlay.js";
import {cast} from "./cast.js";
import {getBit} from "./get_bit.js";
import {readReport} from "./read_report.js";
import {raiseEvent} from "./raise_event.js";
import {getLocale} from "./get_locale.js";
import {getParameter} from "./get_parameter.js";
import {setLocale} from "./set_locale.js";
import {getRunTime} from "./get_run_time.js";
import {getTime} from "./get_time.js";
import {IInsertDatabaseOptions, InsertDatabase} from "./insert_database.js";
import {insertInternal} from "./insert_internal.js";
import {DeleteDatabase, IDeleteDatabaseOptions} from "./delete_database.js";
import {loop} from "./loop.js";
import {IMessageOptions, MessageStatement} from "./message.js";
import {IModifyDatabaseOptions, ModifyDatabase} from "./modify_database.js";
import {modifyInternal} from "./modify_internal.js";
import {moveCorresponding} from "./move_corresponding.js";
import {readTable} from "./read_table.js";
import {replace} from "./replace.js";
import {rollback} from "./rollback.js";
import {SelectDatabase} from "./select.js";
import {setBit} from "./set_bit.js";
import {shift} from "./shift.js";
import {sort} from "./sort.js";
import {setHandler} from "./set_handler.js";
import {split} from "./split.js";
import {translate} from "./translate.js";
import {IUpdateDatabaseOptions, UpdateDatabase} from "./update_database.js";
import {IWriteOptions, WriteStatement} from "./write.js";
import {Context} from "../context.js";
import {ICharacter} from "../types/_character.js";
import {FieldSymbol, Structure, Table} from "../types/index.js";
import {INumeric} from "../types/_numeric.js";
import {CallFunction, ICallFunctionOptions} from "./call_function.js";
import {SelectDatabaseOptions, SelectRuntimeOptions} from "../db/db.js";
import {isAsyncFunction} from "util/types";

// this is a class, as statements like SELECT needs access to the database object instance
// and WRITE will access the Console
export class Statements {
  public append = append;
  public assert = assert;
  public assign = assign;
  public cast = cast;
  public clear = clear;
  public collect = collect;
  public commit = commit;
  public concatenate = concatenate;
  public condense = condense;
  public convert = convert;
  public createData = createData;
  public deleteInternal = deleteInternal;
  public describe = describe;
  public find = find;
  public getBit = getBit;
  public readReport = readReport;
  public getLocale = getLocale;
  public getParameter = getParameter;
  public getRunTime = getRunTime;
  public getTime = getTime;
  public insertInternal = insertInternal;
  public loop = loop;
  public modifyInternal = modifyInternal;
  public moveCorresponding = moveCorresponding;
  public overlay = overlay;
  public raiseEvent = raiseEvent;
  public readTable = readTable;
  public replace = replace;
  public rollback = rollback;
  public setBit = setBit;
  public setHandler = setHandler;
  public setLocale = setLocale;
  public shift = shift;
  public sort = sort;
  public split = split;
  public translate = translate;

  private readonly context: Context;
  private readonly traceTotals: {[name: string]: number};

  public constructor(context: Context) {
    this.context = context;
    this.traceTotals = {};
  }

  private _trace(func: any, name: string, min: number, totals: boolean) {
    const tt = this.traceTotals;
    const exec = (...options: any[]) => {
      const start = Date.now();
      const result = func.bind(this)(...options);
      const runtime = Date.now() - start;
      if (totals === true) {
        if (tt[name] === undefined) {
          tt[name] = 0;
        }
        tt[name] += runtime;
      }
      if (runtime >= min) {
        console.log(`STATEMENT: ${name}, ${runtime} ms`);
        if (totals === true) {
          console.log(JSON.stringify(tt));
        }
      }
      return result;
    };
    return exec;
  }

  private _traceAsync(func: any, name: string, min: number, totals: boolean) {
    const tt = this.traceTotals;
    const exec = async (...options: any[]) => {
      const start = Date.now();
      const result = await func.bind(this)(...options);
      const runtime = Date.now() - start;
      if (totals === true) {
        if (tt[name] === undefined) {
          tt[name] = 0;
        }
        tt[name] += runtime;
      }
      if (runtime >= min) {
        console.log(`STATEMENT: ${name}, ${runtime} ms`);
        if (totals === true) {
          console.log(JSON.stringify(tt));
        }
      }
      return result;
    };
    return exec;
  }

  public _setTrace(min = 10, totals = false) {
    const candidates = [...Object.keys(this),...Object.getOwnPropertyNames(Statements.prototype)];
    for (const c of candidates) {
      if (c === "context" || c === "constructor" || c.startsWith("_") || c === "loop") {
        continue;
      }
      const func = (this as any)[c];
      if (isAsyncFunction(func)) {
        (this as any)[c] = this._traceAsync(func, c, min, totals);
      } else {
        (this as any)[c] = this._trace(func, c, min, totals);
      }
    }
  }

  public async deleteDatabase(table: string | ICharacter, options: IDeleteDatabaseOptions) {
    return new DeleteDatabase(this.context).deleteDatabase(table, options);
  }

  public async insertDatabase(table: string | ICharacter, options: IInsertDatabaseOptions) {
    return new InsertDatabase(this.context).insertDatabase(table, options);
  }

  public async message(options: IMessageOptions) {
    return new MessageStatement(this.context).message(options);
  }

  public async modifyDatabase(table: string | ICharacter, options: IModifyDatabaseOptions) {
    return new ModifyDatabase(this.context).modifyDatabase(table, options);
  }

  public async select(target: Structure | Table | FieldSymbol, select: SelectDatabaseOptions, runtimeOptions?: SelectRuntimeOptions) {
    return new SelectDatabase(this.context).select(target, select, runtimeOptions);
  }

  public async updateDatabase(table: string | ICharacter, options: IUpdateDatabaseOptions) {
    return new UpdateDatabase(this.context).updateDatabase(table, options);
  }

  public async callFunction(options: ICallFunctionOptions) {
    return new CallFunction(this.context).callFunction(options);
  }

  public write(source: INumeric | ICharacter | FieldSymbol | string | number, options?: IWriteOptions) {
    return new WriteStatement(this.context).write(source, options);
  }

}
