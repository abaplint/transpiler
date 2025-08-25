import {append} from "./append";
import {assert} from "./assert";
import {assign} from "./assign";
import {commit} from "./commit";
import {concatenate} from "./concatenate";
import {condense} from "./condense";
import {convert} from "./convert";
import {createData} from "./create_data";
import {deleteInternal} from "./delete_internal";
import {describe} from "./describe";
import {find} from "./find";
import {collect} from "./collect";
import {overlay} from "./overlay";
import {cast} from "./cast";
import {getBit} from "./get_bit";
import {readReport} from "./read_report";
import {getReference} from "./get_reference";
import {raiseEvent} from "./raise_event";
import {receive} from "./receive";
import {getLocale} from "./get_locale";
import {unpack} from "./unpack";
import {getParameter} from "./get_parameter";
import {setLocale} from "./set_locale";
import {getRunTime} from "./get_run_time";
import {getTime} from "./get_time";
import {IInsertDatabaseOptions, insertDatabase} from "./insert_database";
import {insertInternal} from "./insert_internal";
import {deleteDatabase, IDeleteDatabaseOptions} from "./delete_database";
import {loop} from "./loop";
import {IMessageOptions, MessageStatement} from "./message";
import {IModifyDatabaseOptions, modifyDatabase} from "./modify_database";
import {modifyInternal} from "./modify_internal";
import {moveCorresponding} from "./move_corresponding";
import {readTable} from "./read_table";
import {replace} from "./replace";
import {rollback} from "./rollback";
import {select as selectDB} from "./select";
import {setBit} from "./set_bit";
import {shift} from "./shift";
import {sort} from "./sort";
import {wait} from "./wait";
import {fetchNextCursor} from "./fetch_next_cursor";
import {IOpenCursorDatabaseOptions, openCursor} from "./open_cursor";
import {closeCursor} from "./close_cursor";
import {setHandler} from "./set_handler";
import {split} from "./split";
import {translate} from "./translate";
import {callTransaction} from "./call_transaction";
import {IUpdateDatabaseOptions, updateDatabase} from "./update_database";
import {IWriteOptions, WriteStatement} from "./write";
import {Context} from "../context";
import {ICharacter} from "../types/_character";
import {FieldSymbol, Structure, Table} from "../types";
import {INumeric} from "../types/_numeric";
import {CallFunction, ICallFunctionOptions} from "./call_function";
import {SelectDatabaseOptions, SelectRuntimeOptions} from "../db/db";
import {Trace} from "../trace";

// this is a class, as statements like SELECT needs access to the database object instance
// and WRITE will access the Console
export class Statements {
  public append = append;
  public assert = assert;
  public assign = assign;
  public cast = cast;
  public collect = collect;
  public commit = commit;
  public concatenate = concatenate;
  public condense = condense;
  public convert = convert;
  public createData = createData;
  public deleteInternal = deleteInternal;
  public describe = describe;
  public find = find;
  public unpack = unpack;
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
  public getReference = getReference;
  public shift = shift;
  public sort = sort;
  public split = split;
  public translate = translate;
  public wait = wait;
  public receive = receive;
  public callTransaction = callTransaction;

  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  public _setTrace(min = 10, totals = false) {
    return new Trace().setTrace(min, totals, this);
  }

  public async openCursor(target: INumeric, select: string, options: IOpenCursorDatabaseOptions) {
    const num = await openCursor(this.context, select, options);
    target.set(num);
  }

  public async fetchNextCursor(cursor: INumeric, target: any, packageSize?: INumeric) {
    await fetchNextCursor(this.context, cursor.get(), target, packageSize?.get() || 0);
  }

  public async closeCursor(cursor: INumeric) {
    await closeCursor(this.context, cursor.get());
  }

  public async deleteDatabase(table: string | ICharacter, options: IDeleteDatabaseOptions) {
    await deleteDatabase(table, options, this.context);
  }

  public async insertDatabase(table: string | ICharacter, options: IInsertDatabaseOptions) {
    return insertDatabase(table, options, this.context);
  }

  public async modifyDatabase(table: string | ICharacter, options: IModifyDatabaseOptions) {
    return modifyDatabase(table, options, this.context);
  }

  public async select(target: Structure | Table | FieldSymbol, select: SelectDatabaseOptions, runtimeOptions?: SelectRuntimeOptions) {
    return selectDB(target, select, runtimeOptions || {}, this.context);
  }

  public async updateDatabase(table: string | ICharacter, options: IUpdateDatabaseOptions) {
    return updateDatabase(table, options, this.context);
  }

  public async callFunction(options: ICallFunctionOptions) {
    return new CallFunction(this.context).callFunction(options);
  }

  public async message(options: IMessageOptions) {
    return new MessageStatement(this.context).message(options);
  }

  public write(source: INumeric | ICharacter | FieldSymbol | string | number, options?: IWriteOptions) {
    return new WriteStatement(this.context).write(source, options);
  }

}
