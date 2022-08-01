import {append} from "./append";
import {assert} from "./assert";
import {assign} from "./assign";
import {clear} from "./clear";
import {commit} from "./commit";
import {concatenate} from "./concatenate";
import {condense} from "./condense";
import {convert} from "./convert";
import {createData} from "./create_data";
import {deleteInternal} from "./delete_internal";
import {describe} from "./describe";
import {find} from "./find";
import {cast} from "./cast";
import {getBit} from "./get_bit";
import {getLocale} from "./get_locale";
import {setLocale} from "./set_locale";
import {getRunTime} from "./get_run_time";
import {getTime} from "./get_time";
import {IInsertDatabaseOptions, InsertDatabase} from "./insert_database";
import {insertInternal} from "./insert_internal";
import {DeleteDatabase, IDeleteDatabaseOptions} from "./delete_database";
import {loop} from "./loop";
import {IMessageOptions, MessageStatement} from "./message";
import {IModifyDatabaseOptions, ModifyDatabase} from "./modify_database";
import {modifyInternal} from "./modify_internal";
import {moveCorresponding} from "./move_corresponding";
import {readTable} from "./read_table";
import {replace} from "./replace";
import {rollback} from "./rollback";
import {SelectDatabase} from "./select";
import {setBit} from "./set_bit";
import {shift} from "./shift";
import {sort} from "./sort";
import {split} from "./split";
import {translate} from "./translate";
import {IUpdateDatabaseOptions, UpdateDatabase} from "./update_database";
import {IWriteOptions, WriteStatement} from "./write";
import {Context} from "../context";
import {ICharacter} from "../types/_character";
import {FieldSymbol, Structure, Table} from "../types";
import {INumeric} from "../types/_numeric";
import {CallFunction, ICallFunctionOptions} from "./call_function";
import {SelectDatabaseOptions, SelectRuntimeOptions} from "../db/db";

// this is a class, as statements like SELECT needs access to the database object instance
// and WRITE will access the Console
export class Statements {
  public append = append;
  public assert = assert;
  public assign = assign;
  public cast = cast;
  public clear = clear;
  public commit = commit;
  public concatenate = concatenate;
  public condense = condense;
  public convert = convert;
  public createData = createData;
  public deleteInternal = deleteInternal;
  public describe = describe;
  public find = find;
  public getBit = getBit;
  public getLocale = getLocale;
  public getRunTime = getRunTime;
  public getTime = getTime;
  public insertInternal = insertInternal;
  public loop = loop;
  public modifyInternal = modifyInternal;
  public moveCorresponding = moveCorresponding;
  public readTable = readTable;
  public replace = replace;
  public rollback = rollback;
  public setBit = setBit;
  public setLocale = setLocale;
  public shift = shift;
  public sort = sort;
  public split = split;
  public translate = translate;

  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
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
