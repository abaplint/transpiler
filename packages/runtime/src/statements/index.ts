import {append} from "./append";
import {assert} from "./assert";
import {assign} from "./assign";
import {callFunction} from "./call_function";
import {clear} from "./clear";
import {commit} from "./commit";
import {concatenate} from "./concatenate";
import {condense} from "./condense";
import {Console} from "../console";
import {convert} from "./convert";
import {createData} from "./create_data";
import {deleteInternal} from "./delete_internal";
import {describe} from "./describe";
import {find} from "./find";
import {getBit} from "./get_bit";
import {getRunTime} from "./get_run_time";
import {getTime} from "./get_time";
import {InsertDatabase} from "./insert_database";
import {insertInternal} from "./insert_internal";
import {DeleteDatabase} from "./delete_database";
import {loop} from "./loop";
import {MessageStatement} from "./message";
import {ModifyDatabase} from "./modify_database";
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
import {UpdateDatabase} from "./update_database";
import {write} from "./write";
import {DatabaseClient} from "../db";
import {Context} from "../context";

// this is a class, as statements like SELECT needs access to the database object instance
// and WRITE will access the Console
export class Statements {
  private readonly context = new Context();

  public append = append;
  public assert = assert;
  public assign = assign;
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
  public callFunction = callFunction;
  public getRunTime = getRunTime;
  public deleteDatabase = new DeleteDatabase(this.context).deleteDatabase;
  public getTime = getTime;
  public insertDatabase = new InsertDatabase(this.context).insertDatabase;
  public insertInternal = insertInternal;
  public loop = loop;
  public message = new MessageStatement(this.context).message;
  public modifyInternal = modifyInternal;
  public moveCorresponding = moveCorresponding;
  public readTable = readTable;
  public updateDatabase = new UpdateDatabase(this.context).updateDatabase;
  public modifyDatabase = new ModifyDatabase(this.context).modifyDatabase;
  public replace = replace;
  public rollback = rollback;
  public select = new SelectDatabase(this.context).select;
  public setBit = setBit;
  public shift = shift;
  public sort = sort;
  public split = split;
  public translate = translate;
  public write = write;

  // @ts-ignore
  private readonly console: Console; // todo, move to context

  public constructor(console: Console) {
    this.console = console;
  }

  public setDb(db: DatabaseClient) {
    this.context.db = db;
  }

}
