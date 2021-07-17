import {append} from "./append";
import {assert} from "./assert";
import {assign} from "./assign";
import {clear} from "./clear";
import {commit} from "./commit";
import {concatenate} from "./concatenate";
import {condense} from "./condense";
import {Console} from "../console";
import {convert} from "./convert";
import {deleteInternal} from "./delete_internal";
import {describe} from "./describe";
import {find} from "./find";
import {getBit} from "./get_bit";
import {getTime} from "./get_time";
import {insertInternal} from "./insert_internal";
import {loop} from "./loop";
import {message} from "./message";
import {modifyInternal} from "./modify_internal";
import {moveCorresponding} from "./move_corresponding";
import {readTable} from "./read_table";
import {replace} from "./replace";
import {rollback} from "./rollback";
import {select} from "./select";
import {setBit} from "./set_bit";
import {shift} from "./shift";
import {sort} from "./sort";
import {split} from "./split";
import {translate} from "./translate";
import {write} from "./write";

// this is a class, as statements like SELECT needs access to the database object instance
// and WRITE will access the Console
export class Statements {
  public append = append;
  public assert = assert;
  public assign = assign;
  public clear = clear;
  public commit = commit;
  public concatenate = concatenate;
  public condense = condense;
  public convert = convert;
  public deleteInternal = deleteInternal;
  public describe = describe;
  public find = find;
  public getBit = getBit;
  public getTime = getTime;
  public insertInternal = insertInternal;
  public loop = loop;
  public message = message;
  public modifyInternal = modifyInternal;
  public moveCorresponding = moveCorresponding;
  public readTable = readTable;
  public replace = replace;
  public rollback = rollback;
  public select = select;
  public setBit = setBit;
  public shift = shift;
  public sort = sort;
  public split = split;
  public translate = translate;
  public write = write;

  // @ts-ignore
  private readonly console: Console;
  // @ts-ignore
  private db: any;

  public constructor(console: Console) {
    this.console = console;
  }

  public setDb(db: any) {
    this.db = db;
  }

}
