import {FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";

export interface IInsertDatabaseOptions {
  from?: Structure | FieldSymbol,
  table?: Table | FieldSymbol,
}

export function updateDatabase(_table: string | ICharacter, options: IInsertDatabaseOptions): void {
  if (this.db === undefined) {
    throw new Error("Runtime, database not initialized");
  }

  if (options.table instanceof FieldSymbol) {
    options.table = options.table.getPointer() as Table;
  }
  if (options.from instanceof FieldSymbol) {
    options.from = options.from.getPointer() as Structure;
  }

  throw "updateDatabase todo";

}