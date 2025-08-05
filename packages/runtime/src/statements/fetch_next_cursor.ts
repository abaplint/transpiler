import {Context} from "../context";
import {HashedTable, Structure, Table, FieldSymbol} from "../types";
import {rowsToTarget} from "./select";
import {ABAP} from "..";

declare const abap: ABAP;

export async function fetchNextCursor(
  context: Context,
  cursor: number,
  target: Structure | Table | FieldSymbol | HashedTable,
  packageSize: number) {

  if (target instanceof FieldSymbol) {
    if (target.isAssigned() === false) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
      // @ts-ignore
    target = target.getPointer();
  }

  if (target instanceof Structure) {
    packageSize = 1;
  }

  const result = await context.cursors[cursor].fetchNextCursor(packageSize);
  if (result.rows.length === 0) {
    abap.builtin.sy.get().subrc.set(4);
    return;
  }

  abap.builtin.sy.get().subrc.set(0);

  rowsToTarget(target, result.rows);
}