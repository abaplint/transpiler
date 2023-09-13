import {Context} from "../context";
import {Structure, Table} from "../types";

export async function fetchNextCursor(_context: Context, _cursor: number, _target: Structure | Table) {
  throw new Error("fetchNextCursor, runtime todo");
}