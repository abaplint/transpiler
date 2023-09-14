import {Context} from "../context";

export async function closeCursor(context: Context, cursor: number) {
  await context.cursors[cursor].closeCursor();
  delete context.cursors[cursor];
}