import {Context} from "../context";

export async function openCursor(context: Context, select: string) {
  const callbacks = await context.defaultDB().openCursor({select: select});
  const num = context.cursorCounter++;
  context.cursors[num] = callbacks;
  return num;
}