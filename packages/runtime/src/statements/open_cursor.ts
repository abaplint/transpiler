import {Context} from "../context";

export interface IOpenCursorDatabaseOptions {
  connection?: string,
}

export async function openCursor(context: Context, select: string, options: IOpenCursorDatabaseOptions) {
  let db = context.defaultDB();
  if (options?.connection) {
    db = context.databaseConnections[options.connection];
  }

  const callbacks = await db.openCursor({select: select});
  const num = context.cursorCounter++;
  context.cursors[num] = callbacks;
  return num;
}