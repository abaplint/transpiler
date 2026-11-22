import {Context} from "../context";
import {ABAP} from "..";

declare const abap: ABAP;

export interface ICommitOptions {
  /** COMMIT CONNECTION, name of the database connection to commit */
  connection?: string,
  /** COMMIT WORK AND WAIT, note that updates are always executed synchronously */
  wait?: boolean,
}

export async function commit(context: Context, options?: ICommitOptions) {
  if (options?.connection !== undefined) {
    const db = context.databaseConnections[options.connection];
    if (db === undefined) {
      throw new Error(`COMMIT CONNECTION, unknown connection "${options.connection}"`);
    }
    await db.commit();
    return;
  }

  // COMMIT WORK ends the SAP LUW: the PERFORM ... ON COMMIT subroutines and the update function modules run, then
  // all open database connections are committed
  await context.luw.commit();
  for (const name of Object.keys(context.databaseConnections)) {
    await context.databaseConnections[name].commit();
  }

  // always 0 without AND WAIT; with AND WAIT 0 when the update succeeded, which it did when it gets here
  abap.builtin.sy.get().subrc.set(0);
}
