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

  // COMMIT WORK ends the current LUW, ie. all open database connections are committed
  for (const name of Object.keys(context.databaseConnections)) {
    await context.databaseConnections[name].commit();
  }

  // sy-subrc is only set when the WAIT addition is specified
  if (options?.wait === true) {
    abap.builtin.sy.get().subrc.set(0);
  }
}
