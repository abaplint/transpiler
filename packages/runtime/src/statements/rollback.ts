import {Context} from "../context";
import {ABAP} from "..";

declare const abap: ABAP;

export interface IRollbackOptions {
  /** ROLLBACK CONNECTION, name of the database connection to roll back */
  connection?: string,
}

export async function rollback(context: Context, options?: IRollbackOptions) {
  if (options?.connection !== undefined) {
    const db = context.databaseConnections[options.connection];
    if (db === undefined) {
      throw new Error(`ROLLBACK CONNECTION, unknown connection "${options.connection}"`);
    }
    await db.rollback();
    return;
  }

  // ROLLBACK WORK ends the SAP LUW: the PERFORM ... ON ROLLBACK subroutines run, everything registered for COMMIT WORK
  // is forgotten, and all open database connections are rolled back
  await context.luw.rollback();
  for (const name of Object.keys(context.databaseConnections)) {
    await context.databaseConnections[name].rollback();
  }

  // sy-subrc is always 0
  abap.builtin.sy.get().subrc.set(0);
}
