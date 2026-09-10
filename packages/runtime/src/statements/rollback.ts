import {Context} from "../context";

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

  // ROLLBACK WORK ends the current LUW, ie. all open database connections are rolled back
  for (const name of Object.keys(context.databaseConnections)) {
    await context.databaseConnections[name].rollback();
  }
}
