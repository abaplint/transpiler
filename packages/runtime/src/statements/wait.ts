import {INumeric} from "../types/_numeric";
import {Context} from "../context";
import {ABAP} from "..";

declare const abap: ABAP;

export interface IWaitOptions {
  /** UNTIL, the logical expression, if not supplied its the simple "WAIT UP TO" variant */
  cond?: () => any,
  /** UP TO sec SECONDS */
  seconds?: INumeric,
}

// WAIT ends the current database LUW when it interrupts the execution of the program,
// ie. all open database connections are committed, note that this is not a full COMMIT WORK,
// sy-subrc is owned by the WAIT statement itself
async function implicitCommit(context: Context) {
  for (const name of Object.keys(context.databaseConnections)) {
    await context.databaseConnections[name].commit();
  }
}

export async function wait(context: Context, options: IWaitOptions): Promise<void> {
  const timeout = options.seconds === undefined ? undefined : options.seconds.get() * 1000;
  const deadline = timeout === undefined ? undefined : Date.now() + timeout;

  if (options.cond === undefined) {
    // "WAIT UP TO sec SECONDS", the execution is always interrupted and sy-subrc is always zero
    await implicitCommit(context);
    await new Promise(r => setTimeout(r, timeout));
    abap.builtin.sy.get().subrc.set(0);
    return;
  }

  let interrupted = false;
  while (true) {
    if (options.cond() === true) {
      abap.builtin.sy.get().subrc.set(0);
      return;
    }

    const remaining = deadline === undefined ? 500 : deadline - Date.now();
    if (remaining <= 0) {
      abap.builtin.sy.get().subrc.set(8);
      return;
    }

    if (interrupted === false) {
      // the condition is not true, so the execution is about to be interrupted,
      // commit before sleeping so the changes are visible to other sessions during the wait
      interrupted = true;
      await implicitCommit(context);
    }

    await new Promise(r => setTimeout(r, Math.min(500, remaining)));
  }
}
