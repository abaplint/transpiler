import {Console} from "./console/console";
import {DatabaseClient, DatabaseCursorCallbacks} from "./db/db";
import * as RFC from "./rfc";
import {SubmitCall, SubmitHost} from "./submit/submit";

export class Context {
  public console: Console;

  public cursorCounter = 0;
  public cursors: {[key: number]: DatabaseCursorCallbacks} = {};

  // DEFAULT and secondary database connections
  public databaseConnections: {[name: string]: DatabaseClient} = {};

  public RFCDestinations: {[name: string]: RFC.RFCClient} = {};

  /* runs the programs of SUBMIT, and the SUBMITs in progress, innermost last */
  public submit: SubmitHost | undefined;
  public submitted: SubmitCall[] = [];

  public defaultDB() {
    if (this.databaseConnections["DEFAULT"] === undefined) {
      throw new Error("Runtime, database not initialized");
    }
    return this.databaseConnections["DEFAULT"];
  }
}