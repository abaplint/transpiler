import {Console} from "./console/console";
import {DatabaseClient, DatabaseCursorCallbacks} from "./db/db";
import * as RFC from "./rfc";

export class Context {
  public console: Console;

  public cursorCounter = 0;
  public cursors: {[key: number]: DatabaseCursorCallbacks} = {};

  // DEFAULT and secondary database connections
  public databaseConnections: {[name: string]: DatabaseClient} = {};

  public RFCDestinations: {[name: string]: RFC.RFCClient} = {};

  public defaultDB() {
    if (this.databaseConnections["DEFAULT"] === undefined) {
      throw new Error("Runtime, database not initialized");
    }
    return this.databaseConnections["DEFAULT"];
  }
}