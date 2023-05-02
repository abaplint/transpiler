import {Console} from "./console.js";
import {DatabaseClient} from "./db/db.js";
import * as RFC from "./rfc.js";

export class Context {
  public console: Console;

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