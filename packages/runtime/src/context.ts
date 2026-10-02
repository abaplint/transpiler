import {Console} from "./console/console";
import {DatabaseClient, DatabaseCursorCallbacks} from "./db/db";
import * as RFC from "./rfc";
import {DatasetHost, OpenDataset} from "./dataset/dataset";

export class Context {
  public console: Console;

  public cursorCounter = 0;
  public cursors: {[key: number]: DatabaseCursorCallbacks} = {};

  // DEFAULT and secondary database connections
  public databaseConnections: {[name: string]: DatabaseClient} = {};

  public RFCDestinations: {[name: string]: RFC.RFCClient} = {};

  // CALL FUNCTION IN UPDATE TASK hands the copied parameters to the host, if one is set
  public updateTask: {register(name: string, param: {exporting?: any, tables?: any}): Promise<void> | void} | undefined = undefined;

  // the file system OPEN DATASET and friends read and write; none by default,
  // and then every DATASET statement throws
  public dataset: DatasetHost | undefined = undefined;
  public datasets: {[name: string]: OpenDataset} = {};

  public defaultDB() {
    if (this.databaseConnections["DEFAULT"] === undefined) {
      throw new Error("Runtime, database not initialized");
    }
    return this.databaseConnections["DEFAULT"];
  }
}