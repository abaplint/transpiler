import {Console} from "./console/console";
import {DatabaseClient, DatabaseCursorCallbacks} from "./db/db";
import * as RFC from "./rfc";
import {DatasetHost, OpenDataset} from "./dataset/dataset";
import {LUW} from "./statements/luw";

export class Context {
  public console: Console;

  public cursorCounter = 0;
  public cursors: {[key: number]: DatabaseCursorCallbacks} = {};

  // DEFAULT and secondary database connections
  public databaseConnections: {[name: string]: DatabaseClient} = {};

  public RFCDestinations: {[name: string]: RFC.RFCClient} = {};

  // NEW-LINE ran: the next list output starts a new line, unless a new line was just started
  public newLinePending = false;
  // the SAP LUW: what COMMIT WORK runs and ROLLBACK WORK forgets, see statements/luw.ts
  public luw: LUW = new LUW();

  // CALL FUNCTION IN UPDATE TASK hands the copied parameters to the host, if one is set
  public updateTask: {register(name: string, param: {exporting?: any, tables?: any}): Promise<void> | void} | undefined = undefined;

  // the file system OPEN DATASET and friends read and write; none by default,
  // and then every DATASET statement throws
  public dataset: DatasetHost | undefined = undefined;
  public datasets: {[name: string]: OpenDataset} = {};

  // GET RUN TIME, the clock reading of the first call, and the last unwrapped microsecond count
  public runTime: {start: number, last: number} | undefined = undefined;

  public defaultDB() {
    if (this.databaseConnections["DEFAULT"] === undefined) {
      throw new Error("Runtime, database not initialized");
    }
    return this.databaseConnections["DEFAULT"];
  }
}