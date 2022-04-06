import {Console} from "./console";
import {DatabaseClient} from "./db";
import * as RFC from "./rfc";

export class Context {
  public db: DatabaseClient | undefined = undefined;
  // DEFAULT and secondary database connections
  // todo, public DatabaseConnections: {[name: string]: DB.DatabaseClient} = {};
  public console: Console;
  public RFCDestinations: {[name: string]: RFC.RFCClient} = {};
}