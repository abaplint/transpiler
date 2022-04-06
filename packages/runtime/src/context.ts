import {Console} from "./console";
import {DatabaseClient} from "./db";

export class Context {
  public db: DatabaseClient | undefined = undefined;
  // DEFAULT and secondary database connections
  // todo, public DatabaseConnections: {[name: string]: DB.DatabaseClient} = {};
  public console: Console;
}