import {Console} from "./console";
import {DatabaseClient} from "./db";

export class Context {
  public db: DatabaseClient | undefined = undefined;
  public console: Console;
}