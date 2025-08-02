import {isAsyncFunction} from "util/types";
import {Statements} from "./statements";

export class Trace {
  private readonly traceTotals: {[name: string]: number} = {};

  public setTrace(min: number, totals: boolean) {
    const candidates = [...Object.keys(this),...Object.getOwnPropertyNames(Statements.prototype)];
    for (const c of candidates) {
      if (c === "context" || c === "constructor" || c.startsWith("_") || c === "loop") {
        continue;
      }
      const func = (this as any)[c];
      if (isAsyncFunction(func)) {
        (this as any)[c] = this._traceAsync(func, c, min, totals);
      } else {
        (this as any)[c] = this._trace(func, c, min, totals);
      }
    }
  }

//////////////////////////////////////////

  private _trace(func: any, name: string, min: number, totals: boolean) {
    const tt = this.traceTotals;
    const exec = (...options: any[]) => {
      const start = Date.now();
      const result = func.bind(this)(...options);
      const runtime = Date.now() - start;
      if (totals === true) {
        if (tt[name] === undefined) {
          tt[name] = 0;
        }
        tt[name] += runtime;
      }
      if (runtime >= min) {
        console.log(`STATEMENT: ${name}, ${runtime} ms`);
        if (totals === true) {
          console.log(JSON.stringify(tt));
        }
      }
      return result;
    };
    return exec;
  }

  private _traceAsync(func: any, name: string, min: number, totals: boolean) {
    const tt = this.traceTotals;
    const exec = async (...options: any[]) => {
      const start = Date.now();
      const result = await func.bind(this)(...options);
      const runtime = Date.now() - start;
      if (totals === true) {
        if (tt[name] === undefined) {
          tt[name] = 0;
        }
        tt[name] += runtime;
      }
      if (runtime >= min) {
        console.log(`STATEMENT: ${name}, ${runtime} ms`);
        if (totals === true) {
          console.log(JSON.stringify(tt));
        }
      }
      return result;
    };
    return exec;
  }

}