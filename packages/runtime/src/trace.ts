import {Statements} from "./statements";

// Polyfill for detecting async functions without relying on Node's "util/types"
// Works in Node and browser builds.
const isAsyncFunction = (fn: unknown): fn is (...args: any[]) => Promise<any> => {
  if (typeof fn !== "function") {
    return false;
  }
  // Reliable brand check
  const tag = Object.prototype.toString.call(fn);
  if (tag === "[object AsyncFunction]") {
    return true;
  }
  // Fallback via constructor name (covers most environments)
  const ctorName = (fn as any)?.constructor?.name;
  return ctorName === "AsyncFunction";
};

export class Trace {
  private readonly traceTotals: {[name: string]: {calls: number, totalRuntime: number}} = {};

  public setTrace(min: number, totals: boolean, statements: any) {
    const candidates = [...Object.keys(statements),...Object.getOwnPropertyNames(Statements.prototype)];
    for (const c of candidates) {
      if (c === "context" || c === "constructor" || c.startsWith("_") || c === "loop") {
        continue;
      }
      const func = statements[c];
      if (isAsyncFunction(func)) {
        statements[c] = this._traceAsync(func, c, min, totals);
      } else {
        statements[c] = this._trace(func, c, min, totals);
      }
    }

    return this;
  }

  public getTotals() {
    return this.traceTotals;
  }

//////////////////////////////////////////

  private _trace(func: any, name: string, min: number, totals: boolean) {
    const tt = this.traceTotals;
    const exec1 = (...options: any[]) => {
      const start = Date.now();
      const result = func.bind(this)(...options);
      const runtime = Date.now() - start;
      if (totals === true) {
        if (tt[name] === undefined) {
          tt[name] = {calls: 0, totalRuntime: 0};
        }
        tt[name].totalRuntime += runtime;
        tt[name].calls++;
      }
      if (min > 0 && runtime >= min) {
        console.log(`STATEMENT: ${name}, ${runtime} ms`);
        if (totals === true) {
          console.log(JSON.stringify(tt));
        }
      }
      return result;
    };
    return exec1;
  }

  private _traceAsync(func: any, name: string, min: number, totals: boolean) {
    const tt = this.traceTotals;
    const exec2 = async (...options: any[]) => {
      const start = Date.now();
      const result = await func.bind(this)(...options);
      const runtime = Date.now() - start;
      if (totals === true) {
        if (tt[name] === undefined) {
          tt[name] = {calls: 0, totalRuntime: 0};
        }
        tt[name].totalRuntime += runtime;
        tt[name].calls++;
      }
      if (min > 0 && runtime >= min) {
        console.log(`STATEMENT: ${name}, ${runtime} ms`);
        if (totals === true) {
          console.log(JSON.stringify(tt));
        }
      }
      return result;
    };
    return exec2;
  }

}