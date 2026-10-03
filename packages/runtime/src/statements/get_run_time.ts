import {Context} from "../context";
import {INumeric} from "../types/_numeric";

// milliseconds with a fraction, from a monotonic clock where there is one:
// performance.now() in Node, Bun and browsers, Date.now() otherwise
function now(): number {
  if (typeof performance === "object" && typeof performance?.now === "function") {
    return performance.now();
  }
  return Date.now();
}

// GET RUN TIME FIELD: microseconds since the first call in the internal
// session, which gives 0. The first call is per context, so per ABAP instance
export function getRunTime(context: Context, value: INumeric) {
  const t = now();
  if (context.runTime === undefined) {
    context.runTime = {start: t, last: 0};
  }
  let micro = Math.floor((t - context.runTime.start) * 1000);
  // never backwards, also when the clock is Date.now()
  if (micro < context.runTime.last) {
    micro = context.runTime.last;
  }
  context.runTime.last = micro;
  value.set(micro);
}
