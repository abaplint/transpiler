// import {expect} from "chai";
import {ABAP} from "../src";

describe("Trace", () => {
  it.only("test", () => {
    const abap = new ABAP();
    const trace = abap.statements._setTrace(0, true);

    // call some dummy statement
    abap.statements.assert(true);

    console.dir(trace.getTotals());
  });
});