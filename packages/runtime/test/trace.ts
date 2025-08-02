import {expect} from "chai";
import {ABAP} from "../src";

describe("Trace", () => {
  it("test", () => {
    const abap = new ABAP();
    const trace = abap.statements._setTrace(0, true);

    // call some dummy statement
    abap.statements.assert(true);

    const totals = trace.getTotals();
    expect(Object.keys(totals).length).to.equal(1);
    expect(Object.keys(totals)[0]).to.equal("assert");
    expect(totals["assert"].calls).to.equal(1);
  });
});