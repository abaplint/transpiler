// import {expect} from "chai";
import {ABAP} from "../src";

describe("Trace", () => {
  it("test", () => {
    const abap = new ABAP();
    abap.statements._setTrace(10, true);

    // call some dummy statement
    abap.statements.assert(true);
  });
});