import {ABAP} from "../../src";

describe("Statement ASSERT", () => {

  it("No problem", () => {
    const abap = new ABAP();
    abap.statements.assert(true);
  });

});