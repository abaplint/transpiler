import {ABAP} from "../../src/index.js";

describe("Statement ASSERT", () => {

  it("No problem", () => {
    const abap = new ABAP();
    abap.statements.assert(true);
  });

});