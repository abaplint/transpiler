import * as abap from "../../src";

describe("Statement ASSERT", () => {

  it("No problem", () => {
    abap.statements.assert(true);
  });

});