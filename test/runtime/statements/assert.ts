import * as abap from "../../../src/runtime";

describe("Statement ASSERT", () => {

  it("No problem", () => {
    abap.statements.assert(true);
  });

});