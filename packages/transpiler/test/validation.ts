import {expect} from "chai";
import {runSingle} from "./_utils";

describe("Validation", () => {
  it("Unknown variable, throws error", async () => {
    const abap = `WRITE foowrite.`;

    try {
      await runSingle(abap);
      expect.fail();
    } catch (e) {
      expect(e.message).to.contain("not found");
    }
  });

  it("Unknown type, throws error", async () => {
    const abap = `DATA foo TYPE sdfsd.`;

    try {
      await runSingle(abap);
      expect.fail();
    } catch (e) {
      expect(e.message).to.contain("not found");
    }
  });

  it("Forbidden identifier", async () => {
    const abap = `DATA let TYPE string.`;

    try {
      await runSingle(abap);
      expect.fail();
    } catch (e) {
      expect(e.message).to.contain("allowed");
    }
  });
});