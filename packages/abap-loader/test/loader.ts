import {expect} from "chai";
import {run, IFixture} from "./compiler";

describe("ABAP", () => {

  it("Test 1", async () => {
    const fix: IFixture = {
      entryFilename: "zprogram.prog.abap",
      files: [{
        filename: "zprogram.prog.abap",
        contents: "WRITE 'sdf'."}],
    };
    const stats = await run(fix);

    const output = stats.toJson().modules![0].source;
    expect(output).to.equal(`abap.statements.write('sdf');`);
  });

  it("Parser error", async () => {
    const fix: IFixture = {
      entryFilename: "zprogram.prog.abap",
      files: [{
        filename: "zprogram.prog.abap",
        contents: "parser error!!!"}],
    };
    await run(fix)
      .then(() => expect.fail("expected error"))
      .catch((reason) => expect(reason.toString()).to.contain("arser"));
  });

});