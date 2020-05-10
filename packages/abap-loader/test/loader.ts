import {expect} from "chai";
import compiler from "./compiler";

describe("ABAP", () => {

  it("Test 1", async () => {
    const stats = await compiler("zprogram.prog.abap");
    // @ts-ignore
    const output = stats.toJson().modules[0].source;

    expect(output).to.equal(`abap.statements.write('sdf');`);
  });

});