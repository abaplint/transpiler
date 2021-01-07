import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Time type", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Time initial value", async () => {
    const code = `
      DATA time TYPE t.
      WRITE time.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("000000");
  });

  it("Time, adding 1", async () => {
    const code = `
      DATA time TYPE t.
      time = '000241'.
      time = time + 1.
      WRITE time.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("000242");
  });

});