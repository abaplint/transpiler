import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Packed type", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("initial value", async () => {
    const code = `
      DATA foo TYPE p.
      WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("compare value", async () => {
    const code = `
DATA foo TYPE p LENGTH 5.
foo = 12345.
ASSERT foo = '12345'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});