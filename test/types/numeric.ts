// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfloat.prog.abap", contents}]);
}

describe("Running Examples - Numeric type", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("float, initial", async () => {
    const code = `
DATA num TYPE n LENGTH 3.
ASSERT num IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("float, not initial", async () => {
    const code = `
DATA num TYPE n LENGTH 3.
num = 1.
ASSERT num IS NOT INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
