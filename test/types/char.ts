import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Character type", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("compare with new line", async () => {
    const code = `
    DATA foo TYPE string.
    DATA char TYPE c LENGTH 1.
    foo = |\\n|.
    char = foo(1).
    ASSERT char = |\\n|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("compare with new line", async () => {
    const code = `
DATA: BEGIN OF stru,
        field1 TYPE c LENGTH 2,
        field2 TYPE c LENGTH 2,
      END OF stru.
DATA target TYPE c LENGTH 3.
stru-field1 = '12'.
stru-field2 = '34'.
target = stru.
WRITE target.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("123");
  });

});