// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - MODIFY database", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Basic MOIDFY database", async () => {
    const code = `
    DATA ls_t100 TYPE t100.
    MODIFY t100 FROM ls_t100.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
//    expect(abap.console.get()).to.equal("4");
  });

});