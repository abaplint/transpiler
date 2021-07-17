import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - MOVE-CORRESPONDING", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Basic", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA bar TYPE ty.
DATA foo TYPE ty.
bar-field = 2.
MOVE-CORRESPONDING bar TO foo.
WRITE foo-field.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});