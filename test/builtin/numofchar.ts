// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - numofchar", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Builtin numerical: numofchar", async () => {
    const code = `
DATA foo1 TYPE string.
DATA foo2 TYPE c LENGTH 5.
foo1 = 'ab'.
foo2 = 'ab'.
ASSERT numofchar( foo1 ) = 2.
ASSERT numofchar( foo2 ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
