// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/index.js";
import {AsyncFunction, runFiles} from "../_utils.js";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - repeat", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("basic repeat()", async () => {
    const code = `ASSERT repeat( val = 'a' occ = 2 ) = 'aa'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
