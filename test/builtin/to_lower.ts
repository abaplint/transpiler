// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/index.js";
import {AsyncFunction, runFiles} from "../_utils.js";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - to_lower", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("to_lower()", async () => {
    const code = `ASSERT to_lower( 'ABC' ) = 'abc'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("test", async () => {
    const code = `
    DATA foo TYPE c LENGTH 40.
    foo = 'sdf'.
    ASSERT foo = to_lower( foo ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
