// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

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

});
