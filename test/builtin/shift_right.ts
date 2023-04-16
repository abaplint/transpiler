// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - shift_right", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("basic shift_right()", async () => {
    const code = `
DATA lv_str TYPE string.
lv_str = |sdf |.
ASSERT strlen( shift_right( lv_str ) ) = 3.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
