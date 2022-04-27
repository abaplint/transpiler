// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - matches", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("matches 01", async () => {
    const code = `
    DATA lv_bool TYPE abap_bool.
    lv_bool = boolc( matches( val = 'moo' regex = 'moo' ) ).
    ASSERT lv_bool = abap_true.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("matches 02", async () => {
    const code = `
    DATA lv_bool TYPE abap_bool.
    lv_bool = boolc( matches( val = 'moo' regex = 'o' ) ).
    ASSERT lv_bool = abap_false.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
