// import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - matches", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
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

  it("matches pcre", async () => {
    const code = `
    DATA val TYPE abap_bool.
    val = boolc( matches( val = 'hello' pcre = 'hello' ) ).
    ASSERT val = abap_true.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
