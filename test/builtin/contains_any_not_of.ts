import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - contains_any_not_of", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("hex digits only", async () => {
    const code = `
DATA lv_bool TYPE abap_bool.
lv_bool = boolc( contains_any_not_of( val = 'A9993E36' sub = '0123456789ABCDEF' ) ).
ASSERT lv_bool = abap_false.
lv_bool = boolc( contains_any_not_of( val = 'A9993G36' sub = '0123456789ABCDEF' ) ).
ASSERT lv_bool = abap_true.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("empty val is false", async () => {
    const code = `
DATA lv_bool TYPE abap_bool.
lv_bool = boolc( contains_any_not_of( val = \`\` sub = 'abc' ) ).
ASSERT lv_bool = abap_false.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("off and len", async () => {
    const code = `
DATA lv_bool TYPE abap_bool.
lv_bool = boolc( contains_any_not_of( val = 'xx12' sub = '0123456789' off = 2 ) ).
ASSERT lv_bool = abap_false.
lv_bool = boolc( contains_any_not_of( val = 'xx12' sub = '0123456789' off = 1 len = 2 ) ).
ASSERT lv_bool = abap_true.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
