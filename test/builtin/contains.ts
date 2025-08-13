// import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - contains", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic, true", async () => {
    const code = `
    DATA val TYPE abap_bool.
    val = boolc( contains( val = '12' regex = '\\d' ) ).
    ASSERT val = abap_true.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("basic, false", async () => {
    const code = `
    DATA val TYPE abap_bool.
    val = boolc( contains( val = 'ab' regex = '\\d' ) ).
    ASSERT val = abap_false.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("basic, AND", async () => {
    const code = `
DATA val TYPE abap_bool.
val = boolc( abap_true = abap_true AND contains( val = 'ab' regex = '\\d' ) ).
ASSERT val = abap_false.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("sub, start, end", async () => {
    const code = `
DATA lv_bool TYPE abap_bool.

lv_bool = boolc( contains( val = 'axaxa' sub = 'x' ) ).
ASSERT lv_bool = abap_true.
lv_bool = boolc( contains( val = 'axaxa' sub = 'b' ) ).
ASSERT lv_bool = abap_false.

lv_bool = boolc( contains( val = 'axaxa' start = 'a' ) ).
ASSERT lv_bool = abap_true.
lv_bool = boolc( contains( val = 'axaxa' start = 'b' ) ).
ASSERT lv_bool = abap_false.

lv_bool = boolc( contains( val = 'axaxa' end = 'a' ) ).
ASSERT lv_bool = abap_true.
lv_bool = boolc( contains( val = 'axaxa' end = 'b' ) ).
ASSERT lv_bool = abap_false.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
