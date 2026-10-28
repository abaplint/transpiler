// import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_xsdbool.prog.abap", contents}], {skipVersionCheck});
}

describe("Builtin functions - xsdbool", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("xsdbool true", async () => {
    const code = `ASSERT xsdbool( 1 = 1 ) = abap_true.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("xsdbool false", async () => {
    const code = `ASSERT xsdbool( 1 = 2 ) = abap_false.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("xsdbool with less than, in IF", async () => {
    const code = `
DATA lv_one TYPE i VALUE 1.
DATA lv_two TYPE i VALUE 2.
DATA lv_hit TYPE abap_bool.
IF xsdbool( lv_one < lv_two ) = abap_true.
  lv_hit = abap_true.
ENDIF.
ASSERT lv_hit = abap_true.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
