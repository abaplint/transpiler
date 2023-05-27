import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - IF", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("IF, lower case", async () => {
    const code = `
    DATA lv_text TYPE string.
    if lv_text is initial.
    endif.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("OR, lower case", async () => {
    const code = `
    data i_amount_format type c length 2.
    if not ( i_amount_format is initial or i_amount_format+1(1) is initial ).
    endif.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("lower case not", async () => {
    const code = `
DATA: lv_equal_offset     TYPE i,
      lv_semicolon_offset TYPE i.
lv_equal_offset = 2.
lv_semicolon_offset = 0.
IF lv_equal_offset NE 0 AND not ( lv_semicolon_offset NE 0 AND lv_semicolon_offset LT lv_equal_offset ).
  WRITE 'yup'.
ENDIF.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yup");
  });

});