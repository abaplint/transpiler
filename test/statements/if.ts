// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - IF", () => {

  beforeEach(async () => {
    abap = new ABAP();
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

});