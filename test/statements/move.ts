import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - MOVE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("MOVE - integer to integer", async () => {
    const code = `
        data lv_int1 type i value 1.
        data lv_int2 type i value 2.
        move lv_int1 to lv_int2.
        write lv_int2.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("MOVE - integer to string", async () => {
    const code = `
        data lv_int1 type i value 1.
        data lv_string type string value ''.
        move lv_int1 to lv_string.
        write lv_string.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1 ");
  });

  it.skip("MOVE - integer to integer structure", async () => {
    const code = `
        TYPES:
        BEGIN OF ts_str,
        comp_one TYPE i,
        END OF ts_str.
    DATA lv_int1 TYPE i VALUE 1.
    DATA lv_str TYPE ts_str.

    FIELD-SYMBOLS <ls_str> type any.
    ASSIGN lv_str to <ls_str>.
    MOVE lv_int1 TO <ls_str>.
    WRITE <ls_str>.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);

    expect(await f(abap)).to.throw();
  });
  it.skip("MOVE - string to integer structure", async () => {
    const code = `
        TYPES:
        BEGIN OF ts_str,
        comp_one TYPE i,
        END OF ts_str.
    DATA lv_int1 TYPE i VALUE 1.
    DATA lv_str TYPE ts_str.

    FIELD-SYMBOLS <ls_str> type any.
    ASSIGN lv_str to <ls_str>.
    MOVE lv_int1 TO <ls_str>.
    WRITE <ls_str>.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);

    expect(await f(abap)).to.throw();


  });

  it.skip("MOVE - string to structure", async () => {
    const code = `
        data lv_int1 type i value 1.
        data lv_string type string value ''.
        move lv_int1 to lv_string.
        write lv_string.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1 ");
  });
});





