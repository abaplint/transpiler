import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - WRITE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("WRITE - single line", async () => {
    const code = `
        WRITE /.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("\n");
  });

  it("WRITE - single character ", async () => {
    const code = `
      data lv_test type c length 10.
      lv_test = 'A'.
      WRITE lv_test.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("A");
  });

  it("WRITE - single positive integer ", async () => {
    const code = `
      data lv_test type i.
      lv_test = 1.
      WRITE lv_test.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("WRITE - structure with one component", async () => {
    const code = `
      TYPES:
        BEGIN OF ts_str,
          comp_one TYPE c LENGTH 10,
        END OF ts_str.
        DATA ls_str TYPE ts_str.
        ls_str-comp_one = 'A'.
        write: ls_str.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("A");
  });

  it.skip("WRITE - structure with 2 components", async () => {
    const code = `
      TYPES BEGIN OF ts_str_2.
      TYPES aaa TYPE c LENGTH 5.
      TYPES bbb TYPE c LENGTH 5.
      TYPES END OF ts_str_2.
      DATA ls_str_2 TYPE ts_str_2.
      ls_str_2-aaa = 1.
      ls_str_2-bbb = 2.
      WRITE: ls_str_2.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("   1    2 ");
  });

  it.skip("WRITE - TO", async () => {
    const code = `
    DATA str TYPE c LENGTH 10.
    WRITE '2' TO str.
    WRITE '2' TO str.
    WRITE str.`;
    const js = await run(code);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});