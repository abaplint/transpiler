import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - WRITE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
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
    expect(abap.console.get().trimEnd()).to.equal("A");
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
    expect(abap.console.get().trimEnd()).to.equal("A");
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

  it("WRITE - TO", async () => {
    const code = `
    DATA str TYPE c LENGTH 10.
    WRITE '2' TO str.
    WRITE '2' TO str.
    WRITE str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get().trimEnd()).to.equal("2");
  });

  it("WRITE - any type, from string", async () => {
    const code = `
CLASS clas DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS m1 IMPORTING val TYPE any.
ENDCLASS.
CLASS clas IMPLEMENTATION.
  METHOD m1.
    WRITE |{ val }|.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  clas=>m1( \`sdfsd\` ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("WRITE - any type, from string template", async () => {
    const code = `
CLASS clas DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS m1 IMPORTING val TYPE any.
ENDCLASS.
CLASS clas IMPLEMENTATION.
  METHOD m1.
    WRITE |{ val }|.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  clas=>m1( |sdfsd| ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("float, EXPONENT 0 NO-GROUPING NO-SIGN", async () => {
    const code = `
DATA foo TYPE f.
foo = 1 / 1000.
DATA char TYPE c LENGTH 100.
WRITE foo TO char EXPONENT 0 NO-GROUPING NO-SIGN.
CONDENSE char.
WRITE char.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get().trimEnd()).to.equal("0,0010000000000000");
  });

  it("float, EXPONENT 0 NO-GROUPING NO-SIGN, 5000", async () => {
    const code = `
  DATA foo TYPE f.
  foo = 5000.
  DATA char TYPE c LENGTH 100.
  WRITE foo TO char EXPONENT 0 NO-GROUPING NO-SIGN.
  CONDENSE char.
  WRITE char.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get().trimEnd()).to.equal("5000,0000000000000");
  });

  it("float, EXPONENT 0 NO-GROUPING NO-SIGN, minus one", async () => {
    const code = `
    DATA foo TYPE f.
    foo = -1.
    DATA char TYPE c LENGTH 100.
    WRITE foo TO char EXPONENT 0 NO-GROUPING NO-SIGN.
    CONDENSE char.
    WRITE char.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get().trimEnd()).to.equal("1,0000000000000000");
  });

  it("float, normal", async () => {
    const code = `
DATA foo TYPE f.
foo = 1 / 1000.
DATA ch TYPE c LENGTH 100.
WRITE foo TO ch.
CONDENSE ch.
ASSERT ch = '1,0000000000000000E-03'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("text element", async () => {
    // todo, more work is needed on text elements
    const code = `WRITE TEXT-001.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("escaping of js string", async () => {
    const code = "DATA foo TYPE string. foo = `hello ${ world`. WRITE foo.";
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.skip("currency, EUR", async () => {
    const code = `
DATA lv_value_c TYPE c LENGTH 10.
DATA val TYPE p LENGTH 10 DECIMALS 2.
val = '-1003.99'.
WRITE val TO lv_value_c EXPONENT 0 NO-GROUPING NO-SIGN CURRENCY 'EUR'.
WRITE lv_value_c.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get().trimEnd()).to.equal("   1003,99");
  });

  it.skip("currency, HUF", async () => {
    const code = `
DATA lv_value_c TYPE c LENGTH 10.
DATA val TYPE p LENGTH 10 DECIMALS 2.
val = '-1003.99'.
WRITE val TO lv_value_c EXPONENT 0 NO-GROUPING NO-SIGN CURRENCY 'HUF'.
WRITE lv_value_c.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get().trimEnd()).to.equal("    100399");
  });

});