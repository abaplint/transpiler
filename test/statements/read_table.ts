import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - READ TABLE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("read index 1 of structured table", async () => {
    const code = `
      TYPES: BEGIN OF ty_bar,
              field TYPE i,
            END OF ty_bar.
      TYPES ty_tab TYPE STANDARD TABLE OF ty_bar WITH DEFAULT KEY.
      DATA bar TYPE ty_bar.
      DATA tab TYPE ty_tab.

      READ TABLE tab INDEX 1 INTO bar.
      WRITE sy-subrc.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("READ TABLE TRANSPORTING NO FIELDS", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      READ TABLE tab INDEX 1 TRANSPORTING NO FIELDS.
      WRITE sy-subrc.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("READ TABLE WITH KEY", async () => {
    const code = `
      TYPES: BEGIN OF ty_structure,
              field TYPE i,
            END OF ty_structure.

      DATA tab TYPE STANDARD TABLE OF ty_structure WITH DEFAULT KEY.
      DATA line LIKE LINE OF tab.

      line-field = 2.
      APPEND line TO tab.
      line-field = 5.
      APPEND line TO tab.
      ASSERT lines( tab ) = 2.

      CLEAR line.

      READ TABLE tab INTO line WITH KEY field = 2.
      ASSERT sy-subrc = 0.
      ASSERT line-field = 2.

      READ TABLE tab INTO line WITH KEY field = 123.
      ASSERT sy-subrc = 4.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("READ TABLE table_line", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i.
      APPEND 2 TO tab.
      READ TABLE tab WITH KEY table_line = 2 TRANSPORTING NO FIELDS.
      ASSERT sy-subrc = 0.
      READ TABLE tab WITH KEY table_line = 123 TRANSPORTING NO FIELDS.
      ASSERT sy-subrc = 4.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("READ TABLE INDEX not found", async () => {
    const code = `
      DATA lt_segments TYPE STANDARD TABLE OF string.
      DATA lv_segment TYPE string.
      lv_segment = 'abc'.
      READ TABLE lt_segments INTO lv_segment INDEX 2.
      ASSERT sy-subrc = 4.
      ASSERT lv_segment = 'abc'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

});