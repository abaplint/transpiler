import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - APPEND", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("APPEND string", async () => {
    const code = `
      data tab type standard table of string.
      data val type string.
      append 'foo' to tab.
      loop at tab into val.
        write val.
      endloop.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("append structure to table", async () => {
    const code = `
      TYPES: BEGIN OF ty_bar,
              field TYPE i,
            END OF ty_bar.
      TYPES ty_tab TYPE STANDARD TABLE OF ty_bar WITH DEFAULT KEY.
      DATA bar TYPE ty_bar.
      DATA tab TYPE ty_tab.

      bar-field = 1.
      APPEND bar TO tab.
      bar-field = 2.
      APPEND bar TO tab.

      LOOP AT tab INTO bar.
        WRITE / bar-field.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("APPEND INITIAL LINE", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i.
      FIELD-SYMBOLS <fs> LIKE LINE OF tab.
      APPEND INITIAL LINE TO tab ASSIGNING <fs>.
      WRITE <fs>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("APPEND field symbol", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i.
      DATA row LIKE LINE OF tab.
      FIELD-SYMBOLS <fs> LIKE LINE OF tab.
      APPEND INITIAL LINE TO tab ASSIGNING <fs>.
      <fs> = 2.
      READ TABLE tab INDEX 1 INTO row.
      WRITE row.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("APPEND LINES", async () => {
    const code = `
      DATA tab1 TYPE STANDARD TABLE OF i.
      DATA tab2 TYPE STANDARD TABLE OF i.
      DATA line TYPE i.
      APPEND 2 TO tab1.
      APPEND 3 TO tab1.
      APPEND 5 TO tab2.
      APPEND 7 TO tab2.
      APPEND LINES OF tab2 TO tab1.
      LOOP AT tab1 INTO line.
        WRITE / line.
      ENDLOOP.
      ASSERT lines( tab1 ) = 4.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n3\n5\n7");
  });

  it("APPEND ASSIGNING", async () => {
    const code = `
      DATA tab TYPE TABLE OF i.
      FIELD-SYMBOLS <i> TYPE i.
      APPEND 3 TO tab ASSIGNING <i>.
      ASSERT <i> = 3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("APPEND ASSIGNING", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA tab2 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    FIELD-SYMBOLS <lv_line> LIKE LINE OF tab.
    FIELD-SYMBOLS <lv_line_c>  LIKE LINE OF tab.
    APPEND 'bar' TO tab.
    LOOP AT tab ASSIGNING <lv_line>.
      APPEND <lv_line> TO tab2 ASSIGNING <lv_line_c>.
      <lv_line_c> = |foo|.
    ENDLOOP.
    LOOP AT tab2 ASSIGNING <lv_line>.
      WRITE <lv_line>.
    ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("APPEND INITIAL to fs", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
  ASSIGN tab TO <table>.
  APPEND INITIAL LINE TO <table>.
  ASSERT lines( tab ) = 1.
  ASSERT lines( <table> ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});