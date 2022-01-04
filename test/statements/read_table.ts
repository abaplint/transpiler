import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

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
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("READ TABLE TRANSPORTING NO FIELDS", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      READ TABLE tab INDEX 1 TRANSPORTING NO FIELDS.
      WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("READ TABLE calling method", async () => {
    const code = `
CLASS clas DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS bar1.
    CLASS-METHODS bar2 RETURNING VALUE(val) TYPE string..
ENDCLASS.
CLASS clas IMPLEMENTATION.
  METHOD bar1.
    DATA gt_auth TYPE STANDARD TABLE OF string.
    READ TABLE gt_auth WITH KEY table_line = bar2( ) TRANSPORTING NO FIELDS.
  ENDMETHOD.
  METHOD bar2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  clas=>bar1( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("READ TABLE calling method, should be executed once", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    DATA counter TYPE i.
    METHODS bar RETURNING VALUE(str) TYPE string.
    METHODS run.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD bar.
    counter = counter + 1.
  ENDMETHOD.
  METHOD run.
    DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    APPEND 'foo' TO tab.
    APPEND 'bar' TO tab.
    APPEND 'moo' TO tab.
    READ TABLE tab WITH KEY table_line = bar( ) TRANSPORTING NO FIELDS.
    WRITE / counter.
  ENDMETHOD.
ENDCLASS.

FORM run.
  DATA bar TYPE REF TO lcl_bar.
  CREATE OBJECT bar.
  bar->run( ).
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("READ TABLE calling method, should be executed once, also if table is empty", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    DATA counter TYPE i.
    METHODS bar RETURNING VALUE(str) TYPE string.
    METHODS run.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD bar.
    counter = counter + 1.
  ENDMETHOD.
  METHOD run.
    DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    READ TABLE tab WITH KEY table_line = bar( ) TRANSPORTING NO FIELDS.
    WRITE / counter.
  ENDMETHOD.
ENDCLASS.

FORM run.
  DATA bar TYPE REF TO lcl_bar.
  CREATE OBJECT bar.
  bar->run( ).
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("READ TABLE must set sy-tabix", async () => {
    const code = `
FORM run.
  DATA table TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA row LIKE LINE OF table.
  APPEND 1 TO table.
  APPEND 2 TO table.
  READ TABLE table WITH KEY table_line = 1 TRANSPORTING NO FIELDS.
  ASSERT sy-tabix = 1.
  READ TABLE table WITH KEY table_line = 2 TRANSPORTING NO FIELDS.
  ASSERT sy-tabix = 2.
  READ TABLE table INDEX 1 INTO row.
  ASSERT sy-tabix = 1.
  READ TABLE table INDEX 2 INTO row.
  ASSERT sy-tabix = 2.

  READ TABLE table INDEX 123 INTO row.
  ASSERT sy-tabix = 0.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.skip("READ TABLE, with table key", async () => {
    const code = `
TYPES: BEGIN OF ty_css_var,
         name  TYPE string,
         value TYPE string,
       END OF ty_css_var.
TYPES ty_css_vars TYPE SORTED TABLE OF ty_css_var WITH UNIQUE KEY name.
DATA lt_tab TYPE ty_css_vars.
DATA row LIKE LINE OF lt_tab.
row-name = 'foo'.
row-value = 'bar'.
INSERT row INTO TABLE lt_tab.
READ TABLE lt_tab FROM row TRANSPORTING NO FIELDS.
ASSERT sy-tabix = 1.
ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});