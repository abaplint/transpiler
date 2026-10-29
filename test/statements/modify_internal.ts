import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - MODIFY internal", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("MODIFY internal table INDEX FROM", async () => {
    const code = `
    DATA result TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA dat LIKE LINE OF result.
    APPEND 2 TO result.
    MODIFY result INDEX 1 FROM 4.
    ASSERT sy-subrc = 0.
    READ TABLE result INDEX 1 INTO dat.
    WRITE dat.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("MODIFY, testing references", async () => {
    const code = `
  DATA integers TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA int TYPE i.
  APPEND 1 TO integers.
  APPEND 2 TO integers.
  int = 3.
  MODIFY integers INDEX 1 FROM int.
  int = 4.
  MODIFY integers INDEX 2 FROM int.
  LOOP AT integers INTO int.
    WRITE / int.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3\n4");
  });

  it("MODIFY, table key", async () => {
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
MODIFY TABLE lt_tab FROM row.
ASSERT sy-subrc = 4.
ASSERT lines( lt_tab ) = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("MODIFY, TRANSPORTING WHERE", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo     TYPE c LENGTH 10,
         dynpfld TYPE abap_bool,
         bar     TYPE c LENGTH 10,
       END OF ty.

DATA lt_fc      TYPE STANDARD TABLE OF ty.
DATA ls_initial TYPE ty.
DATA ls_fc      LIKE LINE OF lt_fc.

APPEND ls_initial TO lt_fc.
APPEND ls_initial TO lt_fc.

ls_fc-dynpfld = abap_true.
MODIFY lt_fc FROM ls_fc TRANSPORTING dynpfld WHERE dynpfld = space.

WRITE / lines( lt_fc ).
LOOP AT lt_fc INTO ls_fc.
  ASSERT ls_fc-dynpfld = abap_true.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("MODIFY, loop index", async () => {
    const code = `
DATA: BEGIN OF request,
        tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
        wa  TYPE string,
      END OF request.
DATA val TYPE string.

val = |foo|.
INSERT val INTO TABLE request-tab.

LOOP AT request-tab INTO request-wa.
  TRANSLATE request-wa TO UPPER CASE.
  MODIFY request-tab FROM request-wa INDEX sy-tabix.
  WRITE / request-wa.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("FOO");
  });

  it("MODIFY, transporting where from", async () => {
    const code = `
TYPES: BEGIN OF ty_attribute,
         value      TYPE string,
         annotation TYPE string,
       END OF ty_attribute.
TYPES ty_attributes TYPE STANDARD TABLE OF ty_attribute WITH DEFAULT KEY.

DATA exp_attributes TYPE ty_attributes.
DATA attribute TYPE ty_attribute.
FIELD-SYMBOLS <attribute> TYPE ty_attribute.

APPEND INITIAL LINE TO exp_attributes ASSIGNING <attribute>.
<attribute>-value      = 'value1'.

APPEND INITIAL LINE TO exp_attributes ASSIGNING <attribute>.
<attribute>-value      = 'value2'.
<attribute>-annotation = 'BAR'.

attribute-annotation = 'MOO'.
MODIFY exp_attributes FROM attribute TRANSPORTING annotation WHERE annotation IS INITIAL.

LOOP AT exp_attributes INTO attribute.
  WRITE / attribute-annotation.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("MOO\nBAR");
  });

  it("MODIFY TABLE, hashed", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         name TYPE string,
         size TYPE i,
       END OF ty_row.
TYPES ty_rows TYPE HASHED TABLE OF ty_row WITH UNIQUE KEY name.
DATA mt_rows TYPE ty_rows.
DATA ls_row TYPE ty_row.

ls_row-name = 'a'.
INSERT ls_row INTO TABLE mt_rows.
ls_row-size = 1.
MODIFY TABLE mt_rows FROM ls_row.
WRITE / sy-subrc.
WRITE / lines( mt_rows ).
CLEAR ls_row.
READ TABLE mt_rows WITH TABLE KEY name = 'a' INTO ls_row.
WRITE / ls_row-size.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\n1\n1");
  });

  it("MODIFY TABLE, hashed, not found", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         name TYPE string,
         size TYPE i,
       END OF ty_row.
TYPES ty_rows TYPE HASHED TABLE OF ty_row WITH UNIQUE KEY name.
DATA mt_rows TYPE ty_rows.
DATA ls_row TYPE ty_row.

ls_row-name = 'a'.
INSERT ls_row INTO TABLE mt_rows.
ls_row-name = 'b'.
ls_row-size = 1.
MODIFY TABLE mt_rows FROM ls_row.
WRITE / sy-subrc.
WRITE / lines( mt_rows ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4\n1");
  });

  it("MODIFY TABLE, hashed, TRANSPORTING", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         name  TYPE string,
         size  TYPE i,
         other TYPE i,
       END OF ty_row.
TYPES ty_rows TYPE HASHED TABLE OF ty_row WITH UNIQUE KEY name.
DATA mt_rows TYPE ty_rows.
DATA ls_row TYPE ty_row.

ls_row-name = 'a'.
ls_row-size = 1.
ls_row-other = 1.
INSERT ls_row INTO TABLE mt_rows.
ls_row-size = 2.
ls_row-other = 2.
MODIFY TABLE mt_rows FROM ls_row TRANSPORTING size.
WRITE / sy-subrc.
CLEAR ls_row.
READ TABLE mt_rows WITH TABLE KEY name = 'a' INTO ls_row.
WRITE / ls_row-size.
WRITE / ls_row-other.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\n2\n1");
  });

  it("MODIFY TABLE, sorted", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         name TYPE string,
         size TYPE i,
       END OF ty_row.
TYPES ty_rows TYPE SORTED TABLE OF ty_row WITH UNIQUE KEY name.
DATA mt_rows TYPE ty_rows.
DATA ls_row TYPE ty_row.

ls_row-name = 'b'.
INSERT ls_row INTO TABLE mt_rows.
ls_row-name = 'a'.
INSERT ls_row INTO TABLE mt_rows.
ls_row-size = 1.
MODIFY TABLE mt_rows FROM ls_row.
WRITE / sy-subrc.
LOOP AT mt_rows INTO ls_row.
  WRITE / |{ ls_row-name } { ls_row-size }|.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\na 1\nb 0");
  });

  it("MODIFY TABLE, standard, keeps order", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         name TYPE string,
         size TYPE i,
       END OF ty_row.
TYPES ty_rows TYPE STANDARD TABLE OF ty_row WITH NON-UNIQUE KEY name.
DATA mt_rows TYPE ty_rows.
DATA ls_row TYPE ty_row.

ls_row-name = 'a'.
APPEND ls_row TO mt_rows.
ls_row-name = 'b'.
APPEND ls_row TO mt_rows.
ls_row-name = 'a'.
ls_row-size = 1.
MODIFY TABLE mt_rows FROM ls_row.
WRITE / sy-subrc.
LOOP AT mt_rows INTO ls_row.
  WRITE / |{ ls_row-name } { ls_row-size }|.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\na 1\nb 0");
  });

  it("MODIFY TABLE, standard, not found", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         name TYPE string,
         size TYPE i,
       END OF ty_row.
TYPES ty_rows TYPE STANDARD TABLE OF ty_row WITH NON-UNIQUE KEY name.
DATA mt_rows TYPE ty_rows.
DATA ls_row TYPE ty_row.

ls_row-name = 'a'.
APPEND ls_row TO mt_rows.
ls_row-name = 'b'.
MODIFY TABLE mt_rows FROM ls_row.
WRITE / sy-subrc.
WRITE / lines( mt_rows ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4\n1");
  });

  it("MODIFY TABLE, standard default key, integers", async () => {
    const code = `
DATA integers TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
APPEND 1 TO integers.
APPEND 2 TO integers.
MODIFY TABLE integers FROM 2.
WRITE / sy-subrc.
MODIFY TABLE integers FROM 3.
WRITE / sy-subrc.
WRITE / lines( integers ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\n4\n2");
  });

  it("MODIFY TABLE, standard default key, structure", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         name TYPE string,
         size TYPE i,
       END OF ty_row.
TYPES ty_rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA mt_rows TYPE ty_rows.
DATA ls_row TYPE ty_row.

ls_row-name = 'a'.
APPEND ls_row TO mt_rows.
ls_row-name = 'b'.
APPEND ls_row TO mt_rows.
ls_row-size = 1.
MODIFY TABLE mt_rows FROM ls_row.
WRITE / sy-subrc.
LOOP AT mt_rows INTO ls_row.
  WRITE / |{ ls_row-name } { ls_row-size }|.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\na 0\nb 1");
  });

  it("MODIFY TABLE, field symbol to row stays assigned", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         name TYPE string,
         size TYPE i,
       END OF ty_row.
TYPES ty_rows TYPE HASHED TABLE OF ty_row WITH UNIQUE KEY name.
DATA mt_rows TYPE ty_rows.
DATA ls_row TYPE ty_row.
FIELD-SYMBOLS <ls_row> TYPE ty_row.

ls_row-name = 'a'.
INSERT ls_row INTO TABLE mt_rows.
READ TABLE mt_rows WITH TABLE KEY name = 'a' ASSIGNING <ls_row>.
ls_row-size = 1.
MODIFY TABLE mt_rows FROM ls_row.
WRITE / <ls_row>-size.
<ls_row>-size = 2.
CLEAR ls_row.
READ TABLE mt_rows WITH TABLE KEY name = 'a' INTO ls_row.
WRITE / ls_row-size.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("MODIFY INDEX, TRANSPORTING", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         name  TYPE string,
         size  TYPE i,
         other TYPE i,
       END OF ty_row.
DATA mt_rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA ls_row TYPE ty_row.

ls_row-name = 'a'.
ls_row-size = 1.
ls_row-other = 1.
APPEND ls_row TO mt_rows.
ls_row-size = 2.
ls_row-other = 2.
MODIFY mt_rows FROM ls_row INDEX 1 TRANSPORTING size.
WRITE / sy-subrc.
CLEAR ls_row.
READ TABLE mt_rows INDEX 1 INTO ls_row.
WRITE / ls_row-size.
WRITE / ls_row-other.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\n2\n1");
  });

  it("READ TABLE FROM, standard default key, structure", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         name TYPE string,
         size TYPE i,
       END OF ty_row.
DATA mt_rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA ls_row TYPE ty_row.
DATA ls_read TYPE ty_row.

ls_row-name = 'a'.
ls_row-size = 1.
APPEND ls_row TO mt_rows.
ls_row-name = 'b'.
ls_row-size = 2.
APPEND ls_row TO mt_rows.
ls_row-size = 0.
READ TABLE mt_rows FROM ls_row INTO ls_read.
WRITE / sy-subrc.
WRITE / sy-tabix.
WRITE / ls_read-size.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\n2\n2");
  });

});