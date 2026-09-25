import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - SORT", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Basic sort table", async () => {
    const code = `
      DATA: table   TYPE STANDARD TABLE OF i,
            integer TYPE i.
      APPEND 2 TO table.
      APPEND 1 TO table.
      SORT table.
      LOOP AT table INTO integer.
        WRITE / integer.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("Basic sort table, descending", async() => {
    const code = `
      DATA: table   TYPE STANDARD TABLE OF i,
            integer TYPE i.
      APPEND 2 TO table.
      APPEND 3 TO table.
      APPEND 1 TO table.
      SORT table DESCENDING.
      LOOP AT table INTO integer.
        WRITE / integer.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3\n2\n1");
  });

  it("SORT structure", async () => {
    const code = `
      TYPES: BEGIN OF ty_structure,
              field TYPE i,
            END OF ty_structure.
      DATA tab TYPE STANDARD TABLE OF ty_structure WITH DEFAULT KEY.
      DATA row LIKE LINE OF tab.
      row-field = 2.
      APPEND row TO tab.
      row-field = 1.
      APPEND row TO tab.
      SORT tab BY field.
      LOOP AT tab INTO row.
        WRITE / row-field.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("SORT BY table_line", async () => {
    const code = `
      DATA lt_keywords TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
      APPEND 'foo' TO lt_keywords.
      APPEND 'bar' TO lt_keywords.
      SORT lt_keywords BY table_line ASCENDING.
      DATA keyword TYPE string.
      LOOP AT lt_keywords INTO keyword.
        WRITE / keyword.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar\nfoo");
  });

  it("SORT BY private", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
  PRIVATE SECTION.
    DATA foo TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    DATA tab TYPE STANDARD TABLE OF REF TO lcl.
    DATA ref TYPE REF TO lcl.
    CREATE OBJECT ref.
    INSERT ref INTO TABLE tab.
    INSERT ref INTO TABLE tab.
    SORT tab BY table_line->foo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SORT BY interfaced var", async () => {
    const code = `
INTERFACE lif.
  DATA foo TYPE i.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
    CLASS-METHODS run.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    DATA tab TYPE STANDARD TABLE OF REF TO lif.
    DATA ref TYPE REF TO lcl.
    CREATE OBJECT ref.
    INSERT ref INTO TABLE tab.
    INSERT ref INTO TABLE tab.
    SORT tab BY table_line->foo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SORT without BY, structure WITH DEFAULT KEY, sorts by the character-like components", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         name TYPE c LENGTH 2,
         n    TYPE i,
         s    TYPE string,
       END OF ty_row.
DATA lt_r TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA ls TYPE ty_row.
DATA lv_out TYPE string.
ls-name = 'b'. ls-n = 1. ls-s = \`x\`. APPEND ls TO lt_r.
ls-name = 'a'. ls-n = 3. ls-s = \`y\`. APPEND ls TO lt_r.
ls-name = 'a'. ls-n = 4. ls-s = \`y\`. APPEND ls TO lt_r.
ls-name = 'a'. ls-n = 5. ls-s = \`y\`. APPEND ls TO lt_r.
ls-name = 'a'. ls-n = 9. ls-s = \`a\`. APPEND ls TO lt_r.
ls-name = 'A'. ls-n = 2. ls-s = \`z\`. APPEND ls TO lt_r.
SORT lt_r.
LOOP AT lt_r INTO ls.
  lv_out = lv_out && |{ ls-name }{ ls-n }{ ls-s };|.
ENDLOOP.
WRITE / lv_out.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("A2z;a9a;a3y;a4y;a5y;b1x;");
  });

});
