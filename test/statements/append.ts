import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_append.prog.abap", contents}], {skipVersionCheck});
}

describe("Running statements - APPEND", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
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

  it("APPEND should set tabix", async () => {
    const code = `
DATA int_list TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DO 2 TIMES.
  APPEND sy-tabix TO int_list.
  WRITE / sy-tabix.
ENDDO.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("APPEND TO", async () => {
    const code = `
  DATA tab1 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA tab2 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DO 5 TIMES.
    APPEND sy-index TO tab1.
  ENDDO.
  APPEND LINES OF tab1 TO 2 TO tab2.
  ASSERT lines( tab2 ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("APPEND LINES OF lower case", async () => {
    const code = `
  DATA tab1 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA tab2 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DO 5 TIMES.
    APPEND sy-index TO tab1.
  ENDDO.
  append lines of tab1 TO 2 TO tab2.
  ASSERT lines( tab2 ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("APPEND, ASSIGNING fs, structured", async () => {
    const code = `
TYPES:
  BEGIN OF ty_node,
    name     TYPE string,
    children TYPE i,
  END OF ty_node.
TYPES:
  ty_nodes_tt TYPE STANDARD TABLE OF ty_node WITH DEFAULT KEY.
DATA ct_nodes TYPE ty_nodes_tt.
DATA ls_root LIKE LINE OF ct_nodes.
FIELD-SYMBOLS <root> LIKE ls_root.
APPEND ls_root TO ct_nodes ASSIGNING <root>.
<root>-children = <root>-children + 123.
READ TABLE ct_nodes INDEX 1 INTO ls_root.
WRITE ls_root-children.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("123");
  });

  it("APPEND, TO TO, 1", async () => {
    const code = `
  DATA tab1 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA tab2 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DO 10 TIMES.
    APPEND 2 TO tab1.
  ENDDO.

  APPEND LINES OF tab1 FROM 2 TO tab2.
  ASSERT lines( tab2 ) = 9.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("APPEND, TO TO, 2", async () => {
    const code = `
  DATA tab1 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA tab2 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DO 10 TIMES.
    APPEND 2 TO tab1.
  ENDDO.

  APPEND LINES OF tab1 FROM 1 TO 3 TO tab2.
  ASSERT lines( tab2 ) = 3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("APPEND, TO field symbol target", async () => {
    const code = `
    DATA tab1 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA tab2 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    FIELD-SYMBOLS <tab2> LIKE tab2.
    APPEND 1 TO tab1.
    ASSIGN tab2 TO <tab2>.
    APPEND LINES OF tab1 TO <tab2>.
    ASSERT lines( tab2 ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("APPEND, TO field symbol target, structured", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo TYPE i,
       END OF ty.
DATA ls TYPE ty.
DATA tab1 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA tab2 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
FIELD-SYMBOLS <tab2> LIKE tab2.
ls-foo = 5.
APPEND ls TO tab1.
ASSIGN tab2 TO <tab2>.
APPEND LINES OF tab1 TO <tab2>.
ASSERT lines( tab2 ) = 1.
ASSERT lines( <tab2> ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("APPEND, TO field symbol target, structured, table typed", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo TYPE i,
       END OF ty.
TYPES tytab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA ls TYPE ty.
DATA tab1 TYPE tytab.
DATA tab2 TYPE tytab.
FIELD-SYMBOLS <tab2> TYPE tytab.
ls-foo = 5.
APPEND ls TO tab1.
ASSIGN tab2 TO <tab2>.
APPEND LINES OF tab1 TO <tab2>.
ASSERT lines( tab2 ) = 1.
ASSERT lines( <tab2> ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("APPEND LINES, source field symbol", async () => {
    const code = `
  DATA tab1 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA tab2 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA row LIKE LINE OF tab2.
  FIELD-SYMBOLS <tab1> LIKE tab1.
  APPEND 1 TO tab1.
  ASSIGN tab1 TO <tab1>.
  APPEND LINES OF <tab1> TO tab2.
  LOOP AT tab2 INTO row.
    WRITE row.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("APPEND reference into", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA nested_data TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA temp2 TYPE ty.
DATA nested_artist TYPE REF TO ty.
temp2-field = 2.
APPEND temp2 TO nested_data REFERENCE INTO nested_artist.
WRITE nested_artist->field.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("OCCURS", async () => {
    const code = `
DATA tab TYPE i OCCURS 0.
APPEND 2 TO tab.
WRITE lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("WITH HEADER LINE, short APPEND", async () => {
    const code = `
DATA tab TYPE i OCCURS 0 WITH HEADER LINE.
tab = 3.
APPEND tab.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("APPEND, inline VALUE", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo TYPE i,
       END OF ty.
DATA lt_messages TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
APPEND VALUE #( foo = 2 ) TO lt_messages.
READ TABLE lt_messages INDEX 1 TRANSPORTING NO FIELDS.
WRITE / sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

});