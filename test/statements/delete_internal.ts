import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - DELETE internal", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Basic delete internal", async () => {
    const code = `
      DATA table TYPE STANDARD TABLE OF i.
      APPEND 1 TO table.
      APPEND 2 TO table.
      DELETE table WHERE table_line = 1.
      ASSERT lines( table ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE from table INDEX 1", async () => {
    const code = `
      DATA foo TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      APPEND 2 TO foo.
      APPEND 3 TO foo.
      DELETE foo INDEX 1.
      ASSERT lines( foo ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE from table INDEX 2", async () => {
    const code = `
      DATA foo TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      APPEND 2 TO foo.
      APPEND 3 TO foo.
      DELETE foo INDEX 2.
      ASSERT lines( foo ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Basic delete ADJACENT DUPLICATES, no deleted", async () => {
    const code = `
      DATA table TYPE STANDARD TABLE OF i.
      APPEND 1 TO table.
      APPEND 2 TO table.
      DELETE ADJACENT DUPLICATES FROM table.
      ASSERT lines( table ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Basic delete ADJACENT DUPLICATES, one deleted", async () => {
    const code = `
      DATA table TYPE STANDARD TABLE OF i.
      APPEND 1 TO table.
      APPEND 2 TO table.
      APPEND 2 TO table.
      DELETE ADJACENT DUPLICATES FROM table.
      ASSERT lines( table ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE table FROM index", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA row LIKE LINE OF tab.
      DO 4 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      DELETE tab FROM 2.
      LOOP AT tab INTO row.
        WRITE / row.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("DELETE INITIAL where IS INITIAL", async () => {
    const code = `
      DATA lt_keywords TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
      APPEND '' TO lt_keywords.
      ASSERT lines( lt_keywords ) = 1.
      DELETE lt_keywords WHERE table_line IS INITIAL.
      ASSERT lines( lt_keywords ) = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("delete internal tab with object references", async () => {
    const code = `
      CLASS lcl_foo DEFINITION.
      ENDCLASS.
      CLASS lcl_foo IMPLEMENTATION.
      ENDCLASS.

      DATA tab TYPE STANDARD TABLE OF REF TO lcl_foo.
      DATA ref1 TYPE REF TO lcl_foo.
      DATA ref2 TYPE REF TO lcl_foo.
      CREATE OBJECT ref1.
      APPEND ref1 TO tab.
      CREATE OBJECT ref2.
      APPEND ref2 TO tab.

      DELETE tab INDEX 2.
      ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE ADJACENT DUPLICATES COMPARING, 1", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_foo,
          bar TYPE i,
          baz TYPE i,
        END OF ty_foo.
      DATA foo TYPE ty_foo.
      DATA footab TYPE TABLE OF ty_foo.
      DO 8 TIMES.
        foo-bar = sy-index DIV 2.
        foo-baz = sy-index MOD 4.
        APPEND foo TO footab.
      ENDDO.
      SORT footab BY baz.
      DELETE ADJACENT DUPLICATES FROM footab COMPARING baz.
      LOOP AT footab INTO foo.
        WRITE / |{ foo-bar }{ foo-baz }|.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("20\n01\n12\n13");
  });

  it("DELETE ADJACENT DUPLICATES COMPARING, 2", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_foo,
          bar TYPE i,
          baz TYPE i,
        END OF ty_foo.
      DATA foo TYPE ty_foo.
      DATA footab TYPE TABLE OF ty_foo.
      DO 8 TIMES.
        foo-bar = sy-index MOD 2.
        foo-baz = sy-index DIV 4.
        APPEND foo TO footab.
      ENDDO.
      SORT footab BY bar.
      DELETE ADJACENT DUPLICATES FROM footab COMPARING bar baz.
      LOOP AT footab INTO foo.
        WRITE / |{ foo-bar }{ foo-baz }|.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00\n01\n02\n10\n11");
  });

  it("DELETE ADJACENT DUPLICATES COMPARING ALL FIELDS", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_foo,
          bar TYPE i,
          baz TYPE i,
        END OF ty_foo.
      DATA foo TYPE ty_foo.
      DATA footab TYPE TABLE OF ty_foo.
      DO 8 TIMES.
        foo-bar = sy-index MOD 2.
        foo-baz = sy-index DIV 4.
        APPEND foo TO footab.
      ENDDO.
      SORT footab BY bar.
      DELETE ADJACENT DUPLICATES FROM footab COMPARING ALL FIELDS.
      LOOP AT footab INTO foo.
        WRITE / |{ foo-bar }{ foo-baz }|.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00\n01\n02\n10\n11");
  });

  it("DELETE WHERE method_call( ), check it compiles to valid JS", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    METHODS run.
    METHODS get_selected_commit RETURNING VALUE(val) TYPE string.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD run.
    TYPES: BEGIN OF ty_commit,
             sha1 TYPE string,
           END OF ty_commit.
    DATA lt_commits TYPE STANDARD TABLE OF ty_commit WITH DEFAULT KEY.
    DELETE lt_commits WHERE sha1 = get_selected_commit( ).
  ENDMETHOD.
  METHOD get_selected_commit.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA bar TYPE REF TO lcl_bar.
  CREATE OBJECT bar.
  bar->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE from table line", async () => {
    const code = `
DATA ignore TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lv_name TYPE string.
APPEND 'foo' TO ignore.
lv_name = 'foo'.
DELETE TABLE ignore FROM lv_name.
ASSERT lines( ignore ) = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("TO index", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i.
DATA int TYPE i.
DO 5 TIMES.
  APPEND sy-index TO tab.
ENDDO.
DELETE tab TO 3.
LOOP AT tab INTO int.
  WRITE / int.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4\n5");
  });

  it("DELETE fs INDEX", async () => {
    const code = `
DATA chain_tokens TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
FIELD-SYMBOLS <tokens> TYPE STANDARD TABLE.
ASSIGN chain_tokens TO <tokens>.
DELETE <tokens> INDEX 1.
WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("DELETE WHERE NOT IN", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA bar TYPE RANGE OF i.
FIELD-SYMBOLS <moo> LIKE LINE OF bar.
APPEND INITIAL LINE TO bar ASSIGNING <moo>.
<moo>-sign = 'I'.
<moo>-option = 'EQ'.
<moo>-low = 2.

DO 4 TIMES.
  APPEND sy-index TO tab.
ENDDO.
DELETE tab WHERE table_line NOT IN bar.

ASSERT lines( tab ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE WHERE IN", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA bar TYPE RANGE OF i.
FIELD-SYMBOLS <moo> LIKE LINE OF bar.
APPEND INITIAL LINE TO bar ASSIGNING <moo>.
<moo>-sign = 'I'.
<moo>-option = 'EQ'.
<moo>-low = 2.

DO 4 TIMES.
  APPEND sy-index TO tab.
ENDDO.
DELETE tab WHERE table_line IN bar.

ASSERT lines( tab ) = 3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE more IN", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA bar TYPE RANGE OF i.
FIELD-SYMBOLS <moo> LIKE LINE OF bar.
APPEND INITIAL LINE TO bar ASSIGNING <moo>.
<moo>-sign = 'I'.
<moo>-option = 'EQ'.
<moo>-low = 2.
APPEND INITIAL LINE TO bar ASSIGNING <moo>.
<moo>-sign = 'I'.
<moo>-option = 'EQ'.
<moo>-low = 4.

DO 4 TIMES.
  APPEND sy-index TO tab.
ENDDO.
DELETE tab WHERE table_line NOT IN bar.

ASSERT lines( tab ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE TO", async () => {
    const code = `
DATA lt_dists TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA lv_val LIKE LINE OF lt_dists.
DO 10 TIMES.
  APPEND sy-index TO lt_dists.
ENDDO.
DELETE lt_dists TO 5.
ASSERT lines( lt_dists ) = 5.
READ TABLE lt_dists INDEX 1 INTO lv_val.
WRITE lv_val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("6");
  });

  it("DELETE HASHED WHERE", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field1 TYPE c LENGTH 2,
         field2 TYPE c LENGTH 2,
       END OF ty.
DATA tab TYPE HASHED TABLE OF ty WITH UNIQUE KEY field1 field2.
DATA row LIKE LINE OF tab.

row-field1 = 'AA'.
INSERT row INTO TABLE tab.

DELETE tab WHERE field1 = 'AA'.

WRITE lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("DELETE, implicit index", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.

row-field = 6.
INSERT row INTO TABLE tab.
row-field = 3.
INSERT row INTO TABLE tab.
row-field = 10.
INSERT row INTO TABLE tab.

LOOP AT tab TRANSPORTING NO FIELDS WHERE field <= 5.
  DELETE tab.
  EXIT.
ENDLOOP.

LOOP AT tab INTO row.
  WRITE / row-field.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("6\n10");
  });

  it("DELETE, implicit index, early exit", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.

row-field = 6.
INSERT row INTO TABLE tab.
row-field = 3.
INSERT row INTO TABLE tab.
row-field = 3.
INSERT row INTO TABLE tab.
row-field = 10.
INSERT row INTO TABLE tab.

LOOP AT tab TRANSPORTING NO FIELDS WHERE field <= 5.
  DELETE tab.
  EXIT.
ENDLOOP.

LOOP AT tab INTO row.
  WRITE / row-field.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("6\n3\n10");
  });

  it("DELETE, messing with sy-tabix", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.

row-field = 6.
INSERT row INTO TABLE tab.
row-field = 3.
INSERT row INTO TABLE tab.
row-field = 3.
INSERT row INTO TABLE tab.
row-field = 10.
INSERT row INTO TABLE tab.

LOOP AT tab TRANSPORTING NO FIELDS WHERE field <= 5.
  sy-tabix = 1.
  DELETE tab.
  EXIT.
ENDLOOP.

LOOP AT tab INTO row.
  WRITE / row-field.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("6\n3\n10");
  });

  it("DELETE, with table key", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foobar TYPE i,
       END OF ty.
TYPES ty_tt TYPE HASHED TABLE OF ty WITH UNIQUE KEY foobar.

DATA itab TYPE ty_tt.
DATA row LIKE LINE OF itab.

row-foobar = 1.
INSERT row INTO TABLE itab.
row-foobar = 2.
INSERT row INTO TABLE itab.
row-foobar = 3.
INSERT row INTO TABLE itab.

DELETE TABLE itab WITH TABLE KEY foobar = 2.

LOOP AT itab INTO row.
  WRITE / row-foobar.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n3");
  });

  it("DELETE, with awaited statement", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS get_column_index RETURNING VALUE(index) TYPE i.
    METHODS foo.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD get_column_index.
  ENDMETHOD.

  METHOD foo.
    TYPES:
      BEGIN OF mty_s_hashed_column,
        column_index TYPE i,
        column       TYPE REF TO object,
      END OF mty_s_hashed_column,
      mty_ts_hashed_column TYPE HASHED TABLE OF mty_s_hashed_column WITH UNIQUE KEY column_index.

    DATA columns_hashed TYPE mty_ts_hashed_column.

    DELETE TABLE columns_hashed WITH TABLE KEY column_index = get_column_index( ).
  ENDMETHOD.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    // just test its valid syntax
  });

});