import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_loop.prog.abap", contents}], {skipVersionCheck});
}

describe("Running statements - LOOP", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("LOOP AT pass by value", async () => {
    const code = `
      data tab type table of i.
      data val type i.
      append 2 to tab.
      loop at tab into val.
        write / val.
        val = 1.
      endloop.
      loop at tab into val.
        write / val.
      endloop.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n2");
  });

  it("LOOP at assigning", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    FIELD-SYMBOLS <row> TYPE i.
    APPEND 1 TO tab.
    LOOP AT tab ASSIGNING <row>.
      <row> = 2.
    ENDLOOP.
    LOOP AT tab ASSIGNING <row>.
      WRITE <row>.
    ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("LOOP, should set sy-tabix", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DO 3 TIMES.
        APPEND 1 TO tab.
      ENDDO.
      DATA sdf TYPE i.
      LOOP AT tab INTO sdf.
        WRITE / sy-tabix.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

  it("LOOP AT WHERE", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA line LIKE LINE OF tab.
      DO 5 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      LOOP AT tab INTO line WHERE table_line <> 3.
        WRITE / line.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n4\n5");
  });

  it("LOOP AT nothing with CONTINUE and EXIT", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA row LIKE LINE OF tab.
      LOOP AT tab INTO row.
        CONTINUE.
      ENDLOOP.
      LOOP AT tab INTO row.
        EXIT.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("LOOPing and DELETE in same table", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA row LIKE LINE OF tab.
      DO 4 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      ASSERT lines( tab ) = 4.
      LOOP AT tab INTO row.
        WRITE / sy-tabix.
        WRITE / row.
        DELETE tab INDEX 2.
      ENDLOOP.
      ASSERT lines( tab ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n1\n2\n3\n2\n4");
  });

  it("LOOPing FROM and TO", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i.
      DATA line TYPE i.
      DO 10 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      LOOP AT tab FROM 8 INTO line.
        WRITE line.
      ENDLOOP.
      WRITE / ''.
      LOOP AT tab TO 3 INTO line.
        WRITE line.
      ENDLOOP.
      WRITE / ''.
      LOOP AT tab FROM 5 TO 6 INTO line.
        WRITE line.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("8910\n 123\n 56");
  });

  it("LOOPing FROM and TO with var and fs", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i.
      DATA line TYPE i.
      DATA from TYPE i.
      FIELD-SYMBOLS <to> TYPE i.
      DO 10 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      from = 4.
      ASSIGN 7 TO <to>.
      LOOP AT tab FROM from TO <to> INTO line.
        WRITE line.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4567");
  });

  it("LOOPing FROM and TO and WHERE", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_struct,
          foo TYPE i,
          bar TYPE i,
        END OF ty_struct.
      DATA tab TYPE STANDARD TABLE OF ty_struct.
      DATA line TYPE ty_struct.
      DO 10 TIMES.
        line-foo = sy-index.
        line-bar = sy-index MOD 3.
        APPEND line TO tab.
        line-foo = sy-index * 3.
        line-bar = line-foo MOD 7 + 1.
        APPEND line TO tab.
      ENDDO.
      SORT tab BY foo bar.
      LOOP AT tab FROM 7 TO 13 INTO line WHERE foo < 8.
        WRITE |{ line-foo }-{ line-bar }.|.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("6-0.6-7.7-1.");
  });

  it("LOOPing FROM and TO, out of bounds", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i.
      DATA line TYPE i.
      DO 5 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      LOOP AT tab FROM -3 TO 17 INTO line.
        WRITE line.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12345");
  });

  it("LOOP AT <fs1> ASSIGNING <fs2>", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_tabtab,
          index TYPE i,
          str_tab TYPE TABLE OF string,
        END OF ty_tabtab.
      DATA tabtab TYPE ty_tabtab.
      DATA foo TYPE TABLE OF ty_tabtab.
      tabtab-index = 1.
      APPEND 'foo' TO tabtab-str_tab.
      APPEND 'bar' TO tabtab-str_tab.
      APPEND tabtab TO foo.
      tabtab-index = 3.
      CLEAR tabtab-str_tab[].
      APPEND 'meh' TO tabtab-str_tab.
      APPEND tabtab TO foo.
      FIELD-SYMBOLS <foo> TYPE ty_tabtab.
      FIELD-SYMBOLS <bar> TYPE string.
      LOOP AT foo ASSIGNING <foo>.
        LOOP AT <foo>-str_tab ASSIGNING <bar>.
          WRITE <bar>.
        ENDLOOP.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foobarmeh");
  });

  it("LOOP with multiple conditions", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_struct,
          foo TYPE i,
          bar TYPE i,
        END OF ty_struct.
      DATA tab TYPE STANDARD TABLE OF ty_struct.
      DATA line TYPE ty_struct.
      DO 10 TIMES.
        line-foo = sy-index.
        line-bar = sy-index MOD 3.
        APPEND line TO tab.
        line-foo = sy-index * 3.
        line-bar = line-foo MOD 7 + 1.
        APPEND line TO tab.
      ENDDO.
      SORT tab BY foo bar.
      LOOP AT tab INTO line WHERE foo < 10 AND bar >= 2.
        WRITE |{ line-foo }-{ line-bar }.|.
      ENDLOOP.
      WRITE / ''.
      LOOP AT tab INTO line WHERE foo > 25 OR bar = 7.
        WRITE |{ line-foo }-{ line-bar }.|.
      ENDLOOP.
      WRITE / ''.
      LOOP AT tab INTO line WHERE foo = 18 OR ( foo > 23 AND foo <= 30 ).
        WRITE |{ line-foo }-{ line-bar }.|.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2-2.3-4.5-2.6-7.8-2.9-3.\n 6-7.27-7.30-3.\n 18-5.24-4.27-7.30-3.");
  });

  it("LOOP should set sy-subrc", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    APPEND 2 TO tab.
    LOOP AT tab TRANSPORTING NO FIELDS WHERE table_line = 1.
    ENDLOOP.
    WRITE / sy-subrc.
    LOOP AT tab TRANSPORTING NO FIELDS WHERE table_line = 2.
    ENDLOOP.
    WRITE / sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4\n0");
  });

  it("LOOP, sy-subrc empty table", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.
LOOP AT tab INTO row.
  WRITE 'hello'.
ENDLOOP.
IF sy-subrc <> 0.
  WRITE 'bye'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bye");
  });

  it("LOOP, sy-subrc and EXIT first row", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA row LIKE LINE OF tab.
    APPEND 1 TO tab.
    sy-subrc = 4.
    LOOP AT tab INTO row.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      WRITE 'entered'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("entered");
  });

  it("LOOP, REFERENCE INTO", async () => {
    const code = `
TYPES: BEGIN OF ty_node,
         value TYPE i,
       END OF ty_node.
DATA tab TYPE STANDARD TABLE OF ty_node WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.
DATA ref TYPE REF TO ty_node.

APPEND row TO tab.

LOOP AT tab REFERENCE INTO ref.
  ref->value = 4.
ENDLOOP.
LOOP AT tab REFERENCE INTO ref.
  WRITE ref->value.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("LOOP and APPEND INITIl ASSIGNING", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  FIELD-SYMBOLS <row> LIKE LINE OF tab.
  APPEND INITIAL LINE TO tab ASSIGNING <row>.
  <row> = 1.
  APPEND INITIAL LINE TO tab ASSIGNING <row>.
  <row> = 2.
  LOOP AT tab ASSIGNING <row>.
    WRITE / <row>.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("LOOP refs", async () => {
    const code = `
TYPES: BEGIN OF ty_data,
         field TYPE i,
       END OF ty_data.
DATA data TYPE ty_data.
DATA ref TYPE REF TO ty_data.
DATA tab TYPE STANDARD TABLE OF REF TO ty_data.

GET REFERENCE OF data INTO ref.
INSERT ref INTO tab INDEX 1.
ref->field = 2.

LOOP AT tab INTO ref.
  WRITE ref->field.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("LOOP refs", async () => {
    const code = `
TYPES: BEGIN OF ty_data,
         field TYPE i,
       END OF ty_data.
DATA data TYPE ty_data.
DATA ref TYPE REF TO ty_data.
DATA tab TYPE STANDARD TABLE OF REF TO ty_data.

GET REFERENCE OF data INTO ref.
INSERT ref INTO tab INDEX 1.
ref->field = 2.

LOOP AT tab INTO ref.
  ref->field = 3.
ENDLOOP.

LOOP AT tab INTO ref.
  WRITE ref->field.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("LOOP fs", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA row LIKE LINE OF tab.
    FIELD-SYMBOLS <tab> LIKE tab.
    APPEND 'hello' TO tab.
    ASSIGN tab TO <tab>.
    LOOP AT <tab> INTO row.
      WRITE / row.
    ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("LOOP condition with paren", async () => {
    const code = `
TYPES: BEGIN OF ty_cell,
         row_from TYPE i,
         col_from TYPE i,
       END OF ty_cell.
DATA cells TYPE STANDARD TABLE OF ty_cell WITH DEFAULT KEY.
LOOP AT cells TRANSPORTING NO FIELDS
  WHERE ( row_from <= 1 ) AND ( col_from <= 2 ).
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("variable i exists", async () => {
    const code = `
DATA basket TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA row LIKE LINE OF basket.
DATA i TYPE i.
APPEND 2 TO basket.
LOOP AT basket INTO row WHERE table_line = i.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("LOOP condition with method call", async () => {
    const code = `
CLASS lcl_sub DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo RETURNING VALUE(foo) TYPE i.
ENDCLASS.

CLASS lcl_sub IMPLEMENTATION.
  METHOD foo.
    foo = 2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA row LIKE LINE OF tab.
  APPEND 1 TO tab.
  APPEND 2 TO tab.
  LOOP AT tab INTO row WHERE table_line = lcl_sub=>foo( ).
    WRITE row.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("LOOP USING secondary KEY, non-unique", async () => {
    const code = `
TYPES: BEGIN OF ty_node,
         name  TYPE string,
         index TYPE i,
       END OF ty_node.

DATA nodes TYPE SORTED TABLE OF ty_node
  WITH UNIQUE KEY name
  WITH NON-UNIQUE SORTED KEY array_index COMPONENTS index.
DATA row LIKE LINE OF nodes.

row-name = 'a'.
row-index = 2.
INSERT row INTO TABLE nodes.

row-name = 'b'.
row-index = 1.
INSERT row INTO TABLE nodes.

LOOP AT nodes INTO row USING KEY array_index.
  WRITE / row-name.
ENDLOOP.
LOOP AT nodes INTO row.
  WRITE / row-name.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("b\na\na\nb");
  });

  it("LOOP USING secondary KEY, dynamic hardcoded", async () => {
    const code = `
TYPES: BEGIN OF ty_node,
         name  TYPE string,
         index TYPE i,
       END OF ty_node.

DATA nodes TYPE SORTED TABLE OF ty_node
  WITH UNIQUE KEY name
  WITH NON-UNIQUE SORTED KEY array_index COMPONENTS index.
DATA row LIKE LINE OF nodes.

row-name = 'a'.
row-index = 2.
INSERT row INTO TABLE nodes.

row-name = 'b'.
row-index = 1.
INSERT row INTO TABLE nodes.

LOOP AT nodes INTO row USING KEY ('ARRAY_INDEX').
  WRITE / row-name.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("b\na");
  });

  it("LOOP USING secondary KEY, dynamic dynamic", async () => {
    const code = `
TYPES: BEGIN OF ty_node,
         name  TYPE string,
         index TYPE i,
       END OF ty_node.

DATA nodes TYPE SORTED TABLE OF ty_node
  WITH UNIQUE KEY name
  WITH NON-UNIQUE SORTED KEY array_index COMPONENTS index.
DATA row LIKE LINE OF nodes.

row-name = 'a'.
row-index = 2.
INSERT row INTO TABLE nodes.

row-name = 'b'.
row-index = 1.
INSERT row INTO TABLE nodes.

DATA name TYPE string.
name = 'ARRAY_INDEX'.
LOOP AT nodes INTO row USING KEY (name).
  WRITE / row-name.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("b\na");
  });

  it("LOOP, ASSIGNING KEY WHERE", async () => {
    const code = `
TYPES: BEGIN OF ty_node,
         name  TYPE string,
         index TYPE i,
       END OF ty_node.

DATA nodes TYPE HASHED TABLE OF ty_node
WITH UNIQUE KEY index
WITH NON-UNIQUE SORTED KEY array_name COMPONENTS name.
DATA row LIKE LINE OF nodes.
FIELD-SYMBOLS <n> LIKE LINE OF nodes.

row-name = 'a'.
row-index = 1.
INSERT row INTO TABLE nodes.

row-name = 'b'.
row-index = 2.
INSERT row INTO TABLE nodes.

DATA lv_tab_key TYPE string.
lv_tab_key = 'array_name'.
LOOP AT nodes ASSIGNING <n> USING KEY (lv_tab_key) WHERE name = 'a'.
  WRITE / <n>-name.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("a");
  });

  it("LOOP, ASSIGNING KEY WHERE hello, single occurrences", async () => {
    const code = `
FORM run.

  TYPES: BEGIN OF ty_node,
           path  TYPE string,
           name  TYPE string,
           index TYPE i,
           order TYPE i,
         END OF ty_node.

  TYPES:
    ty_nodes_ts TYPE SORTED TABLE OF ty_node WITH UNIQUE KEY path name
      WITH NON-UNIQUE SORTED KEY foobar COMPONENTS path index.

  DATA table TYPE ty_nodes_ts.
  DATA numc TYPE n LENGTH 2.
  FIELD-SYMBOLS <n> LIKE LINE OF table.
  DATA row LIKE LINE OF table.
  DATA lv_found TYPE i.

  DO 20 TIMES.
    numc = sy-index.
    row-path = 'hello' && numc.
    INSERT row INTO TABLE table.
  ENDDO.

  DO 20 TIMES.
    lv_found = 0.
    numc = sy-index.
    LOOP AT table ASSIGNING <n> USING KEY foobar WHERE path = 'hello' && numc.
      lv_found = lv_found + 1.
    ENDLOOP.
    ASSERT lv_found = 1.
  ENDDO.

ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("LOOP, ASSIGNING KEY WHERE hello, double occurrences", async () => {
    const code = `
FORM run.

  TYPES: BEGIN OF ty_node,
           path  TYPE string,
           name  TYPE string,
           index TYPE i,
           order TYPE i,
         END OF ty_node.

  TYPES:
    ty_nodes_ts TYPE SORTED TABLE OF ty_node WITH NON-UNIQUE KEY path name
      WITH NON-UNIQUE SORTED KEY foobar COMPONENTS path index.

  DATA table TYPE ty_nodes_ts.
  DATA numc TYPE n LENGTH 2.
  FIELD-SYMBOLS <n> LIKE LINE OF table.
  DATA row LIKE LINE OF table.
  DATA lv_found TYPE i.

  DO 9 TIMES.
    numc = sy-index.
    row-path = 'hello' && numc.
    INSERT row INTO TABLE table.
    INSERT row INTO TABLE table.
  ENDDO.

  DO 9 TIMES.
    lv_found = 0.
    numc = sy-index.
    LOOP AT table ASSIGNING <n> USING KEY foobar WHERE path = 'hello' && numc.
      lv_found = lv_found + 1.
    ENDLOOP.
    ASSERT lv_found = 2.
  ENDDO.

ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("AT FIRST, AT LAST", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.
APPEND 2 TO tab.
LOOP AT tab INTO row.
  AT FIRST.
    WRITE 1.
  ENDAT.
  WRITE row.
  AT LAST.
    WRITE 3.
  ENDAT.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("123");
  });

  it("AT FIRST, AT LAST, empty", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.
LOOP AT tab INTO row.
  AT FIRST.
    WRITE 1.
  ENDAT.
  WRITE row.
  AT LAST.
    WRITE 3.
  ENDAT.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("");
  });

  it("AT NEW", async () => {
    const code = `
TYPES: BEGIN OF ty,
         obj_name TYPE string,
       END OF ty.
DATA lt_status TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF lt_status.
FIELD-SYMBOLS <ls_status> LIKE LINE OF lt_status.

row-obj_name = 'foo'.
APPEND row TO lt_status.
APPEND row TO lt_status.
row-obj_name = 'bar'.
APPEND row TO lt_status.

LOOP AT lt_status ASSIGNING <ls_status>.
  AT NEW obj_name.
    WRITE / 'new'.
  ENDAT.
  WRITE / |middle { <ls_status>-obj_name }|.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`new
middle foo
middle foo
new
middle bar`);
  });

  it("AT END", async () => {
    const code = `
TYPES: BEGIN OF ty,
         obj_name TYPE string,
       END OF ty.
DATA lt_status TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF lt_status.
FIELD-SYMBOLS <ls_status> LIKE LINE OF lt_status.

row-obj_name = 'foo'.
APPEND row TO lt_status.
APPEND row TO lt_status.
row-obj_name = 'bar'.
APPEND row TO lt_status.

LOOP AT lt_status ASSIGNING <ls_status>.
  WRITE / |middle { <ls_status>-obj_name }|.
  AT END OF obj_name.
    WRITE / 'end'.
  ENDAT.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`middle foo
middle foo
end
middle bar
end`);
  });

  it("AT NEW, AT END", async () => {
    const code = `
TYPES: BEGIN OF ty,
         obj_name TYPE string,
       END OF ty.
DATA lt_status TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF lt_status.
FIELD-SYMBOLS <ls_status> LIKE LINE OF lt_status.

row-obj_name = 'foo'.
APPEND row TO lt_status.
APPEND row TO lt_status.
row-obj_name = 'bar'.
APPEND row TO lt_status.

LOOP AT lt_status ASSIGNING <ls_status>.
  AT NEW obj_name.
    WRITE / 'new'.
  ENDAT.
  WRITE / |middle { <ls_status>-obj_name }|.
  AT END OF obj_name.
    WRITE / 'end'.
  ENDAT.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`new
middle foo
middle foo
end
new
middle bar
end`);
  });

  it("insert into standard table during loop", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.
DATA lv_str TYPE string.
lv_str = |foo|.
INSERT lv_str INTO TABLE tab.

LOOP AT tab INTO row.
  IF lines( tab ) = 1.
    lv_str = |bar|.
    INSERT lv_str INTO TABLE tab.
  ENDIF.
  WRITE / row.
ENDLOOP.

WRITE / lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo\nbar\n2");
  });

  it("insert into hashed table during loop", async () => {
    const code = `
DATA tab TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
DATA row LIKE LINE OF tab.
DATA lv_str TYPE string.
lv_str = |foo|.
INSERT lv_str INTO TABLE tab.

LOOP AT tab INTO row.
  IF lines( tab ) = 1.
    lv_str = |bar|.
    INSERT lv_str INTO TABLE tab.
  ENDIF.
  WRITE / row.
ENDLOOP.

WRITE / lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo\nbar\n2");
  });

  it("LOOP, char sub", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE c LENGTH 10,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.

row-field = |World|.
INSERT row INTO TABLE tab.
row-field = |Hello|.
INSERT row INTO TABLE tab.

LOOP AT tab INTO row WHERE field(1) = 'W'.
  WRITE / row-field.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("World");
  });

  it("LOOP, dynamic where", async () => {
    const code = `
TYPES: BEGIN OF ty,
         BEGIN OF admin,
           id    TYPE i,
           state TYPE i,
         END OF admin,
       END OF ty.
DATA lt_data TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA ls_where TYPE ty-admin.
DATA lv_where TYPE string.
FIELD-SYMBOLS <ls_data> LIKE LINE OF lt_data.
FIELD-SYMBOLS <ls_where> LIKE ls_where.

lv_where = 'ADMIN-ID EQ <LS_WHERE>-ID AND ADMIN-STATE EQ <LS_WHERE>-STATE'.
ls_where-id = 123.
ASSIGN ls_where TO <ls_where>.
INSERT INITIAL LINE INTO TABLE lt_data.

LOOP AT lt_data ASSIGNING <ls_data> WHERE (lv_where).
  WRITE / 'found'.
ENDLOOP.
WRITE / sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("4");
  });

  it("LOOP, method call source", async () => {
    const code = `
TYPES ty_list TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS list RETURNING VALUE(list) TYPE ty_list.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD list.
    INSERT 2 INTO TABLE list.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA val TYPE i.
  LOOP AT lcl=>list( ) INTO val.
    WRITE / val.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("2");
  });

  it("LOOP in FORM with inline DATA declarations", async () => {
    const code = `
FORM foo.
  DATA tab1 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA tab2 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

  INSERT 1 INTO TABLE tab1.
  INSERT 2 INTO TABLE tab2.

  LOOP AT tab1 INTO DATA(val1).
    LOOP AT tab2 INTO DATA(val2).
      WRITE: / val1, val2.
    ENDLOOP.
  ENDLOOP.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("12");
  });

  it.only("LOOP in METHOD with inline DATA declarations", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    DATA tab1 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA tab2 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    INSERT 1 INTO TABLE tab1.
    INSERT 2 INTO TABLE tab2.

    LOOP AT tab1 INTO DATA(val1).
      LOOP AT tab2 INTO DATA(val2).
        WRITE: / val1, val2.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>foo( ).`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("12");
  });

});