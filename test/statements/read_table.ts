import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - READ TABLE", () => {

  beforeEach(async () => {
    abap = new ABAP(new MemoryConsole());
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

  it("READ TABLE, with table key, found", async () => {
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

  it("READ TABLE, with table key, not found", async () => {
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
row-name = 'sdfsd'.
READ TABLE lt_tab FROM row TRANSPORTING NO FIELDS.
ASSERT sy-subrc = 8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("nested structure", async () => {
    const code = `
TYPES: BEGIN OF ty,
         BEGIN OF sub,
           foo TYPE i,
         END OF sub,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
APPEND INITIAL LINE TO tab.
READ TABLE tab WITH KEY sub-foo = 2 TRANSPORTING NO FIELDS.
WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("READ TABLE table_line, structured", async () => {
    const code = `
TYPES: BEGIN OF ty,
         name TYPE string,
         int  TYPE i,
       END OF ty.
DATA nodes TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA l_node LIKE LINE OF nodes.
l_node-name = 'ASDF'.
l_node-int = 2.
READ TABLE nodes WITH KEY table_line = l_node TRANSPORTING NO FIELDS.
WRITE / sy-subrc.
INSERT l_node INTO TABLE nodes.
READ TABLE nodes WITH KEY table_line = l_node TRANSPORTING NO FIELDS.
WRITE / sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4\n0");
  });

  it("READ TABLE assigning index", async () => {
    const code = `
data tab type standard table of i.
field-symbols <fs> like line of tab.
read table tab assigning <fs> index sy-tfill.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("ref, arrow", async () => {
    const code = `
TYPES: BEGIN OF bar,
         name TYPE string,
       END OF bar.
TYPES ty TYPE REF TO bar.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.
READ TABLE tab WITH KEY table_line->name = 'hello' INTO row.
ASSERT sy-subrc = 4.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("FROM INTO", async () => {
    const code = `
TYPES: BEGIN OF t_name_value,
         name  TYPE string,
         value TYPE string,
       END OF t_name_value.
DATA params TYPE SORTED TABLE OF t_name_value WITH UNIQUE KEY name.
DATA row LIKE LINE OF params.
DATA res LIKE LINE OF params.
DATA lr_res TYPE REF TO t_name_value.

row-name = 'foo'.
row-value = 'hello'.
APPEND row TO params.
CLEAR row-value.

READ TABLE params FROM row INTO res.
WRITE res-value.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("FROM field symbol INTO", async () => {
    const code = `
TYPES: BEGIN OF t_name_value,
         name  TYPE string,
         value TYPE string,
       END OF t_name_value.
DATA params TYPE SORTED TABLE OF t_name_value WITH UNIQUE KEY name.
DATA row LIKE LINE OF params.
FIELD-SYMBOLS <row> LIKE row.
DATA res LIKE LINE OF params.
DATA lr_res TYPE REF TO t_name_value.

row-name = 'foo'.
row-value = 'hello'.
APPEND row TO params.
CLEAR row-value.
ASSIGN row TO <row>.

READ TABLE params FROM <row> INTO res.
WRITE res-value.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("field symbol FROM field symbol INTO", async () => {
    const code = `
TYPES: BEGIN OF t_name_value,
         name  TYPE string,
         value TYPE string,
       END OF t_name_value.
DATA params TYPE SORTED TABLE OF t_name_value WITH UNIQUE KEY name.
FIELD-SYMBOLS <fs> LIKE params.
DATA row LIKE LINE OF params.
FIELD-SYMBOLS <row> LIKE row.
DATA res LIKE LINE OF params.
DATA lr_res TYPE REF TO t_name_value.

row-name = 'foo'.
row-value = 'hello'.
APPEND row TO params.
CLEAR row-value.
ASSIGN row TO <row>.

ASSIGN params TO <fs>.
READ TABLE <fs> FROM <row> INTO res.
WRITE res-value.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("clike structure into string", async () => {
    const code = `
TYPES: BEGIN OF ty,
         name TYPE c LENGTH 10,
       END OF ty.
DATA keys TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF keys.
DATA key_name TYPE string.

row-name = 'hello'.
APPEND row TO keys.

READ TABLE keys INDEX 1 INTO key_name.

WRITE key_name.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("simple binary search, last found", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DO 5 TIMES.
  APPEND sy-index TO tab.
ENDDO.
READ TABLE tab WITH KEY table_line = 5 TRANSPORTING NO FIELDS BINARY SEARCH.
ASSERT sy-subrc = 0.
ASSERT sy-tabix = 5.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("simple binary search, middle found", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DO 5 TIMES.
  APPEND sy-index TO tab.
ENDDO.
READ TABLE tab WITH KEY table_line = 2 TRANSPORTING NO FIELDS BINARY SEARCH.
ASSERT sy-subrc = 0.
ASSERT sy-tabix = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("simple binary search, not found", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DO 5 TIMES.
  APPEND sy-index TO tab.
ENDDO.
READ TABLE tab WITH KEY table_line = 456 TRANSPORTING NO FIELDS BINARY SEARCH.
ASSERT sy-subrc = 8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("binary search, find first occurrence", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field1 TYPE i,
         field2 TYPE i,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.
DO 5 TIMES.
  row-field1 = 5.
  row-field2 = sy-index.
  APPEND row TO tab.
ENDDO.
READ TABLE tab WITH KEY field1 = 5 TRANSPORTING NO FIELDS BINARY SEARCH.
ASSERT sy-subrc = 0.
ASSERT sy-tabix = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("COMPONENTS subrc 8", async () => {
    const code = `
TYPES: BEGIN OF ty_file,
         path     TYPE string,
         filename TYPE string,
       END OF ty_file.
TYPES ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY
      WITH UNIQUE SORTED KEY file_path COMPONENTS path filename.

DATA ct_remote TYPE ty_files_tt.
FIELD-SYMBOLS <ls_remote> LIKE LINE OF ct_remote.

READ TABLE ct_remote ASSIGNING <ls_remote>
  WITH KEY file_path COMPONENTS path = 'sdf'.
WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("8");
  });

  it.skip("COMPONENTS subrc 4", async () => {
    const code = `
TYPES: BEGIN OF ty_file,
         path     TYPE string,
         filename TYPE string,
       END OF ty_file.
TYPES ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY
      WITH UNIQUE SORTED KEY file_path COMPONENTS path filename.

DATA ct_remote TYPE ty_files_tt.
DATA row LIKE LINE OF ct_remote.
FIELD-SYMBOLS <ls_remote> LIKE LINE OF ct_remote.

row-path = 'zzz'.
INSERT row INTO TABLE ct_remote.

READ TABLE ct_remote ASSIGNING <ls_remote>
  WITH KEY file_path COMPONENTS path = 'sdf'.
WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("Fieldsymbol index", async () => {
    const code = `
DATA mt_symbol TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
FIELD-SYMBOLS <lv_symbol> LIKE LINE OF mt_symbol.
FIELD-SYMBOLS <lv_offset> LIKE LINE OF mt_symbol.
APPEND 1 TO mt_symbol.
APPEND 1 TO mt_symbol.
READ TABLE mt_symbol INDEX 1 ASSIGNING <lv_offset>.
ASSERT sy-subrc = 0.
READ TABLE mt_symbol INDEX <lv_offset> + 1 ASSIGNING <lv_symbol>.
ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("floated index", async () => {
    const code = `
DATA mt_symbol TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA lv_f TYPE f.
FIELD-SYMBOLS <lv_offset> LIKE LINE OF mt_symbol.
lv_f = 1.
APPEND 1 TO mt_symbol.
READ TABLE mt_symbol INDEX lv_f ASSIGNING <lv_offset>.
ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("read table with table line type = interface", async () => {
    const code = `
INTERFACE lif.
  DATA foo TYPE string.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
    METHODS constructor.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD constructor.
    lif~foo = 'bar'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  DATA tab TYPE STANDARD TABLE OF REF TO lif WITH DEFAULT KEY.
  CREATE OBJECT lo.
  APPEND lo TO tab.
  READ TABLE tab WITH KEY table_line->foo = 'bar' TRANSPORTING NO FIELDS.
  ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("READ TABLE HASHED WITH TABLE KEY, non sorted sequence", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field1 TYPE c LENGTH 2,
       END OF ty.
DATA src TYPE HASHED TABLE OF ty WITH UNIQUE KEY field1.
DATA row TYPE ty.

row-field1 = 'BB'.
INSERT row INTO TABLE src.
row-field1 = 'AA'.
INSERT row INTO TABLE src.

READ TABLE src WITH TABLE KEY field1 = 'AA' INTO row.
ASSERT sy-subrc = 0.
READ TABLE src WITH TABLE KEY field1 = 'BB' INTO row.
ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("READ TABLE HASHED WITH TABLE KEY table_line", async () => {
    const code = `
DATA src TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
DATA row TYPE string.

row = 'BB'.
INSERT row INTO TABLE src.
row = 'AA'.
INSERT row INTO TABLE src.

READ TABLE src WITH TABLE KEY table_line = 'AA' INTO row.
ASSERT sy-subrc = 0.
READ TABLE src WITH TABLE KEY table_line = 'BB' INTO row.
ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("READ TABLE HASHED WITH free key", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field1 TYPE c LENGTH 2,
         field2 TYPE i,
       END OF ty.
DATA src TYPE HASHED TABLE OF ty WITH UNIQUE KEY field1.
DATA row TYPE ty.

row-field1 = 'BB'.
INSERT row INTO TABLE src.

READ TABLE src WITH KEY field2 = 0 INTO row.
ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("READ TABLE HASHED WITH free key overlapping primary key", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field1 TYPE string,
         field2 TYPE i,
       END OF ty.
DATA tab TYPE HASHED TABLE OF ty WITH UNIQUE KEY field1 field2.
DATA row LIKE LINE OF tab.
FIELD-SYMBOLS <fs> LIKE LINE OF tab.

DO 2 TIMES.
  row-field1 = |foo{ sy-index }|.
  row-field2 = sy-index.
  INSERT row INTO TABLE tab.
ENDDO.

READ TABLE tab WITH KEY
  field1 = |foo1|
  field2 = 1
  ASSIGNING <fs>.
ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("READ TABLE standard, unsorted WITH KEY", async () => {
    const code = `
TYPES: BEGIN OF ty_asset_entry,
         url TYPE string,
       END OF ty_asset_entry.
TYPES ty_asset_register TYPE STANDARD TABLE OF ty_asset_entry WITH KEY url.
DATA tab TYPE ty_asset_register.
DATA row LIKE LINE OF tab.

row-url = 'url3'.
INSERT row INTO TABLE tab.
row-url = 'url2'.
INSERT row INTO TABLE tab.
row-url = 'url1'.
INSERT row INTO TABLE tab.

READ TABLE tab WITH KEY url = 'url1' TRANSPORTING NO FIELDS.
WRITE / sy-subrc.
READ TABLE tab WITH KEY url = 'url2' TRANSPORTING NO FIELDS.
WRITE / sy-subrc.
READ TABLE tab WITH KEY url = 'url3' TRANSPORTING NO FIELDS.
WRITE / sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get().trimEnd()).to.equal("0\n0\n0");
  });

  it("READ TABLE standard, unsorted WITH TABLE KEY", async () => {
    const code = `
TYPES: BEGIN OF ty_asset_entry,
         url TYPE string,
       END OF ty_asset_entry.
TYPES ty_asset_register TYPE STANDARD TABLE OF ty_asset_entry WITH KEY url.
DATA tab TYPE ty_asset_register.
DATA row LIKE LINE OF tab.

row-url = 'url1'.
INSERT row INTO TABLE tab.
row-url = 'url3'.
INSERT row INTO TABLE tab.
row-url = 'url2'.
INSERT row INTO TABLE tab.

READ TABLE tab WITH TABLE KEY url = 'url1' TRANSPORTING NO FIELDS.
WRITE / sy-subrc.
READ TABLE tab WITH TABLE KEY url = 'url2' TRANSPORTING NO FIELDS.
WRITE / sy-subrc.
READ TABLE tab WITH TABLE KEY url = 'url3' TRANSPORTING NO FIELDS.
WRITE / sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get().trimEnd()).to.equal("0\n0\n0");
  });

  it("READ TABLE, char into string", async () => {
    const code = `
    TYPES ty TYPE c LENGTH 10.
    DATA lt_names TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
    DATA row LIKE LINE OF lt_names.
    DATA key_name TYPE string.
    row = |foo |.
    INSERT row INTO TABLE lt_names.
    read table lt_names index 1 into key_name.
    ASSERT key_name = |foo|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("READ TABLE, structured into string", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo TYPE c LENGTH 10,
       END OF ty.
DATA lt_names TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF lt_names.
DATA key_name TYPE string.
row-foo = |foo |.
INSERT row INTO TABLE lt_names.
READ TABLE lt_names INDEX 1 INTO key_name.
ASSERT key_name = |foo|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("READ TABLE, hashed, first name", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         name TYPE c LENGTH 30,
         kind TYPE c LENGTH 1,
       END OF ty_row.
TYPES ty_tab TYPE HASHED TABLE OF ty_row WITH UNIQUE KEY name.
DATA tab TYPE ty_tab.
DATA row LIKE LINE OF tab.

row-name = 'FIRST_NAME'.
INSERT row INTO TABLE tab.

CLEAR row.
READ TABLE tab WITH KEY name = 'FIRST_NAME' INTO row.
ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});