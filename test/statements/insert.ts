import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_insert.prog.abap", contents}], {skipVersionCheck});
}

describe("Running statements - INSERT", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("INSERT INTO TABLE", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      INSERT 5 INTO TABLE tab.
      ASSERT lines( tab ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("INSERT INDEX, one time before loop pointer", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA row LIKE LINE OF tab.
      DO 3 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      LOOP AT tab INTO row.
        WRITE / row.
        IF row MOD 2 = 0.
          ASSERT lines( tab ) < 10.
          INSERT 5 INTO tab INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
      ASSERT lines( tab ) = 4.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

  it("INSERT INDEX, with SORT", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA row LIKE LINE OF tab.
      DO 4 TIMES.
        row = 5 - sy-index.
        APPEND row TO tab.
      ENDDO.
      LOOP AT tab INTO row.
        WRITE / row.
        SORT tab.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4\n2\n3\n4");
  });

  it("INSERT INTO TABLE 2", async () => {
    const code = `
      DATA bar TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA data LIKE LINE OF bar.
      INSERT 1 INTO TABLE bar.
      INSERT 2 INTO TABLE bar.
      LOOP AT bar INTO data.
        WRITE / data.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("INSERT ASSIGNING", async () => {
    const code = `
      DATA tab TYPE TABLE OF i.
      FIELD-SYMBOLS <i> TYPE i.
      INSERT 7 INTO TABLE tab ASSIGNING <i>.
      ASSERT <i> = 7.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("INSERT LINES OF", async () => {
    const code = `
  DATA tab1 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA tab2 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA val LIKE LINE OF tab1.
  APPEND 1 TO tab1.
  APPEND 2 TO tab2.
  INSERT LINES OF tab2 INTO TABLE tab1.
  LOOP AT tab1 INTO val.
    WRITE / val.
  ENDLOOP.
  ASSERT lines( tab1 ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("INSERT INDEX", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA int TYPE i.
  INSERT 1 INTO tab INDEX 1.
  INSERT 2 INTO tab INDEX 1.
  LOOP AT tab INTO int.
    WRITE / int.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n1");
  });

  it("INSERT REFERENCE INTO", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i.
DATA ref TYPE REF TO i.
DATA ref2 TYPE REF TO i.
DATA int TYPE i.

int = 2.

INSERT int INTO TABLE tab REFERENCE INTO ref.
WRITE / ref->*.

READ TABLE tab INDEX 1 REFERENCE INTO ref2.
WRITE / ref2->*.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n2");
  });

  it("INSERT string", async () => {
    const code = `
  DATA result TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  DATA str TYPE string.
  INSERT str INTO TABLE result.
  ASSERT lines( result ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("INSERT and references", async () => {
    const code = `
TYPES: BEGIN OF aggregated_data_type,
         group   TYPE i,
       END OF aggregated_data_type.
DATA aggregated_data TYPE STANDARD TABLE OF aggregated_data_type WITH DEFAULT KEY.
DATA temp1 LIKE LINE OF aggregated_data.
DATA aggregated LIKE REF TO temp1.
INSERT INITIAL LINE INTO TABLE aggregated_data REFERENCE INTO aggregated.
aggregated->group = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("INSERT, unique dashed key", async () => {
    const code = `
TYPES: BEGIN OF ty_file,
         path     TYPE string,
         filename TYPE string,
       END OF ty_file.
TYPES: BEGIN OF ty_stage,
         file TYPE ty_file,
       END OF ty_stage .
DATA tab TYPE SORTED TABLE OF ty_stage WITH UNIQUE KEY file-path file-filename.
DATA row LIKE LINE OF tab.
row-file-path = '1'.
row-file-filename = '1'.
INSERT row INTO TABLE tab.
row-file-path = '1'.
row-file-filename = '2'.
INSERT row INTO TABLE tab.
WRITE lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("INSERT, unique key", async () => {
    const code = `
TYPES:
  BEGIN OF ty_config,
    type TYPE string,
    name TYPE string,
  END OF ty_config .
TYPES ty_config_tt TYPE SORTED TABLE OF ty_config WITH UNIQUE KEY type name.
DATA tab TYPE ty_config_tt.
DATA row LIKE LINE OF tab.
INSERT row INTO TABLE tab.
INSERT row INTO TABLE tab.
WRITE / lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("INSERT, hashed table with default key", async () => {
    const code = `
DATA lt_tab TYPE HASHED TABLE OF string WITH UNIQUE DEFAULT KEY.
DATA val TYPE string.
val = |sdfsd|.
INSERT val INTO TABLE lt_tab.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("INSERT, with FS in sorted table", async () => {
    const code = `
TYPES:
  BEGIN OF ty_css_var,
    name  TYPE string,
    value TYPE string,
  END OF ty_css_var,
  ty_css_vars TYPE SORTED TABLE OF ty_css_var WITH UNIQUE KEY name.
DATA tab TYPE ty_css_vars.
DATA row LIKE LINE OF tab.
FIELD-SYMBOLS <fs> LIKE row.

ASSIGN row TO <fs>.

row-name = 'foo1'.
row-value = 1.
INSERT <fs> INTO TABLE tab.

row-name = 'foo2'.
row-value = 2.
INSERT <fs> INTO TABLE tab.

row-name = 'foo1'.
row-value = 3.
INSERT <fs> INTO TABLE tab.

ASSERT sy-subrc = 4.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("INSERT, LINES OF into HASHED from standard", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field1 TYPE c LENGTH 2,
       END OF ty.
DATA normal TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA hashed TYPE HASHED TABLE OF ty WITH UNIQUE KEY field1.
DATA row TYPE ty.

row-field1 = 'AA'.
INSERT row INTO TABLE normal.
row-field1 = 'BB'.
INSERT row INTO TABLE normal.

INSERT LINES OF normal INTO TABLE hashed.
WRITE lines( hashed ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("INSERT, LINES OF into HASHED from HASHED", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field1 TYPE c LENGTH 2,
       END OF ty.
DATA src TYPE HASHED TABLE OF ty WITH UNIQUE KEY field1.
DATA hashed TYPE HASHED TABLE OF ty WITH UNIQUE KEY field1.
DATA row TYPE ty.

row-field1 = 'AA'.
INSERT row INTO TABLE src.
row-field1 = 'BB'.
INSERT row INTO TABLE src.

INSERT LINES OF src INTO TABLE hashed.
WRITE lines( hashed ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("INSERT, basic sorted", async () => {
    const code = `
TYPES ty TYPE c LENGTH 20.
DATA lt_visited TYPE SORTED TABLE OF ty WITH UNIQUE DEFAULT KEY.
INSERT '00000000000003353284' INTO TABLE lt_visited.
ASSERT sy-subrc = 0.
INSERT '00000000000003353299' INTO TABLE lt_visited.
ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("INSERT, basic sorted, duplicate", async () => {
    const code = `
TYPES ty TYPE c LENGTH 20.
DATA lt_visited TYPE SORTED TABLE OF ty WITH UNIQUE DEFAULT KEY.
INSERT '00000000000003353284' INTO TABLE lt_visited.
ASSERT sy-subrc = 0.
INSERT '00000000000003353284' INTO TABLE lt_visited.
ASSERT sy-subrc = 4.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("INSERT, inline VALUE", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo TYPE i,
       END OF ty.
DATA lt_messages TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
INSERT VALUE #( foo = 2 ) INTO TABLE lt_messages.
READ TABLE lt_messages INDEX 1 TRANSPORTING NO FIELDS.
WRITE / sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

});