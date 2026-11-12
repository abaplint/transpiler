import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_table_expr.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - Table Expression", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic index", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
INSERT 2 INTO TABLE tab.
WRITE / tab[ 1 ].`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("basic index, second", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
INSERT 2 INTO TABLE tab.
INSERT 3 INTO TABLE tab.
WRITE / tab[ 2 ].`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("basic field condition", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo TYPE i,
         bar TYPE i,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
INSERT VALUE #( foo = 2 bar = 3 ) INTO TABLE tab.
WRITE / tab[ foo = 2 ]-bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("basic field condition, table line reference", async () => {
    const code = `
TYPES: BEGIN OF ty,
         name TYPE string,
         bar  TYPE i,
       END OF ty.
DATA int TYPE STANDARD TABLE OF REF TO ty WITH DEFAULT KEY.
DATA row TYPE REF TO ty.

CREATE DATA row.
row->name = 'hello'.
row->bar = 2.
INSERT row INTO TABLE int.
WRITE / int[ table_line->name = 'hello' ]->bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("calculated index", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
INSERT 2 INTO TABLE tab.
WRITE / tab[ 0 + 1 ].`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("table line", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
INSERT 2 INTO TABLE tab.
WRITE / tab[ table_line = 2 ].`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("multiple conditions", async () => {
    const code = `
TYPES: BEGIN OF ty,
         name TYPE string,
         bar  TYPE i,
       END OF ty.
DATA int TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
INSERT VALUE #( name = 'hello' bar = 2 ) INTO TABLE int.
WRITE / int[ name = 'hello' bar = 2 ]-bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("throw exception", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
WRITE / tab[ 1 ].`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);

    try {
      await f(abap);
      expect.fail();
    } catch (e) {
      expect(e.toString()).to.contain("CX_SY_ITAB_LINE_NOT_FOUND");
    }
  });

  it("target", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
INSERT 2 INTO TABLE tab.
tab[ 1 ] = 3.
WRITE / tab[ 1 ].`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("target, calculated index", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
INSERT 2 INTO TABLE tab.
tab[ 0 + 1 ] = 3.
WRITE / tab[ 1 ].`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("target, structured", async () => {
    const code = `
TYPES: BEGIN OF ty,
         bar TYPE i,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
INSERT VALUE #( bar = 2 ) INTO TABLE tab.
tab[ 1 ]-bar = 3.
WRITE / tab[ 1 ]-bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("with VALUE", async () => {
    const code = `
TYPES: BEGIN OF ty,
         msgno TYPE i,
       END OF ty.
DATA message_table TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA msgno TYPE i.
DATA row LIKE LINE OF message_table.
INSERT VALUE #( msgno = msgno ) INTO TABLE message_table.
row = VALUE #( message_table[ msgno = msgno ] ).
WRITE / row-msgno.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("with method call, for index", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_index RETURNING VALUE(rv_index) TYPE i.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD get_index.
    rv_index = 1.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  INSERT VALUE #( ) INTO TABLE tab.
  WRITE / tab[ lcl=>get_index( ) ].`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("with method call, where", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get RETURNING VALUE(int) TYPE i.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD get.
    int = 0.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  TYPES: BEGIN OF ty,
           field TYPE i,
         END OF ty.
  DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
  INSERT VALUE #( ) INTO TABLE tab.
  WRITE / tab[ field = lcl=>get( ) ]-field.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("int8 index", async () => {
    const code = `
DATA foo TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
INSERT 1 INTO TABLE foo.
DATA index TYPE int8 VALUE 1.
WRITE / foo[ index ].`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });


  it("KEY name COMPONENTS and KEY name INDEX", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         id   TYPE i,
         name TYPE c LENGTH 1,
       END OF ty_row.
DATA tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY WITH UNIQUE SORTED KEY by_name COMPONENTS name.
tab = VALUE #( ( id = 1 name = 'b' ) ( id = 2 name = 'a' ) ( id = 3 name = 'c' ) ).
WRITE / tab[ KEY by_name COMPONENTS name = 'b' ]-id.
WRITE / tab[ KEY by_name name = 'c' ]-id.
WRITE / tab[ KEY by_name INDEX 1 ]-id.
WRITE / tab[ KEY primary_key INDEX 1 ]-id.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n3\n2\n1");
  });

  it("line_index through a hashed secondary key", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH EMPTY KEY WITH UNIQUE HASHED KEY h COMPONENTS table_line.
DATA lv_line TYPE string.
tab = VALUE #( ( 7 ) ( 8 ) ( 9 ) ).
lv_line = |{ line_index( tab[ KEY h table_line = 8 ] ) } { line_index( tab[ KEY h table_line = 5 ] ) }|.
WRITE lv_line.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("-1 0");
  });
});