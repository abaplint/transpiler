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

  it.skip("target", async () => {
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

  it.only("with VALUE", async () => {
    const code = `
TYPES: BEGIN OF ty,
         msgno TYPE i,
       END OF ty.
DATA message_table TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA msgno TYPE i.
DATA row LIKE LINE OF message_table.
INSERT VALUE #( ) INTO TABLE message_table.
row = VALUE #( message_table[ msgno = msgno ] ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});