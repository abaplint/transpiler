import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";
import {tabl_t100xml} from "../_data";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_value.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - VALUE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic, single field", async () => {
    const code = `
TYPES: BEGIN OF ty,
         bar TYPE i,
       END OF ty.
DATA val TYPE ty.
val = VALUE #( bar = 2 ).
WRITE val-bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("basic, two fields", async () => {
    const code = `
TYPES: BEGIN OF ty,
         bar TYPE i,
         baz TYPE i,
       END OF ty.
DATA val TYPE ty.
val = VALUE #( bar = 2 baz = 3 ).
WRITE val-bar.
WRITE val-baz.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("23");
  });

  it("basic, table rows", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo TYPE i,
       END OF ty.
TYPES tty TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA tab TYPE tty.
tab = VALUE #( ( foo = 1 ) ( foo = 2) ).
WRITE / lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("table rows, sorted", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo TYPE i,
       END OF ty.
TYPES tty TYPE SORTED TABLE OF ty WITH UNIQUE KEY foo.
DATA tab TYPE tty.
DATA row LIKE LINE OF tab.
tab = VALUE #( ( foo = 2 ) ( foo = 1 ) ).
LOOP AT tab INTO row.
  WRITE / row-foo.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("table rows, hashed", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo TYPE i,
       END OF ty.
TYPES tty TYPE HASHED TABLE OF ty WITH UNIQUE KEY foo.
DATA tab TYPE tty.
DATA row LIKE LINE OF tab.
tab = VALUE #( ( foo = 2 ) ( foo = 1 ) ).
WRITE / lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("table rows, hashed, no named type", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo TYPE i,
       END OF ty.
DATA tab TYPE HASHED TABLE OF ty WITH UNIQUE KEY foo.
DATA row LIKE LINE OF tab.
tab = VALUE #( ( foo = 2 ) ( foo = 1 ) ).
WRITE / lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("row defaults kind of thing", async () => {
    const code = `
DATA tab TYPE RANGE OF i.
tab = VALUE #( sign   = 'I'
               option = 'EQ'
               ( low = 1 )
               ( low = 2 )
               ( low = 3 ) ).
WRITE / lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("VALUE DDIC table typed", async () => {
    const code = `
FORM foo.
  DATA(foo) = VALUE t100( ).
ENDFORM.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar_value.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}], {skipDatabaseSetup: true});
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("VALUE referring class type", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES tty TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA sdf TYPE lcl=>tty.
  sdf = VALUE lcl=>tty( ( 1 ) ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("VALUE referring interface type", async () => {
    const code = `
INTERFACE lif.
  TYPES tty TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
ENDINTERFACE.

START-OF-SELECTION.
  DATA sdf TYPE lif=>tty.
  sdf = VALUE lif=>tty( ( 1 ) ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.skip("VALUE empty", async () => {
    const code = `
    DATA val TYPE i.
    val = VALUE #( ).
    WRITE / val.`;
    const js = await run(code);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it.skip("VALUE FOR IN", async () => {
    const code = `
TYPES: BEGIN OF ty,
         val TYPE string,
       END OF ty.
DATA input TYPE STANDARD TABLE OF ty WITH EMPTY KEY.
DATA vals TYPE STANDARD TABLE OF string WITH EMPTY KEY.

INSERT VALUE #( val = 'hello' ) INTO TABLE input.
vals = VALUE #( FOR <input> IN input ( <input>-val ) ).
WRITE / lines( vals ).`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

});