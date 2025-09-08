import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_reduce.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - REDUCE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic, nothing found", async () => {
    const code = `
TYPES: BEGIN OF ty,
         type TYPE c LENGTH 1,
         val  TYPE string,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lv_count TYPE i.

lv_count = REDUCE i( INIT x = 0 FOR wa IN tab WHERE ( type = 'E' ) NEXT x = x + 1 ).
WRITE / lv_count.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("basic, one row matched", async () => {
    const code = `
TYPES: BEGIN OF ty,
         type TYPE c LENGTH 1,
         val  TYPE string,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lv_count TYPE i.

INSERT VALUE #( type = 'E' ) INTO TABLE tab.

lv_count = REDUCE i( INIT x = 0 FOR wa IN tab WHERE ( type = 'E' ) NEXT x = x + 1 ).
WRITE / lv_count.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("basic, two rows matched", async () => {
    const code = `
TYPES: BEGIN OF ty,
         type TYPE c LENGTH 1,
         val  TYPE string,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lv_count TYPE i.

INSERT VALUE #( type = 'E' val = 'Test' ) INTO TABLE tab.
INSERT VALUE #( type = 'E' val = 'Test2' ) INTO TABLE tab.

lv_count = REDUCE i( INIT x = 0 FOR wa IN tab WHERE ( type = 'E' ) NEXT x = x + 1 ).
WRITE / lv_count.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("basic, mix", async () => {
    const code = `
TYPES: BEGIN OF ty,
         type TYPE c LENGTH 1,
         val  TYPE string,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lv_count TYPE i.

INSERT VALUE #( type = 'E' val = 'Test' ) INTO TABLE tab.
INSERT VALUE #( type = 'I' val = 'Test2' ) INTO TABLE tab.

lv_count = REDUCE i( INIT x = 0 FOR wa IN tab WHERE ( type = 'E' ) NEXT x = x + 1 ).
WRITE / lv_count.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

});