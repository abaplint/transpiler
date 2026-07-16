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
    const js = await run(code);
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
    const js = await run(code);
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
    const js = await run(code);
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
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("with field symbol in FOR", async () => {
    const code = `
FORM run.
  TYPES: BEGIN OF ty,
           count TYPE i,
         END OF ty.
  DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
  INSERT VALUE #( count = 1 ) INTO TABLE tab.
  INSERT VALUE #( count = 2 ) INTO TABLE tab.
  DATA(lv_count) = REDUCE #( INIT val = 0 FOR <wa> IN tab NEXT val = val + <wa>-count ).
  WRITE / lv_count.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("with FILTER source", async () => {
    const code = `
FORM run.
  TYPES: BEGIN OF ty,
           count TYPE i,
         END OF ty.
  DATA tab TYPE SORTED TABLE OF ty WITH NON-UNIQUE KEY count.
  INSERT VALUE #( count = 1 ) INTO TABLE tab.
  INSERT VALUE #( count = 2 ) INTO TABLE tab.
  DATA(lv_count) = REDUCE #( INIT val = 0 FOR <wa> IN FILTER #( tab WHERE count < 5 ) NEXT val = val + <wa>-count ).
  WRITE / lv_count.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("index based FOR with WHILE", async () => {
    const code = `
DATA rv TYPE string.
DATA iv_times TYPE i.
iv_times = 3.
rv = REDUCE string( INIT text = \`\`
                    FOR i = 1 WHILE i <= iv_times
                    NEXT text = COND #( WHEN i = 1 THEN \`x\` ELSE |{ text }-x| ) ).
WRITE / rv.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("x-x-x");
  });

  it("index based FOR with UNTIL", async () => {
    const code = `
DATA sum TYPE i.
sum = REDUCE i( INIT s = 0 FOR j = 1 UNTIL j > 5 NEXT s = s + j ).
WRITE / sum.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("15");
  });

  it("index based FOR with THEN step", async () => {
    const code = `
DATA sum TYPE i.
sum = REDUCE i( INIT s = 0 FOR k = 0 THEN k + 2 WHILE k <= 6 NEXT s = s + k ).
WRITE / sum.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12");
  });

});