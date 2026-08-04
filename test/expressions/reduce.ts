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

  it("divided by lines()", async () => {
    const code = `
TYPES ty_ints TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA it TYPE ty_ints.
DATA rv TYPE i.
APPEND 10 TO it.
APPEND 20 TO it.
APPEND 30 TO it.
rv = REDUCE i( INIT s = 0 FOR x IN it NEXT s = s + x ) / lines( it ).
WRITE / rv.`;
    const js = await run(code);
    expect(js).to.not.contain("this.lines");
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("20");
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

  it("assigns INIT and returns the first INIT field", async () => {
    const code = `
TYPES ints TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA input TYPE ints.
DATA result TYPE i.
input = VALUE ints( ( 2 ) ( 3 ) ).
result = REDUCE i( INIT sum = 10 count = 40
                         FOR value IN input
                         NEXT sum = sum + value count = count + 1 ).
WRITE / result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("15");
  });

  it("outer LET", async () => {
    const code = `
TYPES ints TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA input TYPE ints.
DATA result TYPE i.
input = VALUE ints( ( 2 ) ).
result = REDUCE i( LET base = 5 IN
                         INIT sum = base
                         FOR value IN input
                         NEXT sum = sum + value ).
WRITE / result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("7");
  });

  it("multiple nested FOR clauses", async () => {
    const code = `
TYPES ints TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA input TYPE ints.
DATA result TYPE i.
input = VALUE ints( ( 1 ) ( 2 ) ).
result = REDUCE i( INIT sum = 0
                         FOR left IN input
                         FOR right IN input
                         NEXT sum += left * right ).
WRITE / result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("9");
  });

  it("table iteration FROM and TO", async () => {
    const code = `
TYPES ints TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA input TYPE ints.
DATA result TYPE i.
input = VALUE ints( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ).
result = REDUCE i( INIT sum = 0
                         FOR value IN input FROM 2 TO 3
                         NEXT sum = sum + value ).
WRITE / result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("FOR LET and INDEX INTO", async () => {
    const code = `
TYPES ints TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA input TYPE ints.
DATA result TYPE i.
input = VALUE ints( ( 2 ) ( 3 ) ).
result = REDUCE i( INIT sum = 0
                         FOR value IN input INDEX INTO index
                           LET weighted = value * index IN
                         NEXT sum = sum + weighted ).
WRITE / result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("8");
  });

  it("GROUPS iteration", async () => {
    const code = `
TYPES: BEGIN OF row_type,
         id TYPE i,
       END OF row_type.
DATA input TYPE STANDARD TABLE OF row_type WITH EMPTY KEY.
DATA result TYPE i.
input = VALUE #( ( id = 1 ) ( id = 1 ) ( id = 2 ) ).
result = REDUCE i( INIT sum = 0
                   FOR GROUPS group OF row IN input GROUP BY row-id
                   NEXT sum = sum + group ).
WRITE / result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

});
