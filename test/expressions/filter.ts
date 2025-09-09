import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_filter.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - FILTER", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
FORM foo.
  TYPES: BEGIN OF ty,
          field1 TYPE i,
          field2 TYPE i,
        END OF ty.
  DATA tab TYPE SORTED TABLE OF ty WITH UNIQUE KEY field1.

  INSERT VALUE #( field1 = 1 ) INTO TABLE tab.
  INSERT VALUE #( field1 = 2 ) INTO TABLE tab.

  DATA(res) = FILTER #( tab WHERE field1 = 2 ).
  WRITE / lines( res ).
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it.skip("EXCEPT IN, different names", async () => {
    const code = `
TYPES: BEGIN OF ty,
         id    TYPE i,
         value TYPE c LENGTH 10,
       END OF ty.
DATA et_list TYPE SORTED TABLE OF ty WITH UNIQUE KEY id.

TYPES: BEGIN OF ty2,
         id2   TYPE i,
         value TYPE c LENGTH 10,
       END OF ty2.
DATA lt_list TYPE SORTED TABLE OF ty2 WITH UNIQUE KEY id2.

INSERT VALUE #( id = 1 ) INTO TABLE et_list.
INSERT VALUE #( id2 = 2 ) INTO TABLE lt_list.

et_list = FILTER #( et_list EXCEPT IN lt_list WHERE id = id2 ).

WRITE / lines( et_list ).`;
    const js = await run(code);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it.skip("EXCEPT IN, empty list", async () => {
    const code = `
TYPES: BEGIN OF ty,
         id    TYPE i,
         value TYPE c LENGTH 10,
       END OF ty.
DATA et_list TYPE SORTED TABLE OF ty WITH UNIQUE KEY id.
DATA lt_list TYPE SORTED TABLE OF ty WITH UNIQUE KEY id.

INSERT VALUE #( id = 1 ) INTO TABLE et_list.

et_list = FILTER #( et_list EXCEPT IN lt_list WHERE id = id ).

WRITE / lines( et_list ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it.skip("EXCEPT IN, filter hit", async () => {
    const code = `
TYPES: BEGIN OF ty,
         id    TYPE i,
         value TYPE c LENGTH 10,
       END OF ty.
DATA et_list TYPE SORTED TABLE OF ty WITH UNIQUE KEY id.
DATA lt_list TYPE SORTED TABLE OF ty WITH UNIQUE KEY id.

INSERT VALUE #( id = 1 ) INTO TABLE et_list.
INSERT VALUE #( id = 1 ) INTO TABLE lt_list.

et_list = FILTER #( et_list EXCEPT IN lt_list WHERE id = id ).

WRITE / lines( et_list ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it.skip("EXCEPT IN, filter miss", async () => {
    const code = `
TYPES: BEGIN OF ty,
         id    TYPE i,
         value TYPE c LENGTH 10,
       END OF ty.
DATA et_list TYPE SORTED TABLE OF ty WITH UNIQUE KEY id.
DATA lt_list TYPE SORTED TABLE OF ty WITH UNIQUE KEY id.

INSERT VALUE #( id = 1 ) INTO TABLE et_list.
INSERT VALUE #( id = 2 ) INTO TABLE lt_list.

et_list = FILTER #( et_list EXCEPT IN lt_list WHERE id = id ).

WRITE / lines( et_list ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("filter with CONV inference", async () => {
    const code = `
FORM run.
  TYPES: BEGIN OF ty,
           field TYPE c LENGTH 10,
         END OF ty.
  DATA original TYPE SORTED TABLE OF ty WITH NON-UNIQUE KEY field.
  INSERT VALUE #( ) INTO TABLE original.
  INSERT VALUE #( field = '1' ) INTO TABLE original.
  DATA(filtered) = FILTER #( original WHERE field <> CONV #( space ) ).
  WRITE / lines( filtered ).
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

});