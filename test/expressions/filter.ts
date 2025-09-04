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

  it.only("basic", async () => {
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
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

// todo: EXCEPT IN

});