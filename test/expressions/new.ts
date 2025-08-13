import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_new.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - NEW", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it.only("basic", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS foo RETURNING VALUE(val) TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    val = 2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  ref = NEW #( ).
  WRITE / ref->foo( ).`;
    const js = await run(code, true);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});