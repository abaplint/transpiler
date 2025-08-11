import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_ref.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - REF", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
DATA foo TYPE i.
DATA ref TYPE REF TO i.
ref = REF #( foo ).
WRITE ref->*.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it.only("generic data", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS bar IMPORTING data TYPE data.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD bar.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA int TYPE i.
  lcl=>bar( REF #( int ) ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});