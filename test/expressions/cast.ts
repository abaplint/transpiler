// import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_cast.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - CAST", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
INTERFACE lif.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  DATA int TYPE REF TO lif.
  CREATE OBJECT int TYPE lcl.
  ref = CAST #( int ).`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("call the method", async () => {
    const code = `
INTERFACE lif.
  METHODS call.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD lif~call.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  DATA int TYPE REF TO lif.
  CREATE OBJECT int TYPE lcl.
  ref = CAST #( int ).
  ref->lif~call( ).`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});