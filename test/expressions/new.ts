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

  it("basic", async () => {
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("with constructorparameter", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING bar TYPE i.
    METHODS foo RETURNING VALUE(val) TYPE i.
  PRIVATE SECTION.
    DATA mv TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD constructor.
    mv = bar.
  ENDMETHOD.

  METHOD foo.
    val = mv.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  ref = NEW #( bar = 2 ).
  WRITE / ref->foo( ).`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});