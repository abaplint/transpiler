import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CALL", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("return value", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo.
    CLASS-METHODS bar RETURNING VALUE(field) TYPE string.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    DATA field TYPE string.
    CALL METHOD bar RECEIVING field = field.
    WRITE field.
  ENDMETHOD.

  METHOD bar.
    field = 'helloworld'.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl=>foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("helloworld");
  });

  it("dynamic method call", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo.
    CLASS-METHODS run.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD foo.
    WRITE 'hello'.
  ENDMETHOD.

  METHOD run.
    DATA lv_bar TYPE string.
    lv_bar = 'FOO'.
    CALL METHOD lcl=>(lv_bar).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it.only("dynamic method call, method in this", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS foo.
    METHODS bar.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    DATA lv_str TYPE string.
    lv_str = 'BAR'.
    CALL METHOD (lv_str).
  ENDMETHOD.

  METHOD bar.
    WRITE 'hello world'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.
  lo->foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

});