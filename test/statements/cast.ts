import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

// Cast is not directly a statement in ABAP, but separate in the transpiler/runtime
describe("Running statements - Cast", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("id", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    WRITE 'ok'.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  DATA foo TYPE REF TO lcl.
  DATA bar TYPE REF TO lcl.
  CREATE OBJECT foo.
  bar ?= foo.
  bar->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ok");
  });

  it("ok", async () => {
    const code = `
CLASS sup DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS sup IMPLEMENTATION.
  METHOD run.
    WRITE 'ok'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl DEFINITION INHERITING FROM sup.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA foo TYPE REF TO lcl.
  DATA bar TYPE REF TO sup.
  CREATE OBJECT foo.
  bar ?= foo.
  bar->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ok");
  });

  it("ok, static types does not match", async () => {
    const code = `
CLASS sup DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS sup IMPLEMENTATION.
  METHOD run.
    WRITE 'ok'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl DEFINITION INHERITING FROM sup.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA foo TYPE REF TO object.
  DATA bar TYPE REF TO sup.
  CREATE OBJECT foo TYPE lcl.
  bar ?= foo.
  bar->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ok");
  });

  it("initial, ok", async () => {
    const code = `
CLASS lcl DEFINITION.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA foo TYPE REF TO object.
  DATA bar TYPE REF TO lcl.
  bar ?= foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("ok, implements interface, local", async () => {
    const code = `
INTERFACE lif.
  DATA moo TYPE i.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  DATA li TYPE REF TO lif.
  CREATE OBJECT lo.
  li ?= lo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("ok, not bound", async () => {
    const code = `
INTERFACE lif1.
ENDINTERFACE.

INTERFACE lif2.
ENDINTERFACE.

DATA li_node TYPE REF TO lif1.
DATA li_val TYPE REF TO lif2.
li_node ?= li_val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("ok, field symbol", async () => {
    const code = `
CLASS lcl DEFINITION.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA foo TYPE REF TO lcl.
  FIELD-SYMBOLS <fs> TYPE REF TO lcl.
  CREATE OBJECT foo.
  ASSIGN foo TO <fs>.
  <fs> ?= foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

// unit tests throwing cx_sy_move_cast_error not part of this file

});