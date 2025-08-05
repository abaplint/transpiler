import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - RAISE EVENT", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic, no handlers", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    EVENTS foo.
    METHODS method1.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD method1.
    RAISE EVENT foo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  ref->method1( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("basic handler", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    EVENTS foo.
    METHODS method1.
    METHODS handler FOR EVENT foo OF lcl.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD method1.
    SET HANDLER handler FOR me.
    RAISE EVENT foo.
  ENDMETHOD.

  METHOD handler.
    WRITE / 'handled'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  ref->method1( ).`;
    const js = await run(code);
//    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("handled");
  });

  it("basic handler, ALL INSTANCES", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    EVENTS foo.
    METHODS method1.
    METHODS handler FOR EVENT foo OF lcl.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD method1.
    SET HANDLER handler FOR ALL INSTANCES.
    RAISE EVENT foo.
  ENDMETHOD.

  METHOD handler.
    WRITE / 'handled'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  ref->method1( ).`;
    const js = await run(code);
//    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("handled");
  });

  it("basic handler, static event", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-EVENTS foo.
    METHODS method1.
    METHODS handler FOR EVENT foo OF lcl.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD method1.
    SET HANDLER handler.
    RAISE EVENT foo.
  ENDMETHOD.

  METHOD handler.
    WRITE / 'handled'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  ref->method1( ).`;
    const js = await run(code);
//    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("handled");
  });

  it("basic handler, with parameter", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    EVENTS foo EXPORTING VALUE(val) TYPE i.
    METHODS method1.
    METHODS handler FOR EVENT foo OF lcl IMPORTING val.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD method1.
    SET HANDLER handler FOR ALL INSTANCES.
    RAISE EVENT foo EXPORTING val = 2.
  ENDMETHOD.

  METHOD handler.
    WRITE / val.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  ref->method1( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("event defined in super class", async () => {
    const code = `
CLASS dup DEFINITION.
  PUBLIC SECTION.
    EVENTS foo.
ENDCLASS.

CLASS dup IMPLEMENTATION.
ENDCLASS.

CLASS lcl DEFINITION INHERITING FROM dup.
  PUBLIC SECTION.
    METHODS method1.
    METHODS handler FOR EVENT foo OF lcl.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD method1.
    SET HANDLER handler FOR ALL INSTANCES.
    RAISE EVENT foo.
  ENDMETHOD.

  METHOD handler.
    WRITE / 'handled'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  ref->method1( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("handled");
  });

  it("event defined in interface", async () => {
    const code = `
INTERFACE lif.
  EVENTS foo.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS method1.
    INTERFACES lif.
    METHODS handler FOR EVENT foo OF lif.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD method1.
    SET HANDLER handler FOR ALL INSTANCES.
    RAISE EVENT lif~foo.
  ENDMETHOD.

  METHOD handler.
    WRITE / 'handled'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  ref->method1( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("handled");
  });

  it("decoupled raiser and handler", async () => {
    const code = `
INTERFACE lif.
  EVENTS bar.
ENDINTERFACE.

CLASS handler DEFINITION.
  PUBLIC SECTION.
    METHODS register.
    METHODS handler FOR EVENT bar OF lif.
ENDCLASS.

CLASS handler IMPLEMENTATION.
  METHOD handler.
    WRITE / 'handled'.
  ENDMETHOD.

  METHOD register.
    SET HANDLER handler FOR ALL INSTANCES.
  ENDMETHOD.
ENDCLASS.

CLASS raiser DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
    METHODS raise.
ENDCLASS.

CLASS raiser IMPLEMENTATION.
  METHOD raise.
    RAISE EVENT lif~bar.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA foo TYPE REF TO handler.
  CREATE OBJECT foo.
  foo->register( ).

  DATA ref TYPE REF TO raiser.
  CREATE OBJECT ref.
  ref->raise( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("handled");
  });

  it("all static", async () => {
    const code = `
CLASS stat DEFINITION.
  PUBLIC SECTION.
    CLASS-EVENTS foobar.
    CLASS-METHODS raise.
    CLASS-METHODS handler FOR EVENT foobar OF stat.
ENDCLASS.

CLASS stat IMPLEMENTATION.
  METHOD raise.
    SET HANDLER handler.
    RAISE EVENT foobar.
  ENDMETHOD.

  METHOD handler.
    WRITE / 'handled'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  stat=>raise( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("handled");
  });

  it.skip("all static, via interface", async () => {
    const code = `
INTERFACE lif.
  CLASS-EVENTS foobar.
ENDINTERFACE.

CLASS stat DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
    CLASS-METHODS raise.
    CLASS-METHODS handler FOR EVENT foobar OF lif.
ENDCLASS.

CLASS stat IMPLEMENTATION.
  METHOD raise.
    SET HANDLER handler.
    RAISE EVENT lif~foobar.
  ENDMETHOD.

  METHOD handler.
    WRITE / 'handled'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  stat=>raise( ).`;
    const js = await run(code);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("handled");
  });

  it("check sender is set", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    EVENTS foo.
    METHODS method1.
    METHODS handler FOR EVENT foo OF lcl
      IMPORTING sender.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD method1.
    SET HANDLER handler FOR me.
    RAISE EVENT foo.
  ENDMETHOD.

  METHOD handler.
    ASSERT sender IS NOT INITIAL.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  ref->method1( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.only("output private variable in handler (this)", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    EVENTS foo.
    METHODS method1.
    METHODS handler FOR EVENT foo OF lcl.
  PRIVATE SECTION.
    DATA priv TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD method1.
    priv = 2.
    SET HANDLER handler FOR me.
    RAISE EVENT foo.
  ENDMETHOD.

  METHOD handler.
    WRITE priv.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  ref->method1( ).`;
    const js = await run(code);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});