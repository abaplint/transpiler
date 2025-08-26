import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running code structure - Eventing", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    EVENTS foo.
    METHODS raise.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD raise.
    RAISE EVENT foo.
  ENDMETHOD.
ENDCLASS.

CLASS handler DEFINITION.
  PUBLIC SECTION.
    METHODS bar FOR EVENT foo OF lcl.
ENDCLASS.
CLASS handler IMPLEMENTATION.
  METHOD bar.
    WRITE 'handled'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  DATA hnd TYPE REF TO handler.
  CREATE OBJECT ref.
  CREATE OBJECT hnd.
  SET HANDLER hnd->bar FOR ref.
  ref->raise( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("handled");
  });

  it("call event handler method", async () => {
    const code = `
INTERFACE lif.
  EVENTS foo EXPORTING VALUE(action) TYPE string.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS on_event FOR EVENT foo OF lif IMPORTING action.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD on_event.
    WRITE action.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  ref->on_event( 'sdf' ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("sdf");
  });

  it("basic deregistration", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    EVENTS foo.
    METHODS raise.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD raise.
    RAISE EVENT foo.
  ENDMETHOD.
ENDCLASS.

CLASS handler DEFINITION.
  PUBLIC SECTION.
    METHODS bar FOR EVENT foo OF lcl.
ENDCLASS.
CLASS handler IMPLEMENTATION.
  METHOD bar.
    WRITE 'handled'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  DATA hnd TYPE REF TO handler.
  CREATE OBJECT ref.
  CREATE OBJECT hnd.
  SET HANDLER hnd->bar FOR ref.
  ref->raise( ).
  SET HANDLER hnd->bar FOR ref ACTIVATION abap_false.
  ref->raise( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("handled");
  });

});