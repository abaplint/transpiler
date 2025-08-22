import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - RAISE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("test no syntax errors", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS foo EXCEPTIONS bar.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    RAISE bar.
  ENDMETHOD.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("basic, classic exception in method call", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS method EXCEPTIONS hello_world.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD method.
    RAISE hello_world.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>method(
    EXCEPTIONS hello_world = 5
    OTHERS = 7 ).
  WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("no exception, should reset subrc", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS method EXCEPTIONS hello_world.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD method.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  sy-subrc = 2.
  lcl=>method(
    EXCEPTIONS hello_world = 5
    OTHERS = 7 ).
  WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it.skip("RAISE EXCEPTION NEW", async () => {
    const code = `
CLASS lcx DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.
CLASS lcx IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  RAISE EXCEPTION NEW lcx( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});