import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - INITIAL", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("double NOT", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS advance RETURNING VALUE(val) TYPE abap_bool.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD advance.
    val = abap_true.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  IF NOT ref->advance( ) IS NOT INITIAL.
    WRITE 'err'.
  ELSE.
    WRITE 'ok'.
  ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ok");
  });

  it("hashed table initial", async () => {
    const code = `
DATA tab TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.
ASSERT tab IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});