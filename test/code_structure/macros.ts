import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running code structure - Macros", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

// this is tricky, the foo1 and foo2 method calls will have identical virtual positions
  it("tricky", async () => {
    const code = `
DEFINE _macro1.
  WRITE 'A'.
  foo1( ).
end-of-definition.

DEFINE _macro2.
  _macro1.
  foo2( ).
end-of-definition.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
    CLASS-METHODS foo1.
    CLASS-METHODS foo2.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    _macro2.
  ENDMETHOD.
  METHOD foo1.
    WRITE '1'.
  ENDMETHOD.
  METHOD foo2.
    WRITE '2'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("A12");
  });

});