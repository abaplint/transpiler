import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - RETURN", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
CLASS LCL DEFINITION.
  PUBLIC SECTION.
    class-METHODS MAIN RETURNING VALUE(RESULT) TYPE I.
ENDCLASS.

CLASS LCL IMPLEMENTATION.
  METHOD MAIN.
    return 2 + 2.
  ENDMETHOD.
ENDCLASS.

start-of-selection.
  data result type i.
  RESULT = LCL=>MAIN( ).
  WRITE / RESULT.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("inside WHILE in second CASE branch", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING state TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    DATA cond TYPE abap_bool VALUE abap_true.
    CASE state.
      WHEN 1.
        WHILE cond = abap_true.
        ENDWHILE.
      WHEN 2.
        WHILE cond = abap_true.
          RETURN.
        ENDWHILE.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( state = 2 ).
  WRITE 'after'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("after");
  });

});