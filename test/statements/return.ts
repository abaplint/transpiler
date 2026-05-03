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

  it.only("basic", async () => {
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

});