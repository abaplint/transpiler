// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - RAISE", () => {

  beforeEach(async () => {
    abap = new ABAP();
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

});