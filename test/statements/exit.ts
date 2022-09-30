import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - EXIT", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it.skip("EXIT outside of loop", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo RETURNING VALUE(ret) TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    ret = 2.
    EXIT.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  WRITE lcl=>foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});