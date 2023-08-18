import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CASE TYPE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("CASE TYPE, basic", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA foo TYPE string.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lo_artefact TYPE REF TO object.
  DATA lo_lcl TYPE REF TO lcl.
  CREATE OBJECT lo_artefact TYPE lcl.
  CASE TYPE OF lo_artefact.
    WHEN TYPE lcl INTO lo_lcl.
      WRITE lo_lcl->foo.
      WRITE 'world'.
    WHEN OTHERS.
      WRITE 'hello'.
  ENDCASE.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("world");
  });

});