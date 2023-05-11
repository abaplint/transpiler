// import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - INSTANCE OF", () => {

  beforeEach(async () => {
    abap = new ABAP(new MemoryConsole());
  });

  it("test", async () => {
    const code = `
CLASS foo DEFINITION.
ENDCLASS.
CLASS foo IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lcl1 TYPE REF TO foo.
  CREATE OBJECT lcl1.
  ASSERT lcl1 IS INSTANCE OF foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("negative", async () => {
    const code = `
CLASS foo DEFINITION.
  PUBLIC SECTION.
    DATA foo TYPE i.
ENDCLASS.
CLASS foo IMPLEMENTATION.
ENDCLASS.

CLASS bar DEFINITION INHERITING FROM foo.
  PUBLIC SECTION.
    DATA bar TYPE i.
ENDCLASS.
CLASS bar IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lcl1 TYPE REF TO foo.
  CREATE OBJECT lcl1.
  ASSERT NOT lcl1 IS INSTANCE OF bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});