import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - OVERLAY", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("basic", async () => {
    const code = `
    DATA foo TYPE c LENGTH 3.
    OVERLAY foo WITH sy-abcde.
    WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ABC");
  });

  it("structure", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field1 TYPE c LENGTH 1,
         field2 TYPE c LENGTH 1,
       END OF ty.
DATA ls_parts TYPE ty.
OVERLAY ls_parts WITH 'XXXXXXXXXXXXXXXXXXXXXX'.
WRITE ls_parts.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("XX");
  });

});