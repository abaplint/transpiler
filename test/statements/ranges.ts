import {ABAP, MemoryConsole} from "../../packages/runtime/src";
// import {expect} from "chai";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - RANGES", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("BASIC", async () => {
    const code = `
DATA bar TYPE i.
RANGES foo FOR bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});