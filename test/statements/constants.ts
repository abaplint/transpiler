import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CONSTANTS", () => {

  beforeEach(async () => {
    abap = new ABAP(new MemoryConsole());
  });

  it("simple", async () => {
    const code = `
    CONSTANTS cval TYPE string VALUE 'ab'.
    WRITE cval.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ab");
  });

  it("concat", async () => {
    const code = `
    CONSTANTS cval TYPE string VALUE 'a' & 'b'.
    WRITE cval.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ab");
  });

});