import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - ADD", () => {

  beforeEach(async () => {
    abap = new ABAP(new MemoryConsole());
  });

  it("simple", async () => {
    const code = `
      data foo type i.
      add 2 to foo.
      write foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});