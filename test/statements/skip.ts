import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar_skip.prog.abap", contents}]);
}

describe("Running statements - SKIP", () => {

  beforeEach(() => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("adds one blank line by default", async () => {
    const js = await run("SKIP. WRITE 'after'.");
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("\nafter");
  });

  it("adds the requested number of blank lines", async () => {
    const js = await run("SKIP 3. WRITE 'after'.");
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("\n\n\nafter");
  });

  it("positions output at an absolute line", async () => {
    const js = await run("WRITE 'line one'. SKIP TO LINE 4. WRITE 'line four'.");
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("line one\n\n\nline four");
  });

  it("does not add output for zero lines", async () => {
    const js = await run("SKIP 0. WRITE 'first line'.");
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("first line");
  });

});
