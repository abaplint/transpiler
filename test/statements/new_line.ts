import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - NEW-LINE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("the next output starts a new line", async () => {
    const code = `
WRITE 'a'.
NEW-LINE.
WRITE 'b'.`;
    const js = await run(code);
    await new AsyncFunction("abap", js)(abap);
    expect(abap.console.get()).to.equal("a\nb");
  });

  it("a second NEW-LINE, or a / after it, starts no further line", async () => {
    const code = `
WRITE 'a'.
NEW-LINE.
NEW-LINE NO-SCROLLING.
WRITE 'b'.
NEW-LINE.
WRITE / 'c'.`;
    const js = await run(code);
    await new AsyncFunction("abap", js)(abap);
    expect(abap.console.get()).to.equal("a\nb\nc");
  });

  it("at the start of the list, and at the end without output", async () => {
    const code = `
NEW-LINE.
WRITE 'a'.
NEW-LINE.`;
    const js = await run(code);
    await new AsyncFunction("abap", js)(abap);
    expect(abap.console.get()).to.equal("a");
  });

});
