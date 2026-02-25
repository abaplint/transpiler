import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe.only("Running code structure - List Events", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("simple start of selection", async () => {
    const code = `
REPORT zfoobar.

START-OF-SELECTION.
  WRITE 'hello world'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello world");
  });

  it("start + end", async () => {
    const code = `
REPORT zfoobar.

START-OF-SELECTION.
  WRITE / 'hello'.

END-OF-SELECTION.
  WRITE / 'world'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello\nworld");
  });

  it("same the other way around, start + end", async () => {
    const code = `
REPORT zfoobar.

END-OF-SELECTION.
  WRITE / 'world'.

START-OF-SELECTION.
  WRITE / 'hello'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello\nworld");
  });

});