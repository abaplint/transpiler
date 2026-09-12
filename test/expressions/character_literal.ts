import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running expressions - Character literals", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("escaped quote inside a literal", async () => {
    const code = `
      DATA lv TYPE c LENGTH 5.
      lv = 'a''b'.
      WRITE strlen( lv ).
      WRITE lv.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3a'b  ");
  });

  it("literal of two quotes has length two", async () => {
    const code = `
      DATA lv TYPE string.
      lv = ''''''.
      WRITE strlen( lv ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("REPLACE with a two-quote pattern", async () => {
    const code = `
      DATA lv TYPE string.
      lv = \`x''y\`.
      REPLACE ALL OCCURRENCES OF '''''' IN lv WITH ''''.
      WRITE lv.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("x'y");
  });

});
