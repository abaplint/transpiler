import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - insert", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("simple", async () => {
    const code = `WRITE insert( val = |val| sub = |sub| ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`subval`);
  });

  it("test 2", async () => {
    const code = `WRITE insert( val = |val| sub = |sub| off = 1 ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`vsubal`);
  });

  it("off from nmin, two digits", async () => {
    const code = `WRITE insert( val = |abcdefghijklmnop| sub = |XX| off = nmin( val1 = 12 val2 = 30 ) ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`abcdefghijklXXmnop`);
  });

});
