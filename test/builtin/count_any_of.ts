import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - count_any_of", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("test 1", async () => {
    const code = `
    DATA lv TYPE i.
    lv = count_any_of( val = 'hello gg, world'  sub = 'DG' ).
    WRITE lv.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("test 2", async () => {
    const code = `
    DATA lv TYPE i.
    lv = count_any_of( val = 'AABBEEDGGEED'  sub = 'DG' ).
    WRITE lv.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("empty", async () => {
    const code = `
    DATA lv TYPE i.
    lv = count_any_of( val = 'AA'  sub = '' ).
    WRITE lv.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

});
