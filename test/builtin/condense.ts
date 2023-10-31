import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - condense", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("condense", async () => {
    const code = `
      DATA foo TYPE string.
      foo = '12  3 '.
      foo = condense( foo ).
      WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12 3");
  });

  it("condense integer", async () => {
    const code = `
      DATA foo TYPE string.
      data bar type i.
      bar = 2.
      foo = bar.
      foo = condense( foo ).
      WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("condense() test", async () => {
    const code = `
    DATA lv_bar TYPE string.
    lv_bar = condense( | foo\\n bar\\n| ).
    ASSERT lv_bar = |foo\\n bar\\n|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("condense(), from to", async () => {
    const code = `
    DATA res TYPE string.
    res = condense( val = |vcvix rhn| from = |  ,.:;|  to = || ).
    ASSERT res = |vcvixrhn|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("condense(), abc", async () => {
    const code = `
    DATA str TYPE string.
    str = condense( val = |abccaab| del = |ab| ).
    ASSERT str = 'cc'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
