import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - count", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic count()", async () => {
    const code = `
      DATA lv_count TYPE i.
      lv_count = count( val = 'password' sub = 's' ).
      WRITE lv_count.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("count(), escape regex", async () => {
    const code = `
  DATA lv TYPE i.
  lv = count( val = 'ab*foo*cd' sub = '*' ).
  WRITE lv.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("count(), case false", async () => {
    const code = `DATA cnt TYPE i.
  cnt = count( val = 'aabb' case = abap_false regex = \`[AEIOULNRST]\` ).
  WRITE cnt.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("offset and length", async () => {
    const code = `
    ASSERT count( val = 'hello' sub = 'l' off = 2 len = 1 ) = 1.
    ASSERT count( val = 'hello' sub = 'l' off = 2 len = 2 ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("offset", async () => {
    const code = `
    DATA c TYPE i.
    c = count( val = 'hello' sub = 'l' off = 2 ).
    WRITE / c.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("length", async () => {
    const code = `
    DATA c TYPE i.
    c = count( val = 'hello' sub = 'l' len = 2 ).
    WRITE / c.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("pcre", async () => {
    const code = `
    DATA val TYPE i.
    val = count( val = 'hello' pcre = 'l' ).
    ASSERT val = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
