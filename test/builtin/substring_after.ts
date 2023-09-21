import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - substring_after", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("substring_after 01", async () => {
    const code = `
    DATA result TYPE string.
    result = substring_after( val = 'foo=bar' sub = '=' ).
    ASSERT result = 'bar'.
    result = substring_after( val = 'abc' sub = '=' ).
    ASSERT result = ''.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("substring_after 02", async () => {
    const code = `
DATA lv_classname TYPE c LENGTH 100.
DATA result TYPE string.
lv_classname = 'CLASS=FOO'.
result = substring_after( val = lv_classname sub = 'CLASS=' ).
ASSERT result = |FOO|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("substring_after, escape regex", async () => {
    const code = `
DATA val TYPE string.
val = substring_after( val = 'foo?bar' sub = '?' ).
WRITE val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar");
  });

  it("substring_after, pcre", async () => {
    const code = `
    DATA val TYPE string.
    val = substring_after( val = 'hello' pcre = 'hell' ).
    ASSERT val = 'o'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
