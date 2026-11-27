// import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - translate", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("translate() 1", async () => {
    const code = `
DATA foo TYPE string.
foo = translate( val = |foo| from = |o| to = |u| ).
ASSERT foo = 'fuu'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("translate() 2", async () => {
    const code = `
DATA foo TYPE string.
foo = translate( val = |foo| from = |o| to = || ).
ASSERT foo = 'f'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("translate() 3", async () => {
    const code = `
DATA foo TYPE string.
foo = translate( val = |foo| from = |fo| to = |ba| ).
ASSERT foo = 'baa'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("translate(), empty", async () => {
    const code = `
DATA foo TYPE string.
foo = translate( val = |foo| from = || to = || ).
ASSERT foo = 'foo'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("translate(), should single step", async () => {
    const code = `
DATA foo TYPE string.
foo = translate( val = |a| from = |ab| to = |bc| ).
ASSERT foo = 'b'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("translate(), should single step, 2", async () => {
    const code = `
DATA foo TYPE string.
foo = translate( val = |aa| from = |ab| to = |bc| ).
ASSERT foo = 'bb'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("translate(), field symbol to character", async () => {
    const code = `
DATA foo TYPE string.
DATA lv_to TYPE c LENGTH 4.
FIELD-SYMBOLS <lv_to> TYPE c.
lv_to = |u|.
ASSIGN lv_to TO <lv_to>.
foo = translate( val = |foo| from = |o| to = <lv_to> ).
ASSERT foo = 'fuu'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("translate(), FROM is characters, not a regex class", async () => {
    const code = `
DATA foo TYPE string.
foo = translate( val = 'a]b[c' from = ']' to = 'x' ).
ASSERT foo = 'axb[c'.
foo = translate( val = 'a]b[c' from = '[' to = 'x' ).
ASSERT foo = 'a]bxc'.
foo = translate( val = 'a-b' from = '-' to = 'x' ).
ASSERT foo = 'axb'.
foo = translate( val = 'a\\b' from = '\\' to = 'x' ).
ASSERT foo = 'axb'.
foo = translate( val = 'a^b' from = '^' to = 'x' ).
ASSERT foo = 'axb'.
foo = translate( val = 'a]b[c-d\\e^' from = '][-\\^' to = '12345' ).
ASSERT foo = 'a1b2c3d4e5'.
foo = translate( val = 'a-z' from = 'a-z' to = '' ).
ASSERT foo = ''.
foo = translate( val = 'x[]"y' from = '[]"' to = '' ).
ASSERT foo = 'xy'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
