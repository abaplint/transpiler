// import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - substring", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("substring 01", async () => {
    const code = `
    DATA path TYPE string VALUE '/'.
    DATA result TYPE string.
    result = substring( val = path off = strlen( path ) - 1 ).
    ASSERT result = '/'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("basic substring", async () => {
    const code = `ASSERT substring( val = |abc| off = 1 len = 1 ) = |b|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("substring, only val and len supplied", async () => {
    const code = `ASSERT substring( val = |abc| len = 1 ) = |a|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  // nmin( ) and nmax( ) return a float, which used to reach substring( ) as the
  // string "1,2000000000000000E+01" and be read back as 1
  it("substring, len from nmin, two digits", async () => {
    const code = `ASSERT substring( val = |abcdefghijklmnop| len = nmin( val1 = 12 val2 = 30 ) ) = |abcdefghijkl|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("substring, off from nmin, two digits", async () => {
    const code = `ASSERT substring( val = |abcdefghijklmnop| off = nmin( val1 = 12 val2 = 30 ) ) = |mnop|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("substring, off and len from nmin", async () => {
    const code = `ASSERT substring( val = |abcdefghijklmnop| off = nmin( val1 = 10 val2 = 30 ) len = nmin( val1 = 4 val2 = 30 ) ) = |klmn|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("substring, len from nmax, two digits", async () => {
    const code = `ASSERT substring( val = |abcdefghijklmnop| len = nmax( val1 = 12 val2 = 3 ) ) = |abcdefghijkl|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("substring, len from nmin, three digits", async () => {
    const code = `
    DATA lv TYPE string.
    lv = repeat( val = 'a' occ = 150 ).
    ASSERT strlen( substring( val = lv len = nmin( val1 = 100 val2 = 140 ) ) ) = 100.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
