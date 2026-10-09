import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}


async function expectValue(code: string, expected: string) {
  const js = await run(code);
  const f = new AsyncFunction("abap", js);
  await f(abap);
  expect(abap.console.get()).to.equal(expected);
}

async function expectThrows(code: string, exception: string) {
  const js = await run(code);
  const f = new AsyncFunction("abap", js);
  try {
    await f(abap);
    expect.fail("expected " + exception);
  } catch (e) {
    expect(e.toString()).to.contain(exception);
  }
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

  it("substring_after, field symbol to character", async () => {
    const code = `
DATA lv_classname TYPE c LENGTH 100.
FIELD-SYMBOLS <lv_classname> TYPE c.
DATA result TYPE string.
lv_classname = 'CLASS=FOO'.
ASSIGN lv_classname TO <lv_classname>.
result = substring_after( val = <lv_classname> sub = 'CLASS=' ).
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

  it("substring_after, newline", async () => {
    const code = `
data lv_text type string.
lv_text = |a;b\\nc|.
ASSERT substring_after( val = lv_text
                        sub = ';' ) = |b\\nc|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("substring_after, the example of the keyword documentation", async () => {
    await expectValue(`WRITE substring_after( val = 'ABCDEFGH' sub = 'CD' ).`, "EFGH");
  });

  it("substring_after, occ counts from the left", async () => {
    await expectValue(`WRITE substring_after( val = 'aa1bb2aa3bb4' sub = 'aa' occ = 2 ).`, "3bb4");
  });

  it("substring_after, a negative occ counts from the right", async () => {
    await expectValue(`WRITE substring_after( val = 'a-b-c' sub = '-' occ = -1 ).`, "c");
    abap.console.clear();
    await expectValue(`WRITE substring_after( val = 'a-b-c' sub = '-' occ = -2 ).`, "b-c");
  });

  it("substring_after, occ past the last occurrence", async () => {
    await expectValue(`WRITE substring_after( val = 'a-b-c' sub = '-' occ = 3 ).`, "");
  });

  it("substring_after, len characters after the occurrence", async () => {
    await expectValue(`WRITE substring_after( val = 'ABCDEFGH' sub = 'CD' len = 2 ).`, "EF");
    abap.console.clear();
    await expectValue(`WRITE substring_after( val = 'aa1bb2aa3bb4' sub = 'aa' occ = 2 len = 4 ).`, "3bb4");
  });

  it("substring_after, len past the end", async () => {
    await expectThrows(`WRITE substring_after( val = 'ABCDEFGH' sub = 'CD' len = 5 ).`, "CX_SY_RANGE_OUT_OF_BOUNDS");
  });

  it("substring_after, len when nothing is found", async () => {
    await expectValue(`WRITE substring_after( val = 'ABCDEFGH' sub = 'XY' len = 5 ).`, "");
  });

  it("substring_after, case", async () => {
    await expectValue(`WRITE substring_after( val = 'abCDef' sub = 'cd' case = abap_false ).`, "ef");
    abap.console.clear();
    await expectValue(`WRITE substring_after( val = 'abCDef' sub = 'cd' ).`, "");
    abap.console.clear();
    await expectValue(`WRITE substring_after( val = 'abCDef' sub = 'cd' case = abap_true ).`, "");
  });

  it("substring_after, regex with occ and case", async () => {
    await expectValue(`WRITE substring_after( val = 'a1b2c3' regex = '[0-9]' occ = 2 ).`, "c3");
    abap.console.clear();
    await expectValue(`WRITE substring_after( val = 'a1b2c3' regex = '[0-9]' occ = -2 ).`, "c3");
    abap.console.clear();
    await expectValue(`WRITE substring_after( val = 'xAyaz' regex = 'a' occ = 1 case = abap_false ).`, "yaz");
  });

  it("substring_after, occ = 0", async () => {
    await expectThrows(`WRITE substring_after( val = 'a-b-c' sub = '-' occ = 0 ).`, "CX_SY_STRG_PAR_VAL");
  });

  it("substring_after, an empty sub", async () => {
    await expectThrows(`
DATA lv_sub TYPE string.
WRITE substring_after( val = 'a-b-c' sub = lv_sub ).`, "CX_SY_STRG_PAR_VAL");
  });

});
