import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

// All expected values below were measured on SAP S/4HANA 2023 FPS03, ABAP 7.58.

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

describe("Builtin functions - find_any_of", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("first character of sub found", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_of( val = 'abcdef' sub = 'ec' ).
    WRITE lv.`, "2");
  });

  it("no character of sub found", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_of( val = 'abcdef' sub = 'xyz' ).
    WRITE lv.`, "-1");
  });

  it("empty string sub", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_of( val = \`abcdef\` sub = \`\` ).
    WRITE lv.`, "-1");
  });

  it("empty val", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_of( val = '' sub = 'a' ).
    WRITE lv.`, "-1");
  });

  it("blank is an ordinary character", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_of( val = \`a b\` sub = \` \` ).
    WRITE lv.`, "1");
  });

  it("search is case sensitive", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_of( val = 'ABCdef' sub = 'b' ).
    WRITE lv.`, "-1");
  });

  it("off skips the beginning", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_of( val = 'abcabc' sub = 'a' off = 1 ).
    WRITE lv.`, "3");
  });

  it("occ 1 is the default", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_of( val = 'abcabc' sub = 'ba' occ = 1 ).
    WRITE lv.`, "0");
  });

  it("occ counts occurrences of any character", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_of( val = 'abcabc' sub = 'ba' occ = 3 ).
    WRITE lv.`, "3");
  });

  it("occ beyond the last occurrence gives -1", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_of( val = 'abcabc' sub = 'ba' occ = 9 ).
    WRITE lv.`, "-1");
  });

  it("negative occ counts from the end", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_of( val = 'abcabc' sub = 'a' occ = -1 ).
    WRITE lv.`, "3");
  });

  it("negative occ counts occurrences, not positions", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_of( val = 'abcabc' sub = 'ba' occ = -2 ).
    WRITE lv.`, "3");
  });

  it("len limits the search", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_of( val = 'abcabc' sub = 'c' len = 4 ).
    WRITE lv.`, "2");
  });

  it("off and len together", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_of( val = 'abcabc' sub = 'c' off = 1 len = 2 ).
    WRITE lv.`, "2");
  });

  it("occ 0 throws CX_SY_STRG_PAR_VAL", async () => {
    await expectThrows(`
    DATA lv TYPE i.
    lv = find_any_of( val = 'abcdef' sub = 'a' occ = 0 ).
    WRITE lv.`, "CX_SY_STRG_PAR_VAL");
  });

  it("off out of bounds throws CX_SY_RANGE_OUT_OF_BOUNDS", async () => {
    await expectThrows(`
    DATA lv TYPE i.
    lv = find_any_of( val = 'abcdef' sub = 'a' off = 99 ).
    WRITE lv.`, "CX_SY_RANGE_OUT_OF_BOUNDS");
  });

  it("len out of bounds throws CX_SY_RANGE_OUT_OF_BOUNDS", async () => {
    await expectThrows(`
    DATA lv TYPE i.
    lv = find_any_of( val = 'abcdef' sub = 'a' len = 99 ).
    WRITE lv.`, "CX_SY_RANGE_OUT_OF_BOUNDS");
  });

});

describe("Builtin functions - find_any_not_of", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("first character not in sub", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_not_of( val = \`  abc\` sub = \` \` ).
    WRITE lv.`, "2");
  });

  it("trailing blanks do not change the first hit", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_not_of( val = \`  abc  \` sub = \` \` ).
    WRITE lv.`, "2");
  });

  it("negative occ finds the last character not in sub", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_not_of( val = \`  abc  \` sub = \` \` occ = -1 ).
    WRITE lv.`, "4");
  });

  it("every character excluded gives -1", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_not_of( val = \`aaa\` sub = \`a\` ).
    WRITE lv.`, "-1");
  });

  it("every character of val excluded gives -1", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_not_of( val = \`abcabc\` sub = \`abc\` ).
    WRITE lv.`, "-1");
  });

  it("empty string sub excludes nothing", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_not_of( val = \`abcdef\` sub = \`\` ).
    WRITE lv.`, "0");
  });

  it("blank character literal sub", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_not_of( val = 'abcdef' sub = '' ).
    WRITE lv.`, "0");
  });

  it("occ counts characters not in sub", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_not_of( val = 'aabbcc' sub = 'a' occ = 2 ).
    WRITE lv.`, "3");
  });

  it("off skips the beginning", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_not_of( val = 'aabbcc' sub = 'ab' off = 1 ).
    WRITE lv.`, "4");
  });

  it("len limits the search", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = find_any_not_of( val = 'aabbcc' sub = 'c' len = 4 ).
    WRITE lv.`, "0");
  });

  it("occ 0 throws CX_SY_STRG_PAR_VAL", async () => {
    await expectThrows(`
    DATA lv TYPE i.
    lv = find_any_not_of( val = 'abcdef' sub = 'a' occ = 0 ).
    WRITE lv.`, "CX_SY_STRG_PAR_VAL");
  });

});
