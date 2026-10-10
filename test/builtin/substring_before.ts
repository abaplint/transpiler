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

describe("Builtin functions - substring_before", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("substring_before 01", async () => {
    const code = `
    DATA result TYPE string.
    result = substring_before( val = 'abc=CP' regex = '=*CP$' ).
    ASSERT result = 'abc'.
    result = substring_before( val = 'abc' regex = '=*CP$' ).
    ASSERT result = ''.
    result = substring_before( val = 'sdf===CP' regex = '(=+CP)?$' ).
    ASSERT result = 'sdf'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("substring_before 02", async () => {
    const code = `
    DATA res TYPE string.
    DATA input TYPE string.
    input = 'foo=bar'.
    res = substring_before( val = input sub = '=' ).
    ASSERT res = 'foo'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("substring_before 03", async () => {
    const code = `
  DATA res TYPE string.
  res = substring_before( val   = 'ZSOME_PROG_ENDING_WITH_CP'
                          regex = '(=+CP)?$' ).
  WRITE res.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal( `ZSOME_PROG_ENDING_WITH_CP` );
  });

  it("substring_before 04", async () => {
    const code = `
    DATA res TYPE string.
    DATA iv_program_name TYPE c LENGTH 40.
    iv_program_name = 'HELLO=CP'.
    res = substring_before(
      val   = iv_program_name
      regex = '(=+CP)?$' ).
    WRITE res.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal( `HELLO` );
  });

  it("substring_before, field symbol to character", async () => {
    const code = `
    DATA res TYPE string.
    DATA iv_program_name TYPE c LENGTH 40.
    FIELD-SYMBOLS <iv_program_name> TYPE c.
    iv_program_name = 'HELLO=CP'.
    ASSIGN iv_program_name TO <iv_program_name>.
    res = substring_before(
      val   = <iv_program_name>
      regex = '(=+CP)?$' ).
    WRITE res.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal( `HELLO` );
  });

  it("substring_before, escape regex", async () => {
    const code = `
DATA val TYPE string.
val = substring_before( val = 'foo?bar' sub = '?' ).
WRITE val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("substring_before, newline", async () => {
    const code = `
data lv_text type string.
lv_text = |a\\nb;c|.
ASSERT substring_before( val = lv_text
                         sub = ';' ) = |a\\nb|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("substring_before, the example of the keyword documentation", async () => {
    await expectValue(`WRITE substring_before( val = 'ABCDEFGH' sub = 'CD' ).`, "AB");
  });

  it("substring_before, occ", async () => {
    await expectValue(`WRITE substring_before( val = 'aa1bb2aa3bb4' sub = 'aa' occ = 2 ).`, "aa1bb2");
    abap.console.clear();
    await expectValue(`WRITE substring_before( val = 'a-b-c' sub = '-' occ = -1 ).`, "a-b");
  });

  it("substring_before, the len characters in front of the occurrence", async () => {
    await expectValue(`WRITE substring_before( val = 'ABCDEFGH' sub = 'CD' len = 1 ).`, "B");
  });

  it("substring_before, len past the start", async () => {
    await expectThrows(`WRITE substring_before( val = 'ABCDEFGH' sub = 'CD' len = 3 ).`, "CX_SY_RANGE_OUT_OF_BOUNDS");
  });

  it("substring_before, case", async () => {
    await expectValue(`WRITE substring_before( val = 'abCDef' sub = 'cd' case = abap_false ).`, "ab");
    abap.console.clear();
    await expectValue(`WRITE substring_before( val = 'abCDef' sub = 'cd' ).`, "");
  });

  it("substring_before, regex with a negative occ", async () => {
    await expectValue(`WRITE substring_before( val = 'a1b2c3' regex = '[0-9]' occ = -1 ).`, "a1b2c");
  });

  it("substring_before, occ = 0", async () => {
    await expectThrows(`WRITE substring_before( val = 'a-b-c' sub = '-' occ = 0 ).`, "CX_SY_STRG_PAR_VAL");
  });

});
