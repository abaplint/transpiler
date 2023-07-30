import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
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

});
