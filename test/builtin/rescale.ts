import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - rescale", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("dec, rounds half up", async () => {
    const code = `
    DATA lv TYPE decfloat34.
    lv = rescale( val = CONV decfloat34( '1.235' ) dec = 2 ).
    ASSERT lv = CONV decfloat34( '1.24' ).
    lv = rescale( val = CONV decfloat34( '-1.235' ) dec = 2 ).
    ASSERT lv = CONV decfloat34( '-1.24' ).
    WRITE 'OK'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("OK");
  });

  it("dec negative", async () => {
    const code = `
    DATA lv TYPE decfloat34.
    lv = rescale( val = CONV decfloat34( '125' ) dec = -1 ).
    ASSERT lv = CONV decfloat34( '130' ).
    WRITE 'OK'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("OK");
  });

  it("prec", async () => {
    const code = `
    DATA lv TYPE decfloat34.
    lv = rescale( val = CONV decfloat34( '1.235' ) prec = 2 ).
    ASSERT lv = CONV decfloat34( '1.2' ).
    WRITE 'OK'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("OK");
  });

  it("mode, half even", async () => {
    const code = `
    CONSTANTS half_even TYPE i VALUE 3.
    DATA lv TYPE decfloat34.
    lv = rescale( val = CONV decfloat34( '2.665' ) dec = 2 mode = half_even ).
    ASSERT lv = CONV decfloat34( '2.66' ).
    lv = rescale( val = CONV decfloat34( '2.675' ) dec = 2 mode = half_even ).
    ASSERT lv = CONV decfloat34( '2.68' ).
    WRITE 'OK'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("OK");
  });

  it("a scale increase keeps the value", async () => {
    const code = `
    DATA lv TYPE decfloat34.
    lv = rescale( val = CONV decfloat34( '1.2' ) dec = 3 ).
    ASSERT lv = CONV decfloat34( '1.2' ).
    WRITE 'OK'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("OK");
  });

});
