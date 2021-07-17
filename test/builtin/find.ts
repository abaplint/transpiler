import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - find", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("find 01", async () => {
    const code = `
    DATA str TYPE string.
    DATA off TYPE i.
    str = 'foobar'.
    off = find( val = str sub = 'oo' off = 0 ).
    WRITE off.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("find 02", async () => {
    const code = `
    DATA str TYPE string.
    DATA off TYPE i.
    str = 'foobar'.
    off = find( val = str sub = 'oo' off = 3 ).
    WRITE off.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("-1");
  });

  it("find 03", async () => {
    const code = `
DATA lv_end TYPE i.
lv_end = find( val = 'aa' regex = |aa| case = abap_false ).
WRITE lv_end.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("find 04, case", async () => {
    const code = `
DATA lv_end TYPE i.
lv_end = find( val = 'aa' regex = |AA| case = abap_false ).
WRITE lv_end.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("find 05, via regex, not found", async () => {
    const code = `
DATA lv_end TYPE i.
lv_end = find( val = 'aa' regex = |bb| case = abap_false ).
WRITE lv_end.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("-1");
  });

});
