import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - abs", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Builtin numerical: abs", async () => {
    const code = `
  DATA int TYPE i.
  DATA packed TYPE p LENGTH 12 DECIMALS 2.

  int = -3.
  WRITE / int.
  WRITE / abs( int ).
  int = abs( int ).
  WRITE / int.

  packed = '-123.45'.
  WRITE / abs( packed ).

  WRITE / abs( '-12' ).
  WRITE / abs( -18 ).
  WRITE / abs( 7 ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("-3\n3\n3\n123.45\n12\n18\n7");
  });

  it("999", async () => {
    const code = `
DATA iv_int TYPE int8.
DATA lv_int TYPE i.
iv_int = -1000.
lv_int = abs( iv_int + 1 ).
WRITE / lv_int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("999");
  });

});
