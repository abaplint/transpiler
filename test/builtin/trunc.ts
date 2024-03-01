import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - trunc", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Builtin numerical: trunc", async () => {
    const code = `
  DATA chars TYPE c LENGTH 10.
  DATA int TYPE i.
  DATA packed TYPE p.

  chars = '12.34'.
  WRITE / trunc( chars ).

  packed = chars.
  WRITE / trunc( packed ).

  int = 12.
  WRITE / trunc( int ).

  WRITE / trunc( '-43.21' ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12\n12\n12\n-43");
  });

  it("test with abs", async () => {
    const code = `
  DATA float TYPE f.
  DATA lv_integer TYPE i.
  float = '12.375'.
  lv_integer = trunc( abs( float ) ).
  WRITE lv_integer.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("12");
  });

  it("test direct", async () => {
    const code = `
  DATA float TYPE f.
  DATA lv_integer TYPE i.
  float = '12.375'.
  lv_integer = trunc( float ).
  WRITE lv_integer.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("12");
  });

});
