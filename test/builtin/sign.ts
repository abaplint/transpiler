import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - sign", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Builtin numerical: sign", async () => {
    const code = `
  DATA chars TYPE c LENGTH 10.
  DATA int TYPE i.
  DATA packed TYPE p.

  chars = '12.34'.
  WRITE / sign( chars ).

  packed = chars.
  WRITE / sign( packed ).

  int = -12.
  WRITE / sign( int ).

  WRITE / frac( '0' ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n1\n-1\n0");
  });

});
