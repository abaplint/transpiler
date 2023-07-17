import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - floor", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Builtin numerical: floor", async () => {
    const code = `
  DATA chars TYPE c LENGTH 10.
  DATA int TYPE i.
  DATA packed TYPE p.

  chars = '12.34'.
  WRITE / floor( chars ).

  packed = chars.
  WRITE / floor( packed ).

  int = 12.
  WRITE / floor( int ).

  WRITE / floor( '43.21' ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12\n12\n12\n43");
  });

  it("float", async () => {
    const code = `
DATA float TYPE f.
DATA int TYPE i.
float = '43.21'.
int = floor( float ).
ASSERT int = 43.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
