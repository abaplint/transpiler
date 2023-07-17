import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - ceil", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Builtin numerical: ceil", async () => {
    const code = `
  DATA chars TYPE c LENGTH 10.
  DATA int TYPE i.
  DATA packed TYPE p.

  chars = '12.34'.
  WRITE / ceil( chars ).

  packed = chars.
  WRITE / ceil( packed ).

  int = 12.
  WRITE / ceil( int ).

  WRITE / ceil( '43.21' ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("13\n12\n12\n44");
  });

});
