// import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - BIT-OR", () => {

  beforeEach(async () => {
    abap = new ABAP(new MemoryConsole());
  });

  it("test 1", async () => {
    const code = `
    DATA hex1 TYPE x LENGTH 1.
    DATA hex2 TYPE x LENGTH 1.
    hex1 = hex1 bit-or hex2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
//    expect(abap.console.get()).to.equal("no");
  });

  it("test 2", async () => {
    const code = `
DATA hex1 TYPE x LENGTH 1.
DATA hex2 TYPE x LENGTH 1.
DEFINE _foo.
  hex1 = hex1 BIT-OR hex2.
end-of-definition.
_foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});