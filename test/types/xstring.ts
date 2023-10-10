import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - xstring type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("set from float, 1", async () => {
    const code = `
    DATA xstr TYPE xstring.
    DATA float TYPE f.
    float = 1.
    xstr = float.
    WRITE xstr.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("01");
  });

  it("set from float, 12345", async () => {
    const code = `
    DATA xstr TYPE xstring.
    DATA float TYPE f.
    float = 12345.
    xstr = float.
    WRITE xstr.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3039");
  });

  it("set empty from char", async () => {
    const code = `
DATA xstr TYPE xstring.
xstr = ''.
WRITE xstrlen( xstr ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("throw CX_SY_RANGE_OUT_OF_BOUNDS", async () => {
    const code = `
DATA xstr TYPE xstring.
WRITE xstr+100(1).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch (e) {
      expect(e.toString()).to.contain("CX_SY_RANGE_OUT_OF_BOUNDS");
    }
  });

});