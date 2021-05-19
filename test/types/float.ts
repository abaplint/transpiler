import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfloat.prog.abap", contents}]);
}

describe("Running Examples - Float type", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("float, value = 2", async () => {
    const code = `
    DATA float TYPE f.
    float = 2.
    WRITE float.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2,0000000000000000E+00");
  });

  it("convert float to string", async () => {
    const code = `
  DATA float TYPE f.
  DATA str TYPE string.
  float = 2.
  str = float.
  WRITE str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2,0000000000000000E+00");
  });

});
