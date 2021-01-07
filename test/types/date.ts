import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Date type", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Date initial value", async () => {
    const code = `
      DATA date TYPE d.
      WRITE date.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00000000");
  });

  it("Date, adding 1", async () => {
    const code = `
      DATA date TYPE d.
      date = '00020401'.
      date = date + 1.
      WRITE date.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00020402");
  });

  it("Date, adding 397", async () => {
    const code = `
      DATA date TYPE d.
      date = '20090807'.
      date = date + 397.
      WRITE date.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("20100908");
  });

});