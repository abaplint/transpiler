import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zdecfloat34.prog.abap", contents}]);
}

describe("Running Examples - Decfloat34 type", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("basic", async () => {
    const code = `
DATA foo TYPE decfloat34.
foo = 1.
WRITE / foo.

foo = '1.12345'.
WRITE / foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n1,12345");
  });

  it("multiply", async () => {
    const code = `
    DATA dec TYPE decfloat34.
    dec = 1 / 2.
    dec = dec * 10.
    WRITE / dec.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5");
  });

});
