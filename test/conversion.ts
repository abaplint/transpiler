// import {expect} from "chai";
import {ABAP} from "../packages/runtime/src/";
import {AsyncFunction, runFiles} from "./_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Value conversions", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("hex value conversion", async () => {
    const code = `
  DATA hex TYPE x.
  DATA integer TYPE i.
  hex = 'AA'.
  integer = hex.
  ASSERT integer = 170.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});