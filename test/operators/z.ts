// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - Bit compare Z", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("test", async () => {
    const code = `
`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
//    expect(abap.console.get()).to.equal("");
  });

});