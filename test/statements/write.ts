import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - WRITE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("WRITE - single line", async () => {
    const code = `
        WRITE /. 
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("\n");
  });
});