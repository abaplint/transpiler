import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running expressions - String templates", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Output TIMESTAMP = ISO", async () => {
    const code = `
    CONSTANTS lc_epoch TYPE p VALUE '19700101000000'.
    WRITE |{ lc_epoch TIMESTAMP = ISO }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1970-01-01T00:00:00");
  });

  it("Output DATE = ISO", async () => {
    const code = `
    CONSTANTS date TYPE d VALUE '20210321'.
    WRITE |{ date DATE = ISO }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2021-03-21");
  });

  it("Output TIME = ISO", async () => {
    const code = `
    CONSTANTS time TYPE t VALUE '112233'.
    WRITE |{ time TIME = ISO }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("11:22:33");
  });

});