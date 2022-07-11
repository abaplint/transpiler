import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Field Symbol type", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("test", async () => {
    const code = `
DATA: BEGIN OF stru1,
        bar TYPE i,
      END OF stru1.
DATA: BEGIN OF stru2,
        bar TYPE i,
      END OF stru2.
FIELD-SYMBOLS <stru1> TYPE any.
FIELD-SYMBOLS <stru2> TYPE any.
stru1-bar = 2.
ASSIGN stru1 TO <stru1>.
ASSIGN stru2 TO <stru2>.
<stru2> = <stru1>.
WRITE stru2-bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});