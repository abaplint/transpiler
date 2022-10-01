import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - Bit compare M", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("test", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'FF01'.
    hex2 = '11'.
    IF hex1 M hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("no");
  });

});