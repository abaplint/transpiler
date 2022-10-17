import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - WHILE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("simple", async () => {
    const code = `
DATA i TYPE i.
WHILE i < 4.
  WRITE / sy-index.
  i = i + 1.
ENDWHILE.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3\n4");
  });

});