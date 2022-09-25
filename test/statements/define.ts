import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - DEFINE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("simple", async () => {
    const code = `
DEFINE foo.
  WRITE 'hello'.
END-OF-DEFINITION.
foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("plus", async () => {
    const code = `
DEFINE foo.
  &1 = &1 + 1.
END-OF-DEFINITION.
DATA bar TYPE i.
foo bar.
WRITE bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

});