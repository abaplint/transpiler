import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CREATE DATA", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("CREATE DATA, check INITIAL", async () => {
    const code = `
DATA foo TYPE REF TO i.
ASSERT foo IS INITIAL.
CREATE DATA foo.
ASSERT foo IS NOT INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE DATA, assign value and write", async () => {
    const code = `
    DATA foo TYPE REF TO i.
    CREATE DATA foo.
    foo->* = 2.
    WRITE foo->*.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});