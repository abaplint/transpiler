import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - condense", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("condense", async () => {
    const code = `
      DATA foo TYPE string.
      foo = '12  3 '.
      foo = condense( foo ).
      WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12 3");
  });

  it("condense integer", async () => {
    const code = `
      DATA foo TYPE string.
      data bar type i.
      bar = 2.
      foo = bar.
      foo = condense( foo ).
      WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});
