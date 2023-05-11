import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - strlen", () => {

  beforeEach(async () => {
    abap = new ABAP(new MemoryConsole());
  });

  it("basic strlen", async () => {
    const code = `
      DATA foo TYPE string.
      foo = '123'.
      WRITE strlen( foo ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("basic strlen, character", async () => {
    const code = `
      DATA foo TYPE c LENGTH 10.
      foo = '123'.
      WRITE strlen( foo ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("sdf", async () => {
    const code = `
    DATA foo TYPE c LENGTH 40.
    DATA str TYPE string.
    foo = 'sdf'.
    str = foo.
    WRITE strlen( str ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

});
