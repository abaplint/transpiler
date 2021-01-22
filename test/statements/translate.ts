import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - TRANSLATE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("TRANSLATE to upper case", async () => {
    const code = `
  DATA foo TYPE string.
  foo = 'abc'.
  TRANSLATE foo TO UPPER CASE.
  WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ABC");
  });

  it("TRANSLATE to lower case", async () => {
    const code = `
    DATA foo TYPE string.
    foo = 'ABC'.
    TRANSLATE foo TO LOWER CASE.
    WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("abc");
  });

  it("TRANSLATE USING", async () => {
    const code = `
DATA str TYPE string.
str = 'apps/create-from-manifest'.
TRANSLATE str USING '/_-_'.
WRITE str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("apps_create_from_manifest");
  });

});