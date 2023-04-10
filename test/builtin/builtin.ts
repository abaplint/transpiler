import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

// todo, split these tests into separate files

describe("Builtin functions", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("boolc test", async () => {
    const code = `
      DATA rv_yes TYPE abap_bool.
      DATA iv_path TYPE string.
      iv_path = '/'.
      rv_yes = boolc( iv_path = '/' ).
      WRITE rv_yes.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("X");
  });

  it("nested calls to builtins", async () => {
    const code = `
      DATA lv_line TYPE string.
      lv_line = to_upper( shift_left( val = 'aabb' sub = 'a' ) ).
      ASSERT lv_line = 'BB'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("basic xstrlen", async () => {
    const code = `
      DATA foo TYPE xstring.
      foo = 'AA'.
      WRITE xstrlen( foo ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("sy-abcde", async () => {
    const code = `WRITE sy-abcde.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
  });

  it("sy-uname", async () => {
    const code = `WRITE sy-uname.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.not.equal("");
  });

});
