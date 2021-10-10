import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - Arithmetics", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("integers DIV", async () => {
    const code = `
      DATA lv_int TYPE i.
      lv_int = 5 / 2.
      ASSERT lv_int = 3.
      lv_int = 100 / 99.
      ASSERT lv_int = 1.
      lv_int = 5 / 3.
      ASSERT lv_int = 2.
      lv_int = 5 / 4.
      ASSERT lv_int = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("integer DIV", async () => {
    const code = `
      DATA foo TYPE i.
      foo = 5 DIV 2.
      WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("power", async () => {
    const code = `
      DATA foo TYPE i.
      foo = 5 ** 2.
      WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("25");
  });

  it("integer MOD", async () => {
    const code = `
      DATA foo TYPE i.
      foo = 5 MOD 2.
      WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("integer MOD, lower case", async () => {
    const code = `
      DATA foo TYPE i.
      foo = 5 mod 2.
      WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

});