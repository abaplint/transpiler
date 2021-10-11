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

  it("integers DIV 1", async () => {
    const code = `
      DATA lv_int TYPE i.
      lv_int = 5 / 2.
      ASSERT lv_int = 3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("integers DIV 2", async () => {
    const code = `
      DATA lv_int TYPE i.
      lv_int = 100 / 99.
      ASSERT lv_int = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("integers DIV 3", async () => {
    const code = `
      DATA lv_int TYPE i.
      lv_int = 5 / 3.
      ASSERT lv_int = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("integers DIV 4", async () => {
    const code = `
      DATA lv_int TYPE i.
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

  it("rationals", async () => {
    const code = `
  ASSERT 1 / 5 = + '0.2'.
  ASSERT '0.2' * 2 = + '0.4'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("rationals 2", async () => {
    const code = `
  DATA f TYPE f.
  f = 1 / 5.
  ASSERT f = + '0.2'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("rationals 3", async () => {
    const code = `
  DATA f TYPE f.
  f = 1 / 5.
  f = f * 5.
  ASSERT f = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("power, fractions", async () => {
    const code = `
  DATA f TYPE f.
  f = 1 / 5.
  f = f ** 2.
  WRITE f.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4,0000000000000008E-02");
  });

});