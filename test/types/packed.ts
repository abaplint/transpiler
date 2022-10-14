import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;


async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Packed type", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("initial value", async () => {
    const code = `
      DATA foo TYPE p.
      WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("compare value", async () => {
    const code = `
DATA foo TYPE p LENGTH 5.
foo = 12345.
ASSERT foo = '12345'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("compare value, different lengths", async () => {
    const code = `
  DATA foo TYPE p LENGTH 2.
  DATA bar TYPE p LENGTH 4.
  ASSERT foo = bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("long value", async () => {
    const code = `
  DATA pack TYPE p LENGTH 15.
  pack = '19710201012320'.
  ASSERT pack = '19710201012320'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("testing length", async () => {
    const code = `
  DATA lv_timestamp TYPE p LENGTH 8.
  lv_timestamp = '19500505185024'.
  ASSERT lv_timestamp = '19500505185024'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.skip("31 digits", async () => {
    const code = `
    DATA foo TYPE p LENGTH 16.
    foo = 5465645645698765645645646545644.
    WRITE foo.`;
    const js = await run(code);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5465645645698765645645646545644");
  });

  it("length 5, decimals 2", async () => {
    const code = `
    DATA foo TYPE p LENGTH 4 DECIMALS 2.
    foo = '1212.123'.
    WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1212.12");
  });

  it("length 5, decimals 2, rounding", async () => {
    const code = `
    DATA foo TYPE p LENGTH 4 DECIMALS 2.
    foo = '1212.127'.
    WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1212.13");
  });

  it("parse from char", async () => {
    const code = `
    DATA foo TYPE p.
    foo = '2.00'.
    ASSERT foo = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("compare, 2 decimals", async () => {
    const code = `
    TYPES total TYPE p LENGTH 3 DECIMALS 2.
    DATA val TYPE total.
    val = '15.2'.
    ASSERT val = '15.2'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("one hundred", async () => {
    const code = `
    TYPES total TYPE p LENGTH 3 DECIMALS 2.
    DATA val TYPE total.
    val = 100.
    val = val + '0.01'.
    ASSERT val = '100.01'.
    WRITE val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("100.01");
  });

  it("two thirds", async () => {
    const code = `
    TYPES total TYPE p LENGTH 3 DECIMALS 2.
    DATA val TYPE total.
    val = 100.
    val = val + ( 2 / 3 ).
    WRITE val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("100.67");
  });

});
