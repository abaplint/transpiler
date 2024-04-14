import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Integer type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("int", async () => {
    const code = `
  DATA int TYPE i.
  int = 2.
  WRITE int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("int, negative", async () => {
    const code = `
  DATA int TYPE i.
  int = -2.
  WRITE int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("-2");
  });

  it("int, positive", async () => {
    const code = `
  DATA int TYPE i.
  int = +4.
  WRITE int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("no thousand separator, design choice?", async () => {
    const code = `
  DATA int TYPE i.
  int = 2000.
  WRITE int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2000");
  });

  it("convert plus 1", async () => {
    const code = `
    DATA int TYPE i.
    int = '+1.0'.
    WRITE int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("output negative via string", async () => {
    const code = `
    DATA foo TYPE i.
    DATA str TYPE string.
    foo = -68.
    str = foo.
    WRITE str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("68-");
  });

  it("character spaces", async () => {
    const code = `
    DATA foo TYPE i.
    DATA char TYPE c LENGTH 3.
    char = '3 '.
    foo = char.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("negative, sign right", async () => {
    const code = `
    DATA int TYPE i.
    int = '100-'.
    ASSERT int = -100.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.skip("overflow", async () => {
    const code = `
    DATA lv_int TYPE i.
    lv_int = 2 ** 33.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch(e) {
      expect(e.toString()).to.contain("CX_SY_ARITHMETIC_OVERFLOW");
    }
  });

  it("negative to hex1", async () => {
    const code = `
    DATA int TYPE i VALUE -4242.
    DATA hex TYPE x LENGTH 1.
    hex = int.
    ASSERT hex = '6E'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("negative to hex2", async () => {
    const code = `
    DATA int TYPE i VALUE -4242.
    DATA hex TYPE x LENGTH 2.
    hex = int.
    ASSERT hex = 'EF6E'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("negative to hex3", async () => {
    const code = `
    DATA int TYPE i VALUE -4242.
    DATA hex TYPE x LENGTH 3.
    hex = int.
    WRITE / hex.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("FFEF6E");
  });

  it("rounding from char, up", async () => {
    const code = `
    DATA lv_date_int TYPE i.
    lv_date_int = '45141.58832'.
    WRITE / lv_date_int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("45142");
  });

  it("rounding from char, down", async () => {
    const code = `
    DATA lv_date_int TYPE i.
    lv_date_int = '45141.48832'.
    WRITE / lv_date_int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("45141");
  });
});