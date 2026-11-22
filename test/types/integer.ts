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

  it("overflow, ** is calculated in f", async () => {
    const code = `
    DATA lv_int TYPE i.
    lv_int = 2 ** 33.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch(e) {
      expect(e.toString()).to.contain("CX_SY_CONVERSION_OVERFLOW");
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

  it("hex to int, first bit is the sign", async () => {
    const code = `
    DATA lv_hex TYPE x LENGTH 4 VALUE 'FFFFFFFF'.
    DATA lv_int TYPE i.
    lv_int = lv_hex.
    ASSERT lv_int = -1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("hex offset to int, first bit is the sign", async () => {
    const code = `
    DATA lv_hex TYPE x LENGTH 5 VALUE '00FFFFFFFF'.
    DATA lv_int TYPE i.
    lv_int = lv_hex+1.
    ASSERT lv_int = -1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("hex offset length to int, positive", async () => {
    const code = `
    DATA lv_hex TYPE x LENGTH 5 VALUE '00FFFFFFFF'.
    DATA lv_int TYPE i.
    lv_int = lv_hex+1(2).
    ASSERT lv_int = 65535.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("preceding zeros", async () => {
    const code = `
    DATA int TYPE i.
    int = 0001.
    WRITE / int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("to c too short, the right end is kept with *", async () => {
    const code = `
    DATA lv_a TYPE i VALUE 12345.
    DATA lv_c1 TYPE c LENGTH 1.
    DATA lv_c2 TYPE c LENGTH 2.
    DATA lv_c4 TYPE c LENGTH 4.
    DATA lv_c5 TYPE c LENGTH 5.
    DATA lv_c6 TYPE c LENGTH 6.
    lv_c2 = lv_a.
    WRITE / |[{ lv_c2 }]|.
    lv_c4 = lv_a.
    WRITE / |[{ lv_c4 }]|.
    lv_c5 = lv_a.
    WRITE / |[{ lv_c5 }]|.
    lv_a = -1234.
    lv_c4 = lv_a.
    WRITE / |[{ lv_c4 }]|.
    lv_c5 = lv_a.
    WRITE / |[{ lv_c5 }]|.
    lv_a = -15.
    lv_c6 = lv_a.
    WRITE / |[{ lv_c6 }]|.
    lv_a = 5.
    lv_c1 = lv_a.
    WRITE / |[{ lv_c1 }]|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("[*5]\n[*345]\n[12345]\n[*34-]\n[1234-]\n[   15-]\n[5]");
  });
});