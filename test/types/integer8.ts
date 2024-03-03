import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Integer8 type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("int8", async () => {
    const code = `
  DATA val TYPE int8.
  val = 2.
  WRITE val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("int8 from large constant", async () => {
    const code = `
  DATA val TYPE int8.
  val = 198620152477517.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("int8 from large constant, to string", async () => {
    const code = `
    DATA lv_int TYPE int8.
    DATA lv_str TYPE string.
    lv_int = 9219994337134247936.
    lv_str = lv_int.
    WRITE lv_str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("9219994337134247936");
  });

  it("int8 from float", async () => {
    const code = `
    DATA float TYPE f.
    DATA int8 TYPE int8.
    float = 1.
    int8 = float.
    WRITE / int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("int8 from int", async () => {
    const code = `
    DATA int TYPE i.
    DATA int8 TYPE int8.
    int = 1.
    int8 = int.
    WRITE / int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("int8 from int, plus", async () => {
    const code = `
    DATA int TYPE i.
    DATA int8 TYPE int8.
    int = 1.
    int8 = int8 + int.
    WRITE / int8.
    int8 = int + int8.
    WRITE / int8.
    int8 = int8 + int8.
    WRITE / int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n4");
  });

  it("int8 from int, minus", async () => {
    const code = `
    DATA int TYPE i.
    DATA int8 TYPE int8.
    int8 = 2.
    int = 1.
    int8 = int8 - int.
    WRITE / int8.
    int8 = int - int8.
    WRITE / int8.
    int8 = int8 - int8.
    WRITE / int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n0\n0");
  });

  it("int8 from int, MOD", async () => {
    const code = `
    DATA int TYPE i.
    DATA int8 TYPE int8.
    int8 = 2.
    int = 2.
    int8 = int8 MOD int.
    WRITE / int8.
    int8 = 2.
    int8 = int MOD int8.
    WRITE / int8.
    int8 = 2.
    int8 = int8 MOD int8.
    WRITE / int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\n0\n0");
  });

  it("int8 from int, mul", async () => {
    const code = `
    DATA int TYPE i.
    DATA int8 TYPE int8.
    int8 = 2.
    int = 2.
    int8 = int8 * int.
    WRITE / int8.
    int8 = 2.
    int8 = int * int8.
    WRITE / int8.
    int8 = 2.
    int8 = int8 * int8.
    WRITE / int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4\n4\n4");
  });

  it("int8 from int, divide", async () => {
    const code = `
    DATA int TYPE i.
    DATA int8 TYPE int8.
    int8 = 2.
    int = 2.
    int8 = int8 / int.
    WRITE / int8.
    int8 = 2.
    int8 = int / int8.
    WRITE / int8.
    int8 = 2.
    int8 = int8 / int8.
    WRITE / int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n1\n1");
  });

  it("compare, eq ints", async () => {
    const code = `
    DATA int TYPE i.
    DATA int8 TYPE int8.

    IF int = int8.
      WRITE / 'yes1'.
    ENDIF.

    IF int8 = int.
      WRITE / 'yes2'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes1\nyes2");
  });

  it("compare, eq floats", async () => {
    const code = `
    DATA int TYPE f.
    DATA int8 TYPE int8.

    IF int = int8.
      WRITE / 'yes1'.
    ENDIF.

    IF int8 = int.
      WRITE / 'yes2'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes1\nyes2");
  });

  it("compare, lt floats", async () => {
    const code = `
    DATA int TYPE i.
    DATA int8 TYPE int8.

    IF int < int8.
      WRITE / 'yes1'.
    ENDIF.

    IF int8 < int.
      WRITE / 'yes2'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("");
  });

  it("compare, le floats", async () => {
    const code = `
    DATA int TYPE i.
    DATA int8 TYPE int8.

    IF int <= int8.
      WRITE / 'yes1'.
    ENDIF.

    IF int8 <= int.
      WRITE / 'yes2'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes1\nyes2");
  });

  it("into int from int8", async () => {
    const code = `
    DATA int TYPE i.
    DATA int8 TYPE int8.
    int8 = 2.
    int = int8.
    WRITE int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("into float from int8", async () => {
    const code = `
    DATA float TYPE f.
    DATA int8 TYPE int8.
    int8 = 2.
    float = int8.
    WRITE float.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2,0000000000000000E+00");
  });

  it("initial", async () => {
    const code = `
    DATA int8 TYPE int8.
    ASSERT int8 IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("int8 into hex", async () => {
    const code = `
    DATA lv_tmp TYPE int8.
    DATA lv_hex TYPE x LENGTH 1.
    lv_tmp = 45645.
    lv_hex = lv_tmp MOD 256.
    WRITE / lv_hex.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4D");
  });

  it("xstr offsets", async () => {
    const code = `
    DATA lv_tmp TYPE int8.
    DATA lv_hex TYPE xstring.
    lv_tmp = 1.
    lv_hex = '00112233445566778899'.
    WRITE / lv_hex(lv_tmp).
    WRITE / lv_hex+lv_tmp(1).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00\n11");
  });

  it("hex to int8", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    DATA lv_hex8 TYPE x LENGTH 8.
    lv_int8 = 4179443068345003452.
    lv_hex8 = lv_int8.
    WRITE / lv_hex8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3A005D53B7AC2DBC");
  });

  it("hex to int8, minus one", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    DATA lv_hex8 TYPE x LENGTH 8.
    lv_int8 = -1.
    lv_hex8 = lv_int8.
    WRITE / lv_hex8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("FFFFFFFFFFFFFFFF");
  });

  it("hex to int8, one", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    DATA lv_hex8 TYPE x LENGTH 8.
    lv_int8 = 1.
    lv_hex8 = lv_int8.
    WRITE / lv_hex8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0000000000000001");
  });

  it("hex to int8, zero", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    DATA lv_hex8 TYPE x LENGTH 8.
    lv_int8 = 0.
    lv_hex8 = lv_int8.
    WRITE / lv_hex8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0000000000000000");
  });

});