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

  it("int8 to hex", async () => {
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

  it("int8 to hex, minus one", async () => {
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

  it("int8 to hex, minus two", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    DATA lv_hex8 TYPE x LENGTH 8.
    lv_int8 = -2.
    lv_hex8 = lv_int8.
    WRITE / lv_hex8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("FFFFFFFFFFFFFFFE");
  });

  it("int8 to hex, one", async () => {
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

  it("int8 to hex, zero", async () => {
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

  it("int8 to hex, short", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    DATA lv_hex8 TYPE x LENGTH 3.
    lv_int8 = 1.
    lv_hex8 = lv_int8.
    WRITE / lv_hex8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("000001");
  });

  it("int8 to hex, short, negative", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    DATA lv_hex8 TYPE x LENGTH 3.
    lv_int8 = -1.
    lv_hex8 = lv_int8.
    WRITE / lv_hex8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("FFFFFF");
  });

  it("int8 to hex, short, negative, two", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    DATA lv_hex8 TYPE x LENGTH 3.
    lv_int8 = -2.
    lv_hex8 = lv_int8.
    WRITE / lv_hex8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("FFFFFE");
  });

  it("hex to int, negative 128", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    DATA lv_hex  TYPE x LENGTH 8.
    lv_hex = 'FFFFFFFFFFFFFF80'.
    lv_int8 = lv_hex.
    ASSERT lv_int8 = -128.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("hex to int, another negative", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    DATA lv_hex  TYPE x LENGTH 8.
    lv_hex = 'FFFFFFF00FFFFF80'.
    lv_int8 = lv_hex.
    ASSERT lv_int8 = -68451041408.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("hex to int, positive 128", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    DATA lv_hex  TYPE x LENGTH 8.
    lv_hex = '0000000000000080'.
    lv_int8 = lv_hex.
    ASSERT lv_int8 = 128.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("hex to int, short 255", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    DATA lv_hex  TYPE x LENGTH 1.
    lv_hex = 'FF'.
    lv_int8 = lv_hex.
    ASSERT lv_int8 = 255.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("MOD, negative", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    lv_int8 = -1.
    lv_int8 = lv_int8 MOD 64.
    WRITE / lv_int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("63");
  });

  it("abs", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    lv_int8 = -1.
    lv_int8 = abs( lv_int8 ) + lv_int8.
    WRITE / lv_int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("abs, large", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    lv_int8 = -68451041408.
    lv_int8 = abs( lv_int8 ).
    WRITE / lv_int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("68451041408");
  });

  it("abs, larger than max safe", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    lv_int8 = -9007199254740992.
    lv_int8 = abs( lv_int8 ).
    WRITE / lv_int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("9007199254740992");
  });

  it("abs, larger than max safe, positive", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    lv_int8 = 9007199254740992.
    lv_int8 = abs( lv_int8 ).
    WRITE / lv_int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("9007199254740992");
  });

  it("from packed", async () => {
    const code = `
    TYPES ty_uint64 TYPE p LENGTH 11 DECIMALS 0.
    DATA foo TYPE ty_uint64.
    DATA int TYPE int8.
    foo = 1000.
    int = foo.
    WRITE / foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1000");
  });

  it("min and max int4", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    DATA lv_max TYPE i.
    DATA lv_min TYPE i.
    lv_max = 2147483647.
    lv_min = -2147483648.
    lv_int8 = lv_max * lv_max.
    WRITE / lv_int8.
    lv_int8 = lv_min * lv_min.
    WRITE / lv_int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4611686014132420609\n4611686018427387904");
  });

  it("min and max int4, as int8", async () => {
    const code = `
    DATA lv_int8 TYPE int8.
    DATA lv_max TYPE int8.
    DATA lv_min TYPE int8.
    lv_max = 2147483647.
    lv_min = -2147483648.
    lv_int8 = lv_max * lv_max.
    WRITE / lv_int8.
    lv_int8 = lv_min * lv_min.
    WRITE / lv_int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4611686014132420609\n4611686018427387904");
  });

});