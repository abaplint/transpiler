import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - Arithmetics", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
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
    const code = `ASSERT 1 / 5 = + '0.2'.`;
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

  it("degrees to radians", async () => {
    const code = `
  CONSTANTS pi TYPE f VALUE '3.14159265359'.
  DATA degrees TYPE f.
  DATA radians TYPE f.
  degrees = 90.
  radians = ( degrees * pi ) / 180.
  WRITE radians.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1,5707963267950003E+00");
  });

  it("more cos, 1", async () => {
    const code = `
  CONSTANTS pi TYPE f VALUE '3.14159265359'.
  DATA new_x TYPE f.
  new_x = cos( pi ).
  WRITE new_x.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("-1,0000000000000000E+00");
  });

  it("more cos, 2", async () => {
    const code = `
  CONSTANTS pi TYPE f VALUE '3.14159265'.
  DATA new_x TYPE f.
  new_x = cos( pi ).
  WRITE new_x.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("-1,0000000000000000E+00");
  });

  it("mod, negative value", async () => {
    const code = `
    DATA n TYPE i.
    n = -4 MOD 3.
    WRITE / n.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("strlen plussed in string template", async () => {
    const code = `
  DATA result TYPE string.
DATA duplicates TYPE string.
duplicates = |sdf|.
result = |bar{ strlen( duplicates ) + 1 }foo|.
WRITE result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar4foo");
  });

  it("string plus integer into string", async () => {
    const code = `
    DATA lv_trow1 TYPE string.
    DATA lv_trow2 TYPE string.
    DATA iv_shift_rows TYPE i.
    lv_trow1 = '1'.
    iv_shift_rows = 2.
    lv_trow2 = lv_trow1 + iv_shift_rows.
    WRITE lv_trow2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3 ");
  });

  it("char plus integer into char", async () => {
    const code = `
    DATA lv_trow1 TYPE string.
    DATA lv_trow2 TYPE string.
    DATA iv_shift_rows TYPE i.
    lv_trow1 = '1'.
    iv_shift_rows = 2.
    lv_trow2 = lv_trow1 + iv_shift_rows.
    WRITE lv_trow2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3 ");
  });

  it("DIV, negative value", async () => {
    const code = `
    DATA int TYPE i.
    int = -1.
    int = int DIV 256.
    WRITE int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("-1");
  });

  it("DIV, short hex, 1", async () => {
    const code = `
    DATA hex TYPE x LENGTH 1.
    DATA integer TYPE i.
    hex = 'FF'.
    integer = hex DIV 10.
    WRITE integer.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("25");
  });

  it("DIV, short hex, 2", async () => {
    const code = `
    DATA hex TYPE x LENGTH 2.
    DATA integer TYPE i.
    hex = 'FFFF'.
    integer = hex DIV 10.
    WRITE integer.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("6553");
  });

  it("DIV, short hex, length 1", async () => {
    const code = `
    DATA crc TYPE x LENGTH 1.
    crc = -1.
    crc = crc DIV 2.
    WRITE crc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("7F");
  });

  it("character field plus 1", async () => {
    const code = `
    DATA foo TYPE c LENGTH 10.
    foo = '1000'.
    foo = foo + 1.
    WRITE / foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("     1001");
  });

  it("character field minus 1", async () => {
    const code = `
    DATA foo TYPE c LENGTH 10.
    foo = '1000'.
    foo = foo - 1.
    WRITE / foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("      999");
  });

  it("integers, int8 div", async () => {
    const code = `
    DATA val TYPE int8.
    val = 198620152477517.
    val = val DIV 8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("integers, int8 add", async () => {
    const code = `
    DATA val TYPE int8.
    val = 198620152477517.
    val = val + val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("integers, power", async () => {
    const code = `
    DATA lv_maxint TYPE i.
    lv_maxint = 2 ** 31 - 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("integers, max", async () => {
    const code = `
    DATA lv_maxint TYPE i.
    DATA lv_f      TYPE f.
    DATA lv_a      TYPE f.
    lv_f = 1000.
    lv_maxint = 2 ** 31 - 1.
    lv_a = lv_f + lv_maxint + 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.skip("integers, max", async () => {
    const code = `
    DATA lv_maxint TYPE i.
    DATA lv_b      TYPE f.
    DATA lv_a      TYPE f.

    lv_maxint = 2 ** 31 - 1.
    lv_b = 2 * ( lv_maxint + 1 ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("zero DIV zero", async () => {
    const code = `
    DATA lv_length     TYPE i.
    DATA lv_iterations TYPE i.
    DATA ev_size       TYPE i.
    lv_iterations = ev_size DIV lv_length.
    ASSERT lv_iterations = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("zero / zero", async () => {
    const code = `
    DATA lv_length     TYPE i.
    DATA lv_iterations TYPE i.
    DATA ev_size       TYPE i.
    lv_iterations = ev_size / lv_length.
    ASSERT lv_iterations = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.skip("hitting int max value", async () => {
    const code = `
    DATA lv_int1 TYPE i.
    DATA lv_int2 TYPE i.
    lv_int1 = 2000000.
    lv_int2 = 2000000.
    lv_int1 = lv_int1 * lv_int2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch(e) {
      expect(e.toString()).to.contain("COMPUTE_INT_TIMES_OVERFLOW");
    }
  });

  it.skip("hitting int MIN value", async () => {
    const code = `
    DATA lv_int1 TYPE i.
    DATA lv_int2 TYPE i.
    lv_int1 = -2000000.
    lv_int2 = -2000000.
    lv_int1 = lv_int1 * lv_int2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch(e) {
      expect(e.toString()).to.contain("COMPUTE_INT_TIMES_OVERFLOW");
    }
  });

  it("ok, not int max value, its a float field", async () => {
    const code = `
    DATA lv_f TYPE f.
    DATA lv_int1 TYPE i.
    DATA lv_int2 TYPE i.
    lv_int1 = 2000000.
    lv_int2 = 2000000.
    lv_f = lv_int1 * lv_int2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("MOD, both negative", async () => {
    const code = `
    DATA int TYPE i.
    int = -5 MOD -2.
    ASSERT int = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("MOD, negative", async () => {
    const code = `
    DATA int TYPE i.
    int = 5 MOD -2.
    ASSERT int = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("MOD, negative, another", async () => {
    const code = `
    DATA int TYPE i.
    int = -7 MOD 3.
    ASSERT int = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("multiply, overfolow int", async () => {
    const code = `
DATA int1 TYPE i.
DATA int2 TYPE i.
int1 = 268435456.
int2 = 4096.
int1 = int1 * int2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch (e) {
      expect(e.toString()).to.contain("CX_SY_ARITHMETIC_OVERFLOW");
    }
  });

});