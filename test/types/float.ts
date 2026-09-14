import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfloat.prog.abap", contents}]);
}

describe("Running Examples - Float type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("float, initial", async () => {
    const code = `
    DATA float TYPE f.
    WRITE float.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0,0000000000000000E+00");
  });

  it("float, value = 2", async () => {
    const code = `
    DATA float TYPE f.
    float = 2.
    WRITE float.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2,0000000000000000E+00");
  });

  it("float, value = 0.2", async () => {
    const code = `
  DATA f TYPE f.
  f = '0.2'.
  WRITE f.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2,0000000000000001E-01");
  });

  it("convert float to string", async () => {
    const code = `
  DATA float TYPE f.
  DATA str TYPE string.
  float = 2.
  str = float.
  WRITE str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2.0000000000000000E+00");
  });

  it("float, 12345", async () => {
    const code = `
  DATA float TYPE f.
  float = 12345.
  WRITE float.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1,2345000000000000E+04");
  });

  it("float to float", async () => {
    const code = `
  DATA lv_f TYPE f.
  DATA lv_f2 TYPE f.
  lv_f = 123.
  WRITE / lv_f.
  lv_f2 = lv_f.
  WRITE / lv_f2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1,2300000000000000E+02\n1,2300000000000000E+02");
  });

  it("negative float to hex", async () => {
    const code = `
    DATA lv_f TYPE f.
    DATA lv_hex TYPE x LENGTH 4.
    lv_f = -1000.
    lv_hex = lv_f.
    WRITE lv_hex.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("FFFFFC18");
  });

  it("Float, compare integer", async () => {
    const code = `
    DATA lv_int TYPE i.
    lv_int = 2147483647.
    DATA lv_f TYPE f.
    lv_f = 4.
    IF lv_f > lv_int.
      WRITE 'true'.
    ELSE.
      WRITE 'false'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("false");
  });

  it("Float, compare 100 and 1000", async () => {
    const code = `
  DATA lv_f1 TYPE f.
  DATA lv_f2 TYPE f.
  lv_f1 = 100.
  lv_f2 = 1000.
  ASSERT lv_f1 < lv_f2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Float, add integer to float", async () => {
    const code = `
    DATA lv_f TYPE f.
    DATA lv_int TYPE i.
    DATA lv_a TYPE f.
    lv_f = -2147483648.
    lv_int = 2147483647.
    lv_a = lv_f + lv_int + 1.
    ASSERT lv_a = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Float, compare with string", async () => {
    const code = `
  DATA float TYPE f VALUE 5.
  ASSERT float = '5.0'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Float, initial", async () => {
    const code = `
  DATA float TYPE f.
  ASSERT float IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Float, a value written out can be read back", async () => {
    // moving a float into a character field writes the decimal separator as
    // a comma, and moving it back has to accept one. Before this the pair was
    // lossy in the worst way: no error on the way out, CX_SY_CONVERSION_NO_NUMBER
    // on the way back, three layers from wherever the value came from
    const code = `
    DATA float TYPE f.
    DATA ch TYPE c LENGTH 30.
    DATA back TYPE f.
    float = '9.79440789'.
    ch = float.
    back = ch.
    ASSERT back = float.
    WRITE / ch.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("9,7944078900000004E+00");
  });

  it("Float, a point is still a point", async () => {
    // ABAP source literals carry a point, CONV f( '0.25' ) is everywhere, and
    // accepting the comma must not cost that
    const code = `
    DATA float TYPE f.
    float = CONV f( '0.25' ).
    ASSERT float = '0.25'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Float, a comma is a separator and not a licence for anything else", async () => {
    // accepting the comma must not turn "1,2,3" or "abc" into a number
    const code = `
    DATA float TYPE f.
    float = CONV f( '1,2,3' ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail("expected CX_SY_CONVERSION_NO_NUMBER");
    } catch (e) {
      expect(e.toString()).to.contain("CX_SY_CONVERSION_NO_NUMBER");
    }
  });

  it("Float to String", async () => {
    const code = `
    DATA float TYPE f.
    DATA str TYPE string.
    float = '100'.
    str = float.
    ASSERT str = '1.0000000000000000E+02'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
