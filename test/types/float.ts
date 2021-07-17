import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfloat.prog.abap", contents}]);
}

describe("Running Examples - Float type", () => {

  beforeEach(async () => {
    abap = new ABAP();
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
    expect(abap.console.get()).to.equal("2,0000000000000000E+00");
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

});
