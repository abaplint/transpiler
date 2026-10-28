import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Date type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Date initial value", async () => {
    const code = `
      DATA date TYPE d.
      WRITE date.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00000000");
  });

  it("date initial value", async () => {
    const code = `
      DATA date TYPE d.
      ASSERT date IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Date, assignment from character type", async () => {
    const code = `
      DATA date TYPE d.
      date = '20010203'.
      WRITE date.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("20010203");
  });

  it("Date, assignment from numeric type", async () => {
    const code = `
      DATA date TYPE d.
      DO 24 TIMES.
        date = ( sy-index - 1 ) * 65432 + 1234.
        WRITE / date.
      ENDDO.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00040519\n01830711\n03620901\n05411023\n07201214\n09000205\n10790329\n12580520\n14370711\n16160911\n17951104\n19741228\n21540219\n23330414\n25120606\n26910730\n28700921\n30491114\n32290106\n34080301\n35870424\n37660616\n39450809\n41241001");
  });

  it("Date, assignment from numeric special cases", async () => {
    const code = `
      DATA date TYPE d.
      date = 577736.
      WRITE date.
      date = 577737.
      WRITE / date.
      date = 0.
      WRITE / date.
      date = 3652060.
      WRITE / date.
      date = 3652061.
      WRITE / date.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("15821004\n15821015\n00000000\n99991231\n00000000");
  });

  it("Date, adding 1", async () => {
    const code = `
      DATA date TYPE d.
      date = '00020401'.
      date = date + 1.
      WRITE date.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00020402");
  });

  it("Date, adding 397", async () => {
    const code = `
      DATA date TYPE d.
      date = '20090807'.
      date = date + 397.
      WRITE date.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("20100908");
  });

  it("sy date is set", async () => {
    const code = `WRITE sy-datum.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.not.equal("00000000");
  });

  it("compare empty", async () => {
    const code = `
    DATA var1 TYPE d.
    var1 = ''.
    ASSERT var1 = ''.
    ASSERT var1 = ' '.
    ASSERT var1 = '   '.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Date, assignment from an empty string is the initial date", async () => {
    const code = `
    DATA lv_d TYPE d.
    DATA lv_s TYPE string.
    FIELD-SYMBOLS <lv_any> TYPE any.
    lv_d = lv_s.
    ASSERT lv_d IS INITIAL.
    WRITE / lv_d.
    lv_d = '20250107'.
    ASSIGN lv_d TO <lv_any>.
    <lv_any> = lv_s.
    WRITE / lv_d.
    lv_s = \`2025010\`.
    lv_d = lv_s.
    ASSERT lv_d IS NOT INITIAL.
    WRITE / lv_d+0(4).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00000000\n00000000\n2025");
  });

  it("Date, CONCATENATE of blank fields gives blanks", async () => {
    const code = `
    DATA lv_y TYPE c LENGTH 4.
    DATA lv_m TYPE c LENGTH 2.
    DATA lv_d TYPE c LENGTH 2.
    DATA lv_date TYPE d.
    CONCATENATE lv_y lv_m lv_d INTO lv_date.
    WRITE / |<{ lv_date }>|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("<        >");
  });

  it("Date, to integer is days since 01.01.0001", async () => {
    const code = `
    DATA lv_date TYPE d.
    DATA lv_days TYPE i.
    lv_date = '00010101'.
    lv_days = lv_date.
    WRITE / lv_days.
    lv_date = '00010102'.
    lv_days = lv_date.
    WRITE / lv_days.
    lv_date = '20240229'.
    lv_days = lv_date.
    WRITE / lv_days.
    CLEAR lv_date.
    lv_days = lv_date.
    WRITE / lv_days.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\n1\n738946\n0");
  });

  it("Date, blank date to integer is 0", async () => {
    const code = `
    DATA lv_date TYPE d.
    DATA lv_days TYPE i.
    lv_date = '        '.
    lv_days = lv_date.
    WRITE / lv_days.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("Date, to int8 is days since 01.01.0001", async () => {
    const code = `
    DATA lv_date TYPE d VALUE '20240229'.
    DATA lv_days TYPE int8.
    lv_days = lv_date.
    WRITE / lv_days.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("738946");
  });

  it("Date, to packed is days since 01.01.0001", async () => {
    const code = `
    DATA lv_date TYPE d VALUE '20240229'.
    DATA lv_days TYPE p LENGTH 10 DECIMALS 0.
    lv_days = lv_date.
    WRITE / lv_days.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("738946");
  });

  it("Date, to float is days since 01.01.0001", async () => {
    const code = `
    DATA lv_date TYPE d VALUE '00010102'.
    DATA lv_days TYPE f.
    DATA lv_int TYPE i.
    lv_days = lv_date.
    lv_int = lv_days.
    WRITE / lv_int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("Date, to decfloat34 is days since 01.01.0001", async () => {
    const code = `
    DATA lv_date TYPE d VALUE '20240229'.
    DATA lv_days TYPE decfloat34.
    DATA lv_int TYPE i.
    lv_days = lv_date.
    lv_int = lv_days.
    WRITE / lv_int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("738946");
  });

  it("Date, days between two dates", async () => {
    const code = `
    DATA lv_from TYPE d VALUE '20231231'.
    DATA lv_to TYPE d VALUE '20240301'.
    DATA lv_days TYPE i.
    lv_days = lv_to - lv_from.
    WRITE / lv_days.
    lv_days = lv_from - lv_to.
    WRITE / lv_days.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("61\n-61");
  });

  it("Date, roundtrip via integer", async () => {
    const code = `
    DATA lv_date TYPE d VALUE '20240229'.
    DATA lv_days TYPE i.
    lv_days = lv_date.
    lv_date = lv_days.
    WRITE / lv_date.
    lv_days = lv_days + 1.
    lv_date = lv_days.
    WRITE / lv_date.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("20240229\n20240301");
  });

  it("Date, to integer via CONV", async () => {
    const code = `
    DATA lv_date TYPE d VALUE '00010102'.
    DATA lv_days TYPE i.
    lv_days = CONV i( lv_date ).
    WRITE / lv_days.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

});
