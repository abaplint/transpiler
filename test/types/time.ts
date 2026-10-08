import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Time type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Time initial value", async () => {
    const code = `
      DATA time TYPE t.
      WRITE time.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("000000");
  });

  it("Time initial value", async () => {
    const code = `
      DATA time TYPE t.
      ASSERT time IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Time, assignment from character type", async () => {
    const code = `
      DATA time TYPE t.
      time = '123456'.
      WRITE time.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("123456");
  });

  it("Time, assignment from numeric type", async () => {
    const code = `
      DATA time TYPE t.
      time = 123456.
      WRITE time.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("101736");
  });

  it("Time, adding 1", async () => {
    const code = `
      DATA time TYPE t.
      time = '000241'.
      time = time + 1.
      WRITE time.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("000242");
  });

  it("sy uzeit is set", async () => {
    const code = `WRITE sy-uzeit.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.not.equal("000000");
  });

  it("time, offset set", async () => {
    const code = `
    DATA clock TYPE t.
    clock(2) = 8.
    WRITE / clock.
    WRITE / clock(2).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("080000\n08");
  });

  it("time, empty from string", async () => {
    const code = `
    DATA iv_value TYPE string.
    DATA rv_result TYPE t.
    rv_result = '112233'.
    rv_result = iv_value.
    WRITE / rv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("000000");
  });

  it("time, from string, A", async () => {
    const code = `
    DATA iv_value TYPE string.
    DATA rv_result TYPE t.
    rv_result = '112233'.
    iv_value = |A|.
    rv_result = iv_value.
    WRITE / rv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("A00000");
  });

  it("time, from string, ABC", async () => {
    const code = `
    DATA iv_value TYPE string.
    DATA rv_result TYPE t.
    rv_result = '112233'.
    iv_value = |ABCABCABC|.
    rv_result = iv_value.
    WRITE / rv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ABCABC");
  });

  it("Time, to integer is seconds since midnight", async () => {
    const code = `
    DATA lv_time TYPE t.
    DATA lv_secs TYPE i.
    lv_time = '235930'.
    lv_secs = lv_time.
    WRITE / lv_secs.
    lv_time = '000000'.
    lv_secs = lv_time.
    WRITE / lv_secs.
    lv_time = '000101'.
    lv_secs = lv_time.
    WRITE / lv_secs.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("86370\n0\n61");
  });

  it("Time, to int8 is seconds since midnight", async () => {
    const code = `
    DATA lv_time TYPE t VALUE '235930'.
    DATA lv_secs TYPE int8.
    lv_secs = lv_time.
    WRITE / lv_secs.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("86370");
  });

  it("Time, to packed is seconds since midnight", async () => {
    const code = `
    DATA lv_time TYPE t VALUE '235930'.
    DATA lv_secs TYPE p LENGTH 10 DECIMALS 0.
    lv_secs = lv_time.
    WRITE / lv_secs.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("86370");
  });

  it("Time, to float is seconds since midnight", async () => {
    const code = `
    DATA lv_time TYPE t VALUE '000101'.
    DATA lv_secs TYPE f.
    DATA lv_int TYPE i.
    lv_secs = lv_time.
    lv_int = lv_secs.
    WRITE / lv_int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("61");
  });

  it("Time, seconds between across midnight", async () => {
    const code = `
    DATA lv_from TYPE t VALUE '235930'.
    DATA lv_to TYPE t VALUE '000040'.
    DATA lv_secs_from TYPE i.
    DATA lv_secs_to TYPE i.
    DATA lv_diff TYPE i.
    lv_secs_from = lv_from.
    lv_secs_to = lv_to.
    lv_diff = 1 * 86400 + lv_secs_to - lv_secs_from.
    WRITE / lv_diff.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("70");
  });

  it("Time, from integer is seconds MOD 86400", async () => {
    const code = `
    DATA lv_time TYPE t.
    DATA lv_secs TYPE i.
    lv_secs = 86370.
    lv_time = lv_secs.
    WRITE / lv_time.
    lv_secs = 61.
    lv_time = lv_secs.
    WRITE / lv_time.
    lv_secs = 86400.
    lv_time = lv_secs.
    WRITE / lv_time.
    lv_secs = 86461.
    lv_time = lv_secs.
    WRITE / lv_time.
    lv_secs = -1.
    lv_time = lv_secs.
    WRITE / lv_time.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("235930\n000101\n000000\n000101\n235959");
  });

  it("Time, roundtrip via integer", async () => {
    const code = `
    DATA lv_time TYPE t VALUE '123456'.
    DATA lv_secs TYPE i.
    lv_secs = lv_time.
    lv_time = lv_secs.
    WRITE / lv_time.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("123456");
  });

  it("Time, to integer via field symbol and CONV", async () => {
    const code = `
    DATA lv_time TYPE t VALUE '235930'.
    DATA lv_secs TYPE i.
    FIELD-SYMBOLS <lv_time> TYPE t.
    ASSIGN lv_time TO <lv_time>.
    lv_secs = <lv_time>.
    WRITE / lv_secs.
    lv_secs = CONV i( lv_time ).
    WRITE / lv_secs.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("86370\n86370");
  });

});
