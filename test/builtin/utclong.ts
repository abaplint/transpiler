import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

async function expectValue(code: string, expected: string) {
  const js = await run(code);
  const f = new AsyncFunction("abap", js);
  await f(abap);
  expect(abap.console.get()).to.equal(expected);
}

async function expectError(code: string, name: string) {
  const js = await run(code);
  const f = new AsyncFunction("abap", js);
  try {
    await f(abap);
    expect.fail();
  } catch (e) {
    expect(e.toString()).to.contain(name);
  }
}

describe("Builtin functions - utclong", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("utclong_add, days, minutes and seconds", async () => {
    await expectValue(`
    DATA ts TYPE utclong.
    DATA seconds TYPE decfloat34.
    DATA text TYPE string.
    ts = '2024-01-01 15:55:14.1173220'.
    seconds = '53.12'.
    ts = utclong_add( val = ts days = 1 minutes = 13 seconds = seconds ).
    text = ts.
    WRITE text.`, "2024-01-02 16:09:07.2373220");
  });

  it("utclong_add, negative", async () => {
    await expectValue(`
    DATA ts TYPE utclong.
    DATA text TYPE string.
    ts = '2024-01-01 00:30:00.0000000'.
    ts = utclong_add( val = ts minutes = -45 ).
    text = ts.
    WRITE text.`, "2023-12-31 23:45:00.0000000");
  });

  it("utclong_add, seconds since the unix epoch", async () => {
    await expectValue(`
    DATA ts TYPE utclong.
    DATA seconds TYPE int8.
    DATA text TYPE string.
    ts = '1970-01-01 00:00:00.0000000'.
    seconds = 1730617950.
    ts = utclong_add( val = ts seconds = seconds ).
    text = ts.
    WRITE text.`, "2024-11-03 07:12:30.0000000");
  });

  it("utclong_add, leap years", async () => {
    await expectValue(`
    DATA ts TYPE utclong.
    DATA text TYPE string.
    ts = '2024-02-28 12:00:00.0000000'.
    ts = utclong_add( val = ts days = 1 ).
    text = ts.
    WRITE / text.
    ts = '2100-02-28 12:00:00.0000000'.
    ts = utclong_add( val = ts days = 1 ).
    text = ts.
    WRITE / text.`, "2024-02-29 12:00:00.0000000\n2100-03-01 12:00:00.0000000");
  });

  it("utclong_add, seconds rounded to seven decimals", async () => {
    await expectValue(`
    DATA ts TYPE utclong.
    DATA seconds TYPE decfloat34.
    DATA text TYPE string.
    ts = '2024-01-01 00:00:00.0000000'.
    seconds = '0.00000005'.
    ts = utclong_add( val = ts seconds = seconds ).
    text = ts.
    WRITE text.`, "2024-01-01 00:00:00.0000001");
  });

  it("utclong_add, initial counts as the lowest time stamp", async () => {
    await expectValue(`
    DATA ts TYPE utclong.
    DATA text TYPE string.
    ts = utclong_add( val = ts seconds = 1 ).
    text = ts.
    WRITE / text.
    ts = utclong_add( val = ts seconds = -1 ).
    text = ts.
    WRITE / text.
    IF ts IS INITIAL.
      WRITE / 'initial'.
    ENDIF.`, "0001-01-01 00:00:01.0000000\n0001-01-01 00:00:00.0000000");
  });

  it("utclong_add, beyond the last time stamp", async () => {
    await expectError(`
    DATA ts TYPE utclong.
    ts = '9999-12-31 23:59:59.9999999'.
    ts = utclong_add( val = ts seconds = 1 ).`, "CX_SY_ARITHMETIC_OVERFLOW");
  });

  it("utclong_add, before the first time stamp", async () => {
    await expectError(`
    DATA ts TYPE utclong.
    ts = '0001-01-01 00:00:00.0000000'.
    ts = utclong_add( val = ts days = -1 ).`, "CX_SY_ARITHMETIC_OVERFLOW");
  });

  it("utclong_diff, both directions", async () => {
    await expectValue(`
    DATA high TYPE utclong.
    DATA low TYPE utclong.
    DATA diff TYPE i.
    high = '2024-01-01 06:30:00.0000000'.
    low = '2024-01-01 05:30:00.0000000'.
    diff = utclong_diff( high = high low = low ).
    WRITE / diff.
    diff = utclong_diff( high = low low = high ).
    WRITE / diff.`, "3600\n-3600");
  });

  it("utclong_diff, seven decimals", async () => {
    await expectValue(`
    DATA high TYPE utclong.
    DATA low TYPE utclong.
    DATA diff TYPE p LENGTH 16 DECIMALS 7.
    high = '2026-09-15 10:00:00.1234567'.
    low = '2026-09-15 10:00:00.0000000'.
    diff = utclong_diff( high = high low = low ).
    WRITE diff.`, "0,1234567");
  });

  it("utclong_diff, initial counts as the lowest time stamp", async () => {
    await expectValue(`
    DATA high TYPE utclong.
    DATA low TYPE utclong.
    DATA diff TYPE i.
    high = '0001-01-01 00:00:10.0000000'.
    diff = utclong_diff( high = high low = low ).
    WRITE diff.`, "10");
  });

  it("utclong_diff, invalid time stamp", async () => {
    await expectError(`
    DATA high TYPE utclong.
    DATA low TYPE utclong.
    DATA diff TYPE i.
    high = '2024-02-30 00:00:00.0000000'.
    diff = utclong_diff( high = high low = low ).`, "CX_SY_CONVERSION_NO_DATE_TIME");
  });

  it("utclong_current", async () => {
    await expectValue(`
    DATA ts TYPE utclong.
    DATA text TYPE string.
    ts = utclong_current( ).
    text = ts.
    IF text >= '2024' AND strlen( text ) = 27.
      WRITE 'ok'.
    ENDIF.`, "ok");
  });

  it("DESCRIBE FIELD of a utclong", async () => {
    await expectValue(`
    DATA ts TYPE utclong.
    DATA type TYPE c LENGTH 1.
    DESCRIBE FIELD ts TYPE type.
    WRITE type.`, "p");
  });

});
