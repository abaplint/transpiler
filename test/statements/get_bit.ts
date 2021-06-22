import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - GET BIT", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("GET BIT", async () => {
    const code = `
    DATA lv_bit TYPE i.
    DATA lv_c TYPE c LENGTH 1.
    DATA result TYPE string.
    DATA lv_x TYPE xstring.
    lv_x = 'AB'.
    DO 8 TIMES.
      GET BIT sy-index OF lv_x INTO lv_c.
      CONCATENATE result lv_c INTO result.
    ENDDO.
    WRITE / result.
    result = ''.
    lv_x = '01'.
    DO 8 TIMES.
      GET BIT sy-index OF lv_x INTO lv_c.
      CONCATENATE result lv_c INTO result.
    ENDDO.
    WRITE / result.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("10101011\n00000001");
  });

  it("first bit of x", async () => {
    const code = `
  DATA x TYPE x.
  DATA c TYPE c.
  x = '01'.
  GET BIT 1 OF x INTO c.
  WRITE c.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("GET all da BITs", async () => {
    const code = `
  DATA rv_bitbyte TYPE c LENGTH 8.
  DATA iv_x TYPE x VALUE '40'.
  GET BIT 1 OF iv_x INTO rv_bitbyte+0(1).
  GET BIT 2 OF iv_x INTO rv_bitbyte+1(1).
  GET BIT 3 OF iv_x INTO rv_bitbyte+2(1).
  GET BIT 4 OF iv_x INTO rv_bitbyte+3(1).
  GET BIT 5 OF iv_x INTO rv_bitbyte+4(1).
  GET BIT 6 OF iv_x INTO rv_bitbyte+5(1).
  GET BIT 7 OF iv_x INTO rv_bitbyte+6(1).
  GET BIT 8 OF iv_x INTO rv_bitbyte+7(1).
  WRITE rv_bitbyte.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("01000000");
  });

  it("GET more BITs", async () => {
    const code = `
  DATA lv_c      TYPE c LENGTH 1.
  DATA lv_length TYPE i.
  DATA rv_bits   TYPE string.
  DATA iv_binary TYPE x LENGTH 7.
  iv_binary = 'CD06CA7C7E10C9'.
  lv_length = xstrlen( iv_binary ).
  lv_length = lv_length * 8.
  DO lv_length TIMES.
    GET BIT sy-index OF iv_binary INTO lv_c.
    CONCATENATE rv_bits lv_c INTO rv_bits.
  ENDDO.
  WRITE rv_bits.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("11001101000001101100101001111100011111100001000011001001");
  });

});