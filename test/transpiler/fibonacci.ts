// import {expect} from "chai";
import {run} from "../../src/transpiler";

describe("Fibonacci", () => {

  it("Fibonacci", () => {
    const abap = `
    DATA: lv_old     TYPE i VALUE 1,
          lv_current TYPE i VALUE 2,
          lv_next    TYPE i.

    DO 8 TIMES.
      lv_next = lv_old + lv_current.
      lv_old = lv_current.
      lv_current = lv_next.
    ENDDO.`;

    const js = run(abap);
    console.dir(js);
  });
});