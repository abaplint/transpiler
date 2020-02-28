import {expect} from "chai";
import {Transpiler} from "../../src/transpiler";
import * as abap from "../../src/runtime";

describe("Full Examples", () => {

  it("Fibonacci", () => {
    const code = `
    DATA: lv_old     TYPE i VALUE 1,
          lv_current TYPE i VALUE 2,
          lv_next    TYPE i.

    DO 8 TIMES.
      lv_next = lv_old + lv_current.
      lv_old = lv_current.
      lv_current = lv_next.
    ENDDO.`;

    const js = new Transpiler().run(code) + "\nreturn lv_current.get();";
    const f = new Function('abap', js);
    expect(f(abap)).to.equal(89);
  });

  it("Simple IF", () => {
    const code = `
    DATA: foo TYPE i VALUE 1,
          bar TYPE i VALUE 1.

    IF foo = bar.
      foo = 2.
    ENDIF.`;

    const js = new Transpiler().run(code) + "\nreturn foo.get();";
    const f = new Function('abap', js);
    expect(f(abap)).to.equal(2);
  });

  it.skip("Character field semantics", () => {
    const code = `
    DATA: foo TYPE c.
    foo = 'abc'.
    ASSERT foo = 'a'.
    foo = 2 + 1.
    ASSERT foo = '3'.
    ASSERT foo = 3.
    foo = 2 + '1'.
    ASSERT foo = '3'.
    ASSERT foo = 3.
    foo = 0.
    ASSERT foo = '0'.
    ASSERT foo = 0.
    foo = '0'.
    ASSERT foo = '0'.
    ASSERT foo = 0.
    foo = |0|.
    ASSERT foo = '0'.
    ASSERT foo = 0.`;

    const js = new Transpiler().run(code);
    const f = new Function('abap', js);
    f(abap);
  });
});