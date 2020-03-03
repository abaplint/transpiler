import {expect} from "chai";
import {Transpiler} from "../packages/transpiler/src/";
import * as abap from "../packages/runtime/src/";

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
    const f = new Function("abap", js);
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
    const f = new Function("abap", js);
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
    const f = new Function("abap", js);
    f(abap);
  });

  it("Character field semantics", () => {
    const code = `
    DATA lv_str TYPE string.
    DATA lt_table TYPE STANDARD TABLE OF string.
    lv_str = 'foo bar'.
    SPLIT lv_str AT | | INTO TABLE lt_table.
    ASSERT lines( lt_table ) = 2.`;

    const js = new Transpiler().run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it.skip("ASSERTs, left hand and right hand, none should fail", () => {
    const code = `
      ASSERT 1 = 1.
      ASSERT 1 = '1'.
      ASSERT 1 = |1|.
      ASSERT 1 = \`1\`.
      ASSERT '1' = 1.
      ASSERT |1| = 1.
      ASSERT \`1\` = 1.`;

    const js = new Transpiler().run(code);
    const f = new Function("abap", js);
    f(abap);
  });

});