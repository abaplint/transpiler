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

  it("ASSERTs, left hand and right hand, none should fail", () => {
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

  it("Console tracks output", () => {
    const code = `WRITE 'foo'.`;
    const js = new Transpiler().run(code);
    const f = new Function("abap", js);
    abap.Console.clear();
    f(abap);
    expect(abap.Console.get()).to.equal("foo");
  });

  it("Offset +1", () => {
    const code = `
      DATA: bar TYPE string.
      bar = 'abc'.
      WRITE bar+1.`;
    const js = new Transpiler().run(code);
    const f = new Function("abap", js);
    abap.Console.clear();
    f(abap);
    expect(abap.Console.get()).to.equal("bc");
  });

  it("Length (1)", () => {
    const code = `
      DATA: bar TYPE string.
      bar = 'abc'.
      WRITE bar(1).`;
    const js = new Transpiler().run(code);
    const f = new Function("abap", js);
    abap.Console.clear();
    f(abap);
    expect(abap.Console.get()).to.equal("a");
  });

  it("Basic delete internal", () => {
    const code = `
      DATA table TYPE STANDARD TABLE OF i.
      APPEND 1 TO table.
      APPEND 2 TO table.
      DELETE table WHERE table_line = 1.
      ASSERT lines( table ) = 1.`;
    const js = new Transpiler().run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("String compare", () => {
    const code = `
    ASSERT 'a' < 'b'.
    ASSERT 'A' < 'b'.
    ASSERT 'A' < 'B'.
    ASSERT 'b' >= 'B'.
    ASSERT 'a' < 'ba'.
    ASSERT 1 < '2'.
    ASSERT 1 <= '1'.`;

    const js = new Transpiler().run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Basic sort table", () => {
    const code = `
    DATA: table TYPE STANDARD TABLE OF i,
          int   TYPE i.
    APPEND 2 TO table.
    APPEND 1 TO table.
    SORT table.
    LOOP AT table INTO int.
      WRITE / int.
    ENDLOOP.`;

    const js = new Transpiler().run(code);
    const f = new Function("abap", js);
    abap.Console.clear();
    f(abap);
    expect(abap.Console.get()).to.equal("1\n2");
  });

  it("Should throw an error if invalid code is requested to be transpiled", () => {
    const code = `THIS IS NOT ABAP.`;
    try {
      new Transpiler().run(code);
      throw new Error("An exception should have been raised for invalid code");
    } catch(e) {
      // expected - do nothing
    }
  });
});