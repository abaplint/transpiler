import {expect} from "chai";
import {Transpiler} from "../src";

describe("Multiple lines", () => {

  it("IF + ELSEIF + ELSE", () => {
    const abap = `
    IF foo = bar.
    write moo.
    ELSEIF moo = boo.
    ELSE.
    ENDIF.`;

    const expected =
`if (abap.compare.eq(foo, bar)) {
  abap.statements.write(moo);
} else if (abap.compare.eq(moo, boo)) {
} else {
}`;

    expect(new Transpiler().run(abap)).to.equal(expected);
  });

  it("Interfaces should be skipped", () => {
    const abap = `
  INTERFACE lif_foobar.
  ENDINTERFACE.`;

    expect(new Transpiler().run(abap)).to.equal("");
  });

  it("Simple class", () => {
    const abap = `
    CLASS lcl_foobar DEFINITION.
      PUBLIC SECTION.
        METHODS: moo.
    ENDCLASS.

    CLASS lcl_foobar IMPLEMENTATION.
      METHOD moo.
      ENDMETHOD.
    ENDCLASS.`;

    const expected =
`class lcl_foobar {
  moo() {
  }
}`;

    expect(new Transpiler().run(abap)).to.equal(expected);
  });

  it.skip("Simple class, with input parameter", () => {
    const abap = `
    CLASS lcl_foobar DEFINITION.
      PUBLIC SECTION.
        METHODS: moo
          IMPORTING iv_foo TYPE string.
    ENDCLASS.

    CLASS lcl_foobar IMPLEMENTATION.
      METHOD moo.
      ENDMETHOD.
    ENDCLASS.`;

    const expected =
`class lcl_foobar {
  moo(unique1) {
    let iv_foo = unique1.iv_foo;
  }
}`;

    expect(new Transpiler().run(abap)).to.equal(expected);
  });

  it("CASE", () => {
    const abap = `
CASE bar.
WHEN 'foo'.
WRITE 2.
WHEN 1 OR 2.
WHEN foo.
WHEN OTHERS.
ENDCASE.`;

    const expected =
`switch (bar.get()) {
  case 'foo':
  abap.statements.write(2);
  break;
  case 1:
  case 2:
  break;
  case foo.get():
  break;
  default:
  break;
}`;

    expect(new Transpiler().run(abap)).to.equal(expected);
  });

});