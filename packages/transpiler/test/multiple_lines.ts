import {expect} from "chai";
import {Transpiler} from "../src";

describe("Multiple statements", () => {

  it("IF + ELSEIF + ELSE", () => {
    const abap = `
    IF foo = bar.
    write moo.
    ELSEIF moo = boo.
    ELSE.
    ENDIF.`;

    const expected =
`if (foo.eq(bar)) {
  abap.statements.write(moo);
} else if (moo.eq(boo)) {
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