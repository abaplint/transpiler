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

  it.skip("Simple class", () => {
    const abap = `
    CLASS lcl_foobar DEFINITION.
      PUBLIC SECTION.
        METHODS: moo.
    ENDCLASS.

    CLASS lcl_foobar IMPLEMENTATION.
      METHOD moo.
      ENDMETHOD.
    ENDCLASS.`;

    const expected = "sdf";

    expect(new Transpiler().run(abap)).to.equal(expected);
  });

});