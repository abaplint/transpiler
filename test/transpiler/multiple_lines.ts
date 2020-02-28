import {expect} from "chai";
import {Transpiler} from "../../src/transpiler";

describe("Multiple statements", () => {

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