import {expect} from "chai";
import {runSingle} from "./_utils";

describe("Multiple lines", () => {

  it("IF + ELSEIF + ELSE", async () => {
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

    expect(await runSingle(abap, {ignoreSyntaxCheck: true})).to.equal(expected);
  });

  it("Interfaces should be skipped", async () => {
    const abap = `
  INTERFACE lif_foobar.
  ENDINTERFACE.`;

    expect(await runSingle(abap)).to.equal("");
  });

  it("TYPES should be skipped", async () => {
    const abap = `
    TYPES: BEGIN OF ty_header,
    field TYPE string,
    value TYPE string,
  END OF ty_header.`;

    expect(await runSingle(abap)).to.equal("");
  });

  it("Simple class", async () => {
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
  constructor() {
    this.me = new abap.types.ABAPObject();
  }
  moo() {
  }
}`;

    expect(await runSingle(abap)).to.equal(expected);
  });

  it("Simple class, with input parameter", async () => {
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
  constructor() {
    this.me = new abap.types.ABAPObject();
  }
  moo(unique1) {
    let iv_foo = new abap.types.String();
    if (unique1 && unique1.iv_foo) {iv_foo.set(unique1.iv_foo);}
  }
}`;

    expect(await runSingle(abap)).to.equal(expected);
  });

  it("Simple class, with return parameter", async () => {
    const abap = `
    CLASS lcl_foobar DEFINITION.
      PUBLIC SECTION.
        METHODS: moo
          RETURNING VALUE(rv_foo) TYPE string.
    ENDCLASS.

    CLASS lcl_foobar IMPLEMENTATION.
      METHOD moo.
      ENDMETHOD.
    ENDCLASS.`;

    const expected =
`class lcl_foobar {
  constructor() {
    this.me = new abap.types.ABAPObject();
  }
  moo() {
    let rv_foo = new abap.types.String();
    return rv_foo;
  }
}`;

    expect(await runSingle(abap)).to.equal(expected);
  });

  it("CASE", async () => {
    const abap = `
CASE bar.
WHEN 'foo'.
WRITE 2.
WHEN 1 OR 2.
WHEN foo.
WHEN OTHERS.
ENDCASE.`;

    const expected =
`let constant_1 = new abap.types.Integer();
constant_1.set(1);
let constant_2 = new abap.types.Integer();
constant_2.set(2);
switch (bar.get()) {
  case 'foo':
  abap.statements.write(constant_2);
  break;
  case constant_1.get():
  case constant_2.get():
  break;
  case foo.get():
  break;
  default:
  break;
}`;

    expect(await runSingle(abap, {ignoreSyntaxCheck: true})).to.equal(expected);
  });

  it("REF TO object", async () => {
    const abap = `
CLASS zcl_words DEFINITION.
ENDCLASS.
CLASS zcl_words IMPLEMENTATION.
ENDCLASS.
DATA foo TYPE REF TO zcl_words.
CREATE OBJECT foo.`;

    const expected =
`class zcl_words {
  constructor() {
    this.me = new abap.types.ABAPObject();
  }
}
let foo = new abap.types.ABAPObject();
foo.set(new zcl_words());`;

    expect(await runSingle(abap)).to.equal(expected);
  });

  it("Locally defined structure", async () => {
    const abap = `
  TYPES: BEGIN OF foo,
  bar TYPE c,
END OF foo.
DATA moo TYPE foo.`;

    const expected = `let moo = new abap.types.Structure({bar: new abap.types.Character()});`;

    expect(await runSingle(abap)).to.equal(expected);
  });

  it("Class attribute", async () => {
    const abap = `
      CLASS zcl_words DEFINITION.
        PUBLIC SECTION.
          DATA bar TYPE i.
          METHODS: run.
      ENDCLASS.
      CLASS zcl_words IMPLEMENTATION.
        METHOD run.
          WRITE bar.
        ENDMETHOD.
      ENDCLASS.`;

    const expected = `class zcl_words {
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.bar = new abap.types.Integer();
  }
  run() {
    abap.statements.write(this.bar);
  }
}`;

    expect(await runSingle(abap)).to.equal(expected);
  });

  it("Class constructor1", async () => {
    const abap = `
      CLASS zcl_words DEFINITION.
        PUBLIC SECTION.
          DATA bar TYPE i.
          METHODS: constructor.
      ENDCLASS.

      CLASS zcl_words IMPLEMENTATION.
        METHOD constructor.
          bar = 2.
          WRITE bar.
        ENDMETHOD.
      ENDCLASS.`;

    const expected = `let constant_2 = new abap.types.Integer();
constant_2.set(2);
class zcl_words {
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.bar = new abap.types.Integer();
    this.bar.set(constant_2);
    abap.statements.write(this.bar);
  }
}`;

    expect(await runSingle(abap)).to.equal(expected);
  });

  it("Class constructor2", async () => {
    const abap = `
CLASS zcl_ret DEFINITION.
  PUBLIC SECTION.
    DATA bar TYPE i.
    CLASS-METHODS:
      run RETURNING VALUE(rv_ret) TYPE string.
ENDCLASS.

CLASS zcl_ret IMPLEMENTATION.
  METHOD run.
    rv_ret = 'X'.
  ENDMETHOD.
ENDCLASS.`;

    const expected = `class zcl_ret {
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.bar = new abap.types.Integer();
  }
  static run() {
    let rv_ret = new abap.types.String();
    rv_ret.set('X');
    return rv_ret;
  }
}`;

    expect(await runSingle(abap)).to.equal(expected);
  });

  it("TRY, CATCH", async () => {
    const abap = `
TRY.
CATCH cx_root.
ENDTRY.`;

    const expected =
`try {
} catch (e) {
}`;

    expect(await runSingle(abap, {ignoreSyntaxCheck: true})).to.equal(expected);
  });

  it("DO. ENDDO.", async () => {
    const abap = `
DO.
ENDDO.`;

    const expected =
`while (true) {
}`;

    expect(await runSingle(abap, {ignoreSyntaxCheck: true})).to.equal(expected);
  });

  it("Class constant, should set value", async () => {
    const abap = `
CLASS zcl_ret DEFINITION.
  PRIVATE SECTION.
    CONSTANTS: c_maxdcodes TYPE i VALUE 30.
ENDCLASS.

CLASS zcl_ret IMPLEMENTATION.
ENDCLASS.`;

    const expected = `let constant_30 = new abap.types.Integer();
constant_30.set(30);
class zcl_ret {
  constructor() {
    this.me = new abap.types.ABAPObject();
  }
}
zcl_ret.c_maxdcodes = new abap.types.Integer();
zcl_ret.c_maxdcodes.set(30);`;

    expect(await runSingle(abap)).to.equal(expected);
  });

});