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
let unique1 = bar;
if (abap.compare.eq(unique1, 'foo')) {
  abap.statements.write(constant_2);
} else if (abap.compare.eq(unique1, constant_1) || abap.compare.eq(unique1, constant_2)) {
} else if (abap.compare.eq(unique1, foo)) {
} else {
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
  run() {
    return zcl_ret.run();
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

  it("Class static data", async () => {
    const abap = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA foo TYPE i.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
ENDCLASS.`;

    const expected = `class lcl_bar {
  constructor() {
    this.me = new abap.types.ABAPObject();
  }
}
lcl_bar.foo = new abap.types.Integer();`;

    expect(await runSingle(abap)).to.equal(expected);
  });

  it("method call", async () => {
    const abap = `
INTERFACE lif_bar.
  METHODS moo IMPORTING foo TYPE string EXPORTING bar TYPE string.
ENDINTERFACE.
DATA bar TYPE REF TO lif_bar.
DATA str TYPE string.
bar->moo( EXPORTING foo = 'abc'
          IMPORTING bar = str ).`;

    const expected = `let bar = new abap.types.ABAPObject();
let str = new abap.types.String();
bar.get().moo({foo: 'abc', bar: str});`;

    expect(await runSingle(abap)).to.equal(expected);
  });

  it("method call, add default parameter name", async () => {
    const abap = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS bar IMPORTING imp TYPE i.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD bar.
  ENDMETHOD.
ENDCLASS.

FORM bar.
  lcl_bar=>bar( 2 ).
ENDFORM.`;

    const expected = `let constant_2 = new abap.types.Integer();
constant_2.set(2);
class lcl_bar {
  constructor() {
    this.me = new abap.types.ABAPObject();
  }
  bar(unique1) {
    return lcl_bar.bar(unique1);
  }
  static bar(unique1) {
    let imp = new abap.types.Integer();
    if (unique1 && unique1.imp) {imp.set(unique1.imp);}
  }
}
function bar() {
  lcl_bar.bar({imp: constant_2});
}`;

    expect(await runSingle(abap)).to.equal(expected);
  });

  it("constructor with parameter", async () => {
    const abap = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING input TYPE i.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD constructor.
    WRITE input.
  ENDMETHOD.
ENDCLASS.

FORM bar.
  DATA bar TYPE REF TO lcl_bar.
  CREATE OBJECT bar EXPORTING input = 42.
ENDFORM.`;

    const expected = `let constant_42 = new abap.types.Integer();
constant_42.set(42);
class lcl_bar {
  constructor(unique1) {
    this.me = new abap.types.ABAPObject();
    let input = new abap.types.Integer();
    if (unique1 && unique1.input) {input.set(unique1.input);}
    abap.statements.write(input);
  }
}
function bar() {
  let bar = new abap.types.ABAPObject();
  bar.set(new lcl_bar({input: constant_42}));
}`;

    expect(await runSingle(abap)).to.equal(expected);
  });

});