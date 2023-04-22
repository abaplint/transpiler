/* eslint-disable max-len */
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

  it("Interfaces should not be skipped", async () => {
    const abap = `
  INTERFACE lif_foobar.
  ENDINTERFACE.`;

    const expected = `class lif_foobar {
  static INTERNAL_TYPE = 'INTF';
  static ATTRIBUTES = {};
}
abap.Classes['PROG-ZFOOBAR-LIF_FOOBAR'] = lif_foobar;`;

    expect(await runSingle(abap)).to.equal(expected);
  });

  it("TYPES should be skipped", async () => {
    const abap = `
    TYPES: BEGIN OF ty_header,
    field TYPE string,
    value TYPE string,
  END OF ty_header.`;

    expect(await runSingle(abap)).to.equal("");
  });

  it("Simple class, 1", async () => {
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
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL_FOOBAR';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
  }
  async constructor_(INPUT) {
    if (super.constructor_) { await super.constructor_(INPUT); }
    return this;
  }
  async moo() {
  }
}
abap.Classes['PROG-ZFOOBAR-LCL_FOOBAR'] = lcl_foobar;`;

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
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL_FOOBAR';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
  }
  async constructor_(INPUT) {
    if (super.constructor_) { await super.constructor_(INPUT); }
    return this;
  }
  async moo(INPUT) {
    let iv_foo = new abap.types.String({qualifiedName: "STRING"});
    if (INPUT && INPUT.iv_foo) {iv_foo.set(INPUT.iv_foo);}
  }
}
abap.Classes['PROG-ZFOOBAR-LCL_FOOBAR'] = lcl_foobar;`;

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
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL_FOOBAR';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
  }
  async constructor_(INPUT) {
    if (super.constructor_) { await super.constructor_(INPUT); }
    return this;
  }
  async moo() {
    let rv_foo = new abap.types.String({qualifiedName: "STRING"});
    return rv_foo;
  }
}
abap.Classes['PROG-ZFOOBAR-LCL_FOOBAR'] = lcl_foobar;`;

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
`let unique1 = bar;
if (abap.compare.eq(unique1, new abap.types.Character(3).set('foo'))) {
  abap.statements.write(new abap.types.Integer().set(2));
} else if (abap.compare.eq(unique1, new abap.types.Integer().set(1)) || abap.compare.eq(unique1, new abap.types.Integer().set(2))) {
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
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-ZCL_WORDS';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
  }
  async constructor_(INPUT) {
    if (super.constructor_) { await super.constructor_(INPUT); }
    return this;
  }
}
abap.Classes['PROG-ZFOOBAR-ZCL_WORDS'] = zcl_words;
let foo = new abap.types.ABAPObject({qualifiedName: "ZCL_WORDS", RTTIName: "\\\\PROGRAM=ZFOOBAR\\\\CLASS=ZCL_WORDS"});
foo.set(await (new abap.Classes['PROG-ZFOOBAR-ZCL_WORDS']()).constructor_());`;

    expect(await runSingle(abap)).to.equal(expected);
  });

  it("Locally defined structure", async () => {
    const abap = `
  TYPES: BEGIN OF foo,
  bar TYPE c,
END OF foo.
DATA moo TYPE foo.`;

    const expected = `let moo = new abap.types.Structure({"bar": new abap.types.Character(1, {"qualifiedName":"foo-bar"})}, "foo");`;
    expect(await runSingle(abap)).to.equal(expected);
  });

  it("Locally defined structure, bool", async () => {
    const abap = `
  TYPES: BEGIN OF foo,
  bar TYPE abap_bool,
END OF foo.
DATA moo TYPE foo.`;

// hmm, the qualified name is wrong?
    const expected = `let moo = new abap.types.Structure({"bar": new abap.types.Character(1, {"qualifiedName":"ABAP_BOOL","ddicName":"ABAP_BOOL"})}, "foo");`;
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
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-ZCL_WORDS';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"BAR": {"type": () => {return new abap.types.Integer({qualifiedName: "I"});}, "visibility": "U", "is_constant": " "}};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.bar = new abap.types.Integer({qualifiedName: "I"});
  }
  async constructor_(INPUT) {
    if (super.constructor_) { await super.constructor_(INPUT); }
    return this;
  }
  async run() {
    abap.statements.write(this.bar);
  }
}
abap.Classes['PROG-ZFOOBAR-ZCL_WORDS'] = zcl_words;`;

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

    const expected = `class zcl_words {
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-ZCL_WORDS';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"BAR": {"type": () => {return new abap.types.Integer({qualifiedName: "I"});}, "visibility": "U", "is_constant": " "}};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.bar = new abap.types.Integer({qualifiedName: "I"});
  }
  async constructor_(INPUT) {
    this.bar.set(new abap.types.Integer().set(2));
    abap.statements.write(this.bar);
    return this;
  }
}
abap.Classes['PROG-ZFOOBAR-ZCL_WORDS'] = zcl_words;`;

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
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-ZCL_RET';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"BAR": {"type": () => {return new abap.types.Integer({qualifiedName: "I"});}, "visibility": "U", "is_constant": " "}};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.bar = new abap.types.Integer({qualifiedName: "I"});
  }
  async constructor_(INPUT) {
    if (super.constructor_) { await super.constructor_(INPUT); }
    return this;
  }
  async run() {
    return zcl_ret.run();
  }
  static async run() {
    let rv_ret = new abap.types.String({qualifiedName: "STRING"});
    rv_ret.set(new abap.types.Character(1).set('X'));
    return rv_ret;
  }
}
abap.Classes['PROG-ZFOOBAR-ZCL_RET'] = zcl_ret;`;

    expect(await runSingle(abap)).to.equal(expected);
  });

  it("TRY, CATCH", async () => {
    const abap = `
TRY.
CATCH cx_bar cx_foo INTO bar.
CATCH cx_moo.
ENDTRY.`;

    const expected = `try {
} catch (e) {
  if (e instanceof abap.Classes['CX_BAR'] || e instanceof abap.Classes['CX_FOO']) {
    bar.set(e);
  } else if (e instanceof abap.Classes['CX_MOO']) {
  } else {
    throw e;
  }
}`;

    expect(await runSingle(abap, {ignoreSyntaxCheck: true})).to.equal(expected);
  });

  it("DO. ENDDO.", async () => {
    const abap = `
DO.
ENDDO.`;

    const expected =
`const indexBackup1 = abap.builtin.sy.get().index.get();
let unique1 = 1;
while (true) {
  abap.builtin.sy.get().index.set(unique1++);
}
abap.builtin.sy.get().index.set(indexBackup1);`;

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

    const expected = `class zcl_ret {
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-ZCL_RET';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"C_MAXDCODES": {"type": () => {return new abap.types.Integer({qualifiedName: "I"});}, "visibility": "I", "is_constant": "X"}};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.c_maxdcodes = zcl_ret.c_maxdcodes;
  }
  async constructor_(INPUT) {
    if (super.constructor_) { await super.constructor_(INPUT); }
    return this;
  }
}
abap.Classes['PROG-ZFOOBAR-ZCL_RET'] = zcl_ret;
zcl_ret.c_maxdcodes = new abap.types.Integer({qualifiedName: "I"});
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
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL_BAR';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"FOO": {"type": () => {return new abap.types.Integer({qualifiedName: "I"});}, "visibility": "U", "is_constant": " "}};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
  }
  async constructor_(INPUT) {
    if (super.constructor_) { await super.constructor_(INPUT); }
    return this;
  }
}
abap.Classes['PROG-ZFOOBAR-LCL_BAR'] = lcl_bar;
lcl_bar.foo = new abap.types.Integer({qualifiedName: "I"});`;

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

    const expected = `class lcl_bar {
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL_BAR';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
  }
  async constructor_(INPUT) {
    if (super.constructor_) { await super.constructor_(INPUT); }
    return this;
  }
  async bar(INPUT) {
    return lcl_bar.bar(INPUT);
  }
  static async bar(INPUT) {
    let imp = new abap.types.Integer({qualifiedName: "I"});
    if (INPUT && INPUT.imp) {imp.set(INPUT.imp);}
  }
}
abap.Classes['PROG-ZFOOBAR-LCL_BAR'] = lcl_bar;
async function bar() {
  await abap.Classes['PROG-ZFOOBAR-LCL_BAR'].bar({imp: new abap.types.Integer().set(2)});
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

    const expected = `class lcl_bar {
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL_BAR';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
  }
  async constructor_(INPUT) {
    let input = new abap.types.Integer({qualifiedName: "I"});
    if (INPUT && INPUT.input) {input.set(INPUT.input);}
    abap.statements.write(input);
    return this;
  }
}
abap.Classes['PROG-ZFOOBAR-LCL_BAR'] = lcl_bar;
async function bar() {
  let bar = new abap.types.ABAPObject({qualifiedName: "LCL_BAR", RTTIName: "\\\\PROGRAM=ZFOOBAR\\\\CLASS=LCL_BAR"});
  bar.set(await (new abap.Classes['PROG-ZFOOBAR-LCL_BAR']()).constructor_({input: new abap.types.Integer().set(42)}));
}`;

    expect(await runSingle(abap)).to.equal(expected);
  });

  it("Escape class constants", async () => {
    const abap = `
  CLASS lcl_bar DEFINITION.
    PUBLIC SECTION.
      CONSTANTS foo TYPE string VALUE ''''.
  ENDCLASS.
  CLASS lcl_bar IMPLEMENTATION.
  ENDCLASS.`;
    const expected = `class lcl_bar {
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL_BAR';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"FOO": {"type": () => {return new abap.types.String({qualifiedName: "STRING"});}, "visibility": "U", "is_constant": "X"}};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.foo = lcl_bar.foo;
  }
  async constructor_(INPUT) {
    if (super.constructor_) { await super.constructor_(INPUT); }
    return this;
  }
}
abap.Classes['PROG-ZFOOBAR-LCL_BAR'] = lcl_bar;
lcl_bar.foo = new abap.types.String({qualifiedName: "STRING"});
lcl_bar.foo.set('\\'');`;
    expect(await runSingle(abap)).to.equal(expected);
  });

  it("constants next should reference first", async () => {
    const abap = `
  CONSTANTS first TYPE c LENGTH 1 VALUE 'b'.
  CONSTANTS next TYPE c LENGTH 1 VALUE first.`;
    const expected = `let first = new abap.types.Character(1, {});
first.set('b');
let next = new abap.types.Character(1, {});
next.set(first);`;
    expect(await runSingle(abap)).to.equal(expected);
  });

  it("bool VALUE abap_true", async () => {
    const abap = `DATA bool TYPE abap_bool VALUE abap_true.`;
    const expected = `let bool = new abap.types.Character(1, {"qualifiedName":"ABAP_BOOL","ddicName":"ABAP_BOOL"});
bool.set(abap.builtin.abap_true);`;
    expect(await runSingle(abap)).to.equal(expected);
  });

  it("constants next should reference first, class and interface", async () => {
    const abap = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CONSTANTS first TYPE c LENGTH 1 VALUE 'b'.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
ENDCLASS.

INTERFACE bar.
  CONSTANTS next TYPE c LENGTH 1 VALUE lcl_bar=>first.
ENDINTERFACE.`;
    const expected = `class lcl_bar {
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL_BAR';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"FIRST": {"type": () => {return new abap.types.Character(1, {});}, "visibility": "U", "is_constant": "X"}};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.first = lcl_bar.first;
  }
  async constructor_(INPUT) {
    if (super.constructor_) { await super.constructor_(INPUT); }
    return this;
  }
}
abap.Classes['PROG-ZFOOBAR-LCL_BAR'] = lcl_bar;
lcl_bar.first = new abap.types.Character(1, {});
lcl_bar.first.set('b');
class bar {
  static INTERNAL_TYPE = 'INTF';
  static ATTRIBUTES = {"NEXT": {"type": () => {return new abap.types.Character(1, {});}, "visibility": "U", "is_constant": "X"}};
}
abap.Classes['PROG-ZFOOBAR-BAR'] = bar;
bar.bar$next = new abap.types.Character(1, {});
bar.bar$next.set(abap.Classes['PROG-ZFOOBAR-LCL_BAR'].first);`;
    expect(await runSingle(abap)).to.equal(expected);
  });

  it("CREATE OBJECT, dynamic", async () => {
    const abap = `
    DATA blah TYPE REF TO OBJECT.
    CREATE OBJECT blah TYPE ('ZCL_BLAH').`;
    const expected = `let blah = new abap.types.ABAPObject({qualifiedName: undefined, RTTIName: undefined});
if (abap.Classes['ZCL_BLAH'] === undefined) { throw new abap.Classes['CX_SY_CREATE_OBJECT_ERROR']; }
blah.set(await (new abap.Classes['ZCL_BLAH']()).constructor_());`;
    expect(await runSingle(abap)).to.equal(expected);
  });

  it("Complex table key", async () => {
    const abap = `
  TYPES:
    BEGIN OF ty_node,
      path     TYPE string,
      name     TYPE string,
      type     TYPE string,
      value    TYPE string,
      index    TYPE i,
      order    TYPE i,
      children TYPE i,
    END OF ty_node.
   TYPES:
    ty_nodes_ts TYPE SORTED TABLE OF ty_node
      WITH UNIQUE KEY path name
      WITH NON-UNIQUE SORTED KEY array_index COMPONENTS path index
      WITH NON-UNIQUE SORTED KEY item_order COMPONENTS path order.
   DATA bar TYPE ty_nodes_ts.`;
    expect(await runSingle(abap)).to.include(`"keyFields":["PATH","NAME"]`);
  });

  it("CLEANUP", async () => {
    const abap = `
DATA cx TYPE REF TO cx_root.
TRY.
    TRY.
        RAISE EXCEPTION TYPE cx_abap_message_digest.
      CLEANUP INTO cx.
        WRITE / 'foo'.
    ENDTRY.
  CATCH cx_root.
    WRITE / 'bar'.
ENDTRY.`;

    const expected = `let cx = (() => { throw "Void type: cx_root" })();
try {
  try {
    throw await (new abap.Classes['CX_ABAP_MESSAGE_DIGEST']()).constructor_();
  } finally {
    // Transpiler todo: CLEANUP ignored
  }
} catch (e) {
  if (e instanceof abap.Classes['CX_ROOT']) {
    abap.statements.write(new abap.types.Character(3).set('bar'),{newLine: true});
  } else {
    throw e;
  }
}`;
    expect(await runSingle(abap, {
      ignoreSyntaxCheck: true,
      unknownTypes: "runtimeError"})).to.equals(expected);
  });

  it("Upper case method impl name, should transpile to lower", async () => {
    const abap = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    METHODS moo.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD MOO.
  ENDMETHOD.
ENDCLASS.`;
    expect(await runSingle(abap)).to.include(`async moo() {`);
  });

  it("constants and class CaSe", async () => {
    const abap = `CLASS LCL_CONSTANT_TEST DEFINITION.
  PUBLIC SECTION.
    constants AREA_NAME type STRING value 'BAR'.
ENDCLASS.
CLASS LCL_CONSTANT_TEST IMPLEMENTATION.
ENDCLASS.`;
    const expected = `class lcl_constant_test {
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL_CONSTANT_TEST';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"AREA_NAME": {"type": () => {return new abap.types.String({qualifiedName: "STRING"});}, "visibility": "U", "is_constant": "X"}};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.area_name = lcl_constant_test.area_name;
  }
  async constructor_(INPUT) {
    if (super.constructor_) { await super.constructor_(INPUT); }
    return this;
  }
}
abap.Classes['PROG-ZFOOBAR-LCL_CONSTANT_TEST'] = lcl_constant_test;
lcl_constant_test.area_name = new abap.types.String({qualifiedName: "STRING"});
lcl_constant_test.area_name.set('BAR');`;
    expect(await runSingle(abap)).to.equals(expected);
  });

  it("constants CaSE, default value", async () => {
    const abap = `INTERFACE lif.
  CONSTANTS default_value TYPE string VALUE 'sdf'.
ENDINTERFACE.
CLASS lcl DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS foo
      IMPORTING
        bar TYPE string DEFAULT LIF=>DEFAULT_VALUE.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD foo.
  ENDMETHOD.
ENDCLASS.`;
    const expected = `class lif {
  static INTERNAL_TYPE = 'INTF';
  static ATTRIBUTES = {"DEFAULT_VALUE": {"type": () => {return new abap.types.String({qualifiedName: "STRING"});}, "visibility": "U", "is_constant": "X"}};
}
abap.Classes['PROG-ZFOOBAR-LIF'] = lif;
lif.lif$default_value = new abap.types.String({qualifiedName: "STRING"});
lif.lif$default_value.set('sdf');
class lcl {
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
  }
  async constructor_(INPUT) {
    if (super.constructor_) { await super.constructor_(INPUT); }
    return this;
  }
  async foo(INPUT) {
    return lcl.foo(INPUT);
  }
  static async foo(INPUT) {
    let bar = new abap.types.String({qualifiedName: "STRING"});
    if (INPUT && INPUT.bar) {bar.set(INPUT.bar);}
    if (INPUT === undefined || INPUT.bar === undefined) {bar = abap.Classes['PROG-ZFOOBAR-LIF'].lif$default_value;}
  }
}
abap.Classes['PROG-ZFOOBAR-LCL'] = lcl;`;
    expect(await runSingle(abap)).to.equals(expected);
  });

  it("kernel call", async () => {
    const abap = `
DATA hex16 TYPE x LENGTH 16.
CALL 'RFCControl'
  ID 'CODE' FIELD 'U'
  ID 'UUID' FIELD hex16.`;
    const expected = `let hex16 = new abap.types.Hex({length: 16});
if (abap.Classes['KERNEL_CALL'] === undefined) throw new Error("Call kernel class missing");
await abap.Classes['KERNEL_CALL'].call({name: new abap.types.Character(10).set('RFCControl'),code: new abap.types.Character(1).set('U'),uuid: hex16});`;
    expect(await runSingle(abap)).to.equals(expected);
  });

  it("LAISO", async () => {
    const abap = `
TYPES ty TYPE sy-langu.
DATA foo TYPE ty.`;
    const expected = `
let foo = new abap.types.Character(1, {"qualifiedName":"ty","conversionExit":"ISOLA"});`;
    expect(await runSingle(abap)).to.equals(expected);
  });

});