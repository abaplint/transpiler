/* eslint-disable max-len */
import {expect} from "chai";
import {runSingle} from "./_utils";
import {UnknownTypesEnum} from "../src/types";

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
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LIF_FOOBAR';
  static ATTRIBUTES = {};
  static METHODS = {};
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
  static STATIC_SUPER = undefined;
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL_FOOBAR';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {};
  static METHODS = {"MOO": {"visibility": "U", "parameters": {}}};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.INTERNAL_ID = abap.internalIdCounter++;
    this.FRIENDS_ACCESS_INSTANCE = {
      "moo": this.moo.bind(this),
    };
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
  static STATIC_SUPER = undefined;
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL_FOOBAR';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {};
  static METHODS = {"MOO": {"visibility": "U", "parameters": {"RV_FOO": {"type": () => {return new abap.types.String({qualifiedName: "STRING"});}, "is_optional": " ", "parm_kind": "R", "type_name": "StringType"}}}};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.INTERNAL_ID = abap.internalIdCounter++;
    this.FRIENDS_ACCESS_INSTANCE = {
      "moo": this.moo.bind(this),
    };
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
if (abap.compare.eq(unique1, abap.CharacterFactory.get(3, 'foo'))) {
  abap.statements.write(abap.IntegerFactory.get(2));
} else if (abap.compare.eq(unique1, abap.IntegerFactory.get(1)) || abap.compare.eq(unique1, abap.IntegerFactory.get(2))) {
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
  static STATIC_SUPER = undefined;
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-ZCL_WORDS';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {};
  static METHODS = {};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.INTERNAL_ID = abap.internalIdCounter++;
    this.FRIENDS_ACCESS_INSTANCE = {
    };
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

    const expected = `let moo = new abap.types.Structure({
"bar": new abap.types.Character(1, {"qualifiedName":"foo-bar"})}, "foo", undefined, {}, {});`;
    expect(await runSingle(abap)).to.equal(expected);
  });

  it("Locally defined structure, bool", async () => {
    const abap = `
  TYPES: BEGIN OF foo,
  bar TYPE abap_bool,
END OF foo.
DATA moo TYPE foo.`;

// hmm, the qualified name is wrong?
    const expected = `let moo = new abap.types.Structure({
"bar": new abap.types.Character(1, {"qualifiedName":"ABAP_BOOL","ddicName":"ABAP_BOOL"})}, "foo", undefined, {}, {});`;
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
  static STATIC_SUPER = undefined;
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-ZCL_WORDS';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"BAR": {"type": () => {return new abap.types.Integer({qualifiedName: "I"});}, "visibility": "U", "is_constant": " ", "is_class": " "}};
  static METHODS = {"RUN": {"visibility": "U", "parameters": {}}};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.INTERNAL_ID = abap.internalIdCounter++;
    this.FRIENDS_ACCESS_INSTANCE = {
      "run": this.run.bind(this),
    };
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
  static STATIC_SUPER = undefined;
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-ZCL_WORDS';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"BAR": {"type": () => {return new abap.types.Integer({qualifiedName: "I"});}, "visibility": "U", "is_constant": " ", "is_class": " "}};
  static METHODS = {"CONSTRUCTOR": {"visibility": "U", "parameters": {}}};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.INTERNAL_ID = abap.internalIdCounter++;
    this.FRIENDS_ACCESS_INSTANCE = {
    };
    this.bar = new abap.types.Integer({qualifiedName: "I"});
  }
  async constructor_(INPUT) {
    this.bar.set(abap.IntegerFactory.get(2));
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
  static STATIC_SUPER = undefined;
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-ZCL_RET';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"BAR": {"type": () => {return new abap.types.Integer({qualifiedName: "I"});}, "visibility": "U", "is_constant": " ", "is_class": " "}};
  static METHODS = {"RUN": {"visibility": "U", "parameters": {"RV_RET": {"type": () => {return new abap.types.String({qualifiedName: "STRING"});}, "is_optional": " ", "parm_kind": "R", "type_name": "StringType"}}}};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.INTERNAL_ID = abap.internalIdCounter++;
    this.FRIENDS_ACCESS_INSTANCE = {
    };
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
    rv_ret.set(abap.CharacterFactory.get(1, 'X'));
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
  if ((abap.Classes['CX_BAR'] && e instanceof abap.Classes['CX_BAR']) || (abap.Classes['CX_FOO'] && e instanceof abap.Classes['CX_FOO'])) {
    bar.set(e);
  } else if ((abap.Classes['CX_MOO'] && e instanceof abap.Classes['CX_MOO'])) {
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
  static STATIC_SUPER = undefined;
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-ZCL_RET';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"C_MAXDCODES": {"type": () => {return new abap.types.Integer({qualifiedName: "I"});}, "visibility": "I", "is_constant": "X", "is_class": "X"}};
  static METHODS = {};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.INTERNAL_ID = abap.internalIdCounter++;
    this.FRIENDS_ACCESS_INSTANCE = {
    };
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
  static STATIC_SUPER = undefined;
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL_BAR';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"FOO": {"type": () => {return new abap.types.Integer({qualifiedName: "I"});}, "visibility": "U", "is_constant": " ", "is_class": "X"}};
  static METHODS = {};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.INTERNAL_ID = abap.internalIdCounter++;
    this.FRIENDS_ACCESS_INSTANCE = {
    };
    this.foo = lcl_bar.foo;
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

  it("Escape class constants", async () => {
    const abap = `
  CLASS lcl_bar DEFINITION.
    PUBLIC SECTION.
      CONSTANTS foo TYPE string VALUE ''''.
  ENDCLASS.
  CLASS lcl_bar IMPLEMENTATION.
  ENDCLASS.`;
    const expected = `class lcl_bar {
  static STATIC_SUPER = undefined;
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL_BAR';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"FOO": {"type": () => {return new abap.types.String({qualifiedName: "STRING"});}, "visibility": "U", "is_constant": "X", "is_class": "X"}};
  static METHODS = {};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.INTERNAL_ID = abap.internalIdCounter++;
    this.FRIENDS_ACCESS_INSTANCE = {
    };
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
  static STATIC_SUPER = undefined;
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL_BAR';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"FIRST": {"type": () => {return new abap.types.Character(1, {});}, "visibility": "U", "is_constant": "X", "is_class": "X"}};
  static METHODS = {};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.INTERNAL_ID = abap.internalIdCounter++;
    this.FRIENDS_ACCESS_INSTANCE = {
    };
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
  static INTERNAL_NAME = 'PROG-ZFOOBAR-BAR';
  static ATTRIBUTES = {"NEXT": {"type": () => {return new abap.types.Character(1, {});}, "visibility": "U", "is_constant": "X", "is_class": "X"}};
  static METHODS = {};
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
let unique1 = abap.Classes["PROG-ZFOOBAR-"+'ZCL_BLAH'.trimEnd()];
if (unique1 === undefined) { unique1 = abap.Classes['ZCL_BLAH'.trimEnd()]; }
if (unique1 === undefined) { throw new abap.Classes['CX_SY_CREATE_OBJECT_ERROR']; }
blah.set(await (new unique1()).constructor_());`;
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

    const expected = `let cx = (() => { throw new Error("Void type: CX_ROOT") })();
try {
  try {
    const unique1 = await (new abap.Classes['CX_ABAP_MESSAGE_DIGEST']()).constructor_();
    unique1.EXTRA_CX = {"INTERNAL_FILENAME": "zfoobar.prog.abap","INTERNAL_LINE": 5};
    throw unique1;
  } finally {
    // Transpiler todo: CLEANUP ignored
  }
} catch (e) {
  if ((abap.Classes['CX_ROOT'] && e instanceof abap.Classes['CX_ROOT'])) {
    abap.statements.write(abap.CharacterFactory.get(3, 'bar'),{newLine: true});
  } else {
    throw e;
  }
}`;
    expect(await runSingle(abap, {
      ignoreSyntaxCheck: true,
      unknownTypes: UnknownTypesEnum.runtimeError})).to.equals(expected);
  });

  it("NEW unknown class, runtime error", async () => {
    const abap = `
FORM foo.
  DATA(sdf) = NEW zcl_unknown( ).
ENDFORM.`;

    const expected = `async function foo(INPUT) {
  let sdf = (() => { throw new Error("Void type: ZCL_UNKNOWN") })();
  sdf.set(function() { throw new Error("Void type: ZCL_UNKNOWN") })();
}
abap.Forms['PROG-ZFOOBAR-FOO'] = foo;`;
    expect(await runSingle(abap, {
      ignoreSyntaxCheck: true,
      unknownTypes: UnknownTypesEnum.runtimeError})).to.equals(expected);
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
  static STATIC_SUPER = undefined;
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL_CONSTANT_TEST';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {"AREA_NAME": {"type": () => {return new abap.types.String({qualifiedName: "STRING"});}, "visibility": "U", "is_constant": "X", "is_class": "X"}};
  static METHODS = {};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.INTERNAL_ID = abap.internalIdCounter++;
    this.FRIENDS_ACCESS_INSTANCE = {
    };
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
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LIF';
  static ATTRIBUTES = {"DEFAULT_VALUE": {"type": () => {return new abap.types.String({qualifiedName: "STRING"});}, "visibility": "U", "is_constant": "X", "is_class": "X"}};
  static METHODS = {};
}
abap.Classes['PROG-ZFOOBAR-LIF'] = lif;
lif.lif$default_value = new abap.types.String({qualifiedName: "STRING"});
lif.lif$default_value.set('sdf');
class lcl {
  static STATIC_SUPER = undefined;
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'PROG-ZFOOBAR-LCL';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {};
  static METHODS = {"FOO": {"visibility": "U", "parameters": {"BAR": {"type": () => {return new abap.types.String({qualifiedName: "STRING"});}, "is_optional": " ", "parm_kind": "I", "type_name": "StringType"}}}};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.INTERNAL_ID = abap.internalIdCounter++;
    this.FRIENDS_ACCESS_INSTANCE = {
    };
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
DATA hex16 TYPE c LENGTH 16.
CALL 'RFCControl'
  ID 'CODE' FIELD 'U'
  ID 'UUID' FIELD hex16.`;
    const expected = `let hex16 = new abap.types.Character(16, {});
if (abap.Classes['KERNEL_CALL'] === undefined) throw new Error("Call kernel class missing");
await abap.Classes['KERNEL_CALL'].call({name: abap.CharacterFactory.get(10, 'RFCControl'),code: abap.CharacterFactory.get(1, 'U'),uuid: hex16});`;
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

  it("ATTRIBUTES from intf", async () => {
    const abap = `
INTERFACE lif_intf.
  DATA field TYPE i.
ENDINTERFACE.

CLASS lcl_impl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_intf.
ENDCLASS.
CLASS lcl_impl IMPLEMENTATION.
ENDCLASS.`;
    expect(await runSingle(abap)).to.include(`ATTRIBUTES = {"LIF_INTF~FIELD":`);
  });

  it("call fm exceptions", async () => {
    const abap = `
CALL FUNCTION 'FUNCTION_EXISTS'
  EXPORTING
    funcname           = 'SDFSDFSD'
  EXCEPTIONS
    function_not_exist = 1
    OTHERS             = 2.`;
    const expected = `try {
  if (abap.FunctionModules['FUNCTION_EXISTS'] === undefined) { if (abap.Classes['CX_SY_DYN_CALL_ILLEGAL_FUNC'.trimEnd()] === undefined) { throw "CX_SY_DYN_CALL_ILLEGAL_FUNC not found"; } else { throw new abap.Classes['CX_SY_DYN_CALL_ILLEGAL_FUNC'.trimEnd()]();} }
  await abap.FunctionModules['FUNCTION_EXISTS']({exporting: {funcname: abap.CharacterFactory.get(8, 'SDFSDFSD')}});
  abap.builtin.sy.get().subrc.set(0);
} catch (e) {
  if (e.classic) {
      switch (e.classic.toUpperCase()) {
      case "FUNCTION_NOT_EXIST": abap.builtin.sy.get().subrc.set(1); break;
      default: abap.builtin.sy.get().subrc.set(2); break;
        }
    } else {
        throw e;
    }
  }`;
    expect(await runSingle(abap)).to.equal(expected);
  });

  it("dynamic call lookup", async () => {
    const abap = `
data lo_obj type ref to object.
CALL METHOD lo_obj->('SETUP').`;
    const js = await runSingle(abap);
    expect(js).to.include("dynamicCallLookup");
  });

  it("dynamic call lookup, another", async () => {
    const abap = `
data lo_obj type ref to object.
data nam type string.
CALL METHOD lo_obj->(nam).`;
    const js = await runSingle(abap);
    expect(js).to.include("dynamicCallLookup");
  });

  it("testing indentation of JS", async () => {
    const abap = `
DATA l_fieldname TYPE string.
FIELD-SYMBOLS <l_record_all> TYPE ANY.
ASSIGN (l_fieldname) TO <l_record_all>.
WRITE 'sdf'.`;
    const js = await runSingle(abap);
    expect(js).to.include("\nabap.statements.write(abap.CharacterFactory.get(3, 'sdf'));");
  });

});