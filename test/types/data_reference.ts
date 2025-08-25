import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Data reference", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("set data reference from data reference", async () => {
    const code = `
TYPES: BEGIN OF node,
         bar TYPE i,
       END OF node.
DATA node TYPE node.
DATA item1 TYPE REF TO node.
DATA item2 TYPE REF TO node.
GET REFERENCE OF node INTO item1.
node-bar = 2.
item2 = item1.
WRITE / item2->bar.

item2->bar = 3.
WRITE / item1->bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n3");
  });

  it("clear", async () => {
    const code = `
TYPES: BEGIN OF node,
  bar TYPE i,
END OF node.
DATA node TYPE node.
DATA ref TYPE REF TO node.
DATA tab TYPE STANDARD TABLE OF REF TO node.
node-bar = 2.
GET REFERENCE OF node INTO ref.
APPEND ref TO tab.

CLEAR ref.
READ TABLE tab INDEX 1 INTO ref.
WRITE ref->bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("chained set", async () => {
    const code = `
TYPES: BEGIN OF ty,
         descr TYPE string,
       END OF ty.
DATA foo TYPE REF TO ty.
CREATE DATA foo.
foo->*-descr = 'hello'.
WRITE foo->*-descr.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("data references and field symbols", async () => {
    const code = `
DATA int TYPE i.
DATA ref1 TYPE REF TO data.
DATA ref2 TYPE REF TO data.
FIELD-SYMBOLS <fs> TYPE data.
FIELD-SYMBOLS <any> TYPE any.

GET REFERENCE OF int INTO ref1.
ASSIGN ref1 TO <fs>.

ref2 = <fs>.

ASSIGN ref2->* TO <any>.
<any> = 2.

WRITE int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("more data references and field symbols", async () => {
    const code = `
DATA ref TYPE REF TO i.
FIELD-SYMBOLS <ref> TYPE data.
FIELD-SYMBOLS <data> TYPE data.
CREATE DATA ref.
ASSIGN ref TO <ref>.
ASSIGN <ref>->* TO <data>.
ref->* = 2.
WRITE <data>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("another data references and field symbols", async () => {
    const code = `
DATA ref TYPE REF TO i.
FIELD-SYMBOLS <ref> TYPE data.
FIELD-SYMBOLS <data> TYPE data.
CREATE DATA ref.
ASSIGN ref TO <ref>.
ASSIGN <ref>->* TO <data>.
<data> = 2.
WRITE <data>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("type after assign", async () => {
    const code = `
DATA lv_typ TYPE c LENGTH 1.
DATA ref TYPE REF TO i.
FIELD-SYMBOLS <ref> TYPE data.
FIELD-SYMBOLS <data> TYPE data.
CREATE DATA ref.
ASSIGN ref TO <ref>.
ASSIGN <ref>->* TO <data>.
DESCRIBE FIELD <data> TYPE lv_typ.
WRITE lv_typ.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("I");
  });

  it("unassigned", async () => {
    const code = `
DATA ref TYPE REF TO i.
FIELD-SYMBOLS <fs> TYPE data.
ref = <fs>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch(e) {
      expect(e.toString()).to.contain("GETWA_NOT_ASSIGNED");
    }
  });

  it("invalid assignment", async () => {
    const code = `
DATA int TYPE i.
DATA ref TYPE REF TO i.
FIELD-SYMBOLS <fs> TYPE data.
ASSIGN int TO <fs>.
ref = <fs>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch(e) {
      expect(e.toString()).to.contain("OBJECTS_MOVE_NOT_SUPPORTED");
    }
  });

  it("many references and stuff", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS test.
    CLASS-METHODS deref
      IMPORTING
        !ir_data       TYPE REF TO data
      RETURNING
        VALUE(rr_data) TYPE REF TO data .
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD test.
    DATA lv_type TYPE c LENGTH 1.
    DATA dat TYPE i.
    DATA foo TYPE REF TO i.
    DATA bar TYPE REF TO data.
    DATA result TYPE REF TO data.
    FIELD-SYMBOLS <fs> TYPE any.

    GET REFERENCE OF dat INTO foo.
    GET REFERENCE OF foo INTO bar.

* WRITE '@KERNEL console.dir("call deref");'.
    result = deref( bar ).

    ASSIGN result->* TO <fs>.
    DESCRIBE FIELD <fs> TYPE lv_type.
    WRITE / lv_type.
  ENDMETHOD.

  METHOD deref.
    DATA lv_type TYPE c LENGTH 1.
    FIELD-SYMBOLS <data> TYPE data.
    rr_data = ir_data.
    ASSIGN ir_data->* TO <data>.
    DESCRIBE FIELD <data> TYPE lv_type.
    IF lv_type = 'l'.
* WRITE '@KERNEL console.dir("sub");'.
      rr_data = deref( <data> ).
* WRITE '@KERNEL console.dir("sub done");'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>test( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("I");
  });

  it("many references and stuff", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo1 CHANGING foo TYPE data.
    CLASS-METHODS foo2 CHANGING foo TYPE data.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo1.
    FIELD-SYMBOLS <any> TYPE any.
    ASSIGN foo->* TO <any>.
    <any> = 2.
  ENDMETHOD.

  METHOD foo2.
    FIELD-SYMBOLS <any> TYPE any.
    ASSIGN foo->* TO <any>.
    ASSIGN COMPONENT 'FIELD' OF STRUCTURE <any> TO <any>.
    foo1( CHANGING foo = <any> ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  TYPES: BEGIN OF ty,
           field TYPE REF TO i,
         END OF ty.
  DATA bar TYPE REF TO ty.
  CREATE DATA bar.
  CREATE DATA bar->field.
  FIELD-SYMBOLS <any> TYPE any.
  lcl=>foo2( CHANGING foo = bar ).

  ASSIGN bar->* TO <any>.
  ASSIGN COMPONENT 'FIELD' OF STRUCTURE <any> TO <any>.
  ASSIGN <any>->* TO <any>.
  WRITE <any>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("eq, string and table", async () => {
    const code = `
DATA ref1 TYPE REF TO data.
DATA ref2 TYPE REF TO data.
DATA str TYPE string.
DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
GET REFERENCE OF str INTO ref1.
GET REFERENCE OF tab INTO ref2.
IF ref1 = ref2.
  WRITE 'eq'.
ELSE.
  WRITE 'ne'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ne");
  });

  it("eq, empty", async () => {
    const code = `
DATA ref1 TYPE REF TO data.
DATA ref2 TYPE REF TO data.
IF ref1 = ref2.
  WRITE 'eq'.
ELSE.
  WRITE 'ne'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("eq");
  });

  it("ne, different refs", async () => {
    const code = `
DATA ref1 TYPE REF TO data.
DATA ref2 TYPE REF TO data.
DATA str1 TYPE string.
DATA str2 TYPE string.
GET REFERENCE OF str1 INTO ref1.
GET REFERENCE OF str2 INTO ref2.
IF ref1 = ref2.
  WRITE 'eq'.
ELSE.
  WRITE 'ne'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ne");
  });

  it("eq, same refs", async () => {
    const code = `
DATA ref1 TYPE REF TO data.
DATA ref2 TYPE REF TO data.
DATA str1 TYPE string.
GET REFERENCE OF str1 INTO ref1.
GET REFERENCE OF str1 INTO ref2.
IF ref1 = ref2.
  WRITE 'eq'.
ELSE.
  WRITE 'ne'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("eq");
  });

  it("append1", async () => {
    const code = `
DATA: BEGIN OF data,
        field1 TYPE string,
        field2 TYPE i,
      END OF data.

TYPES: BEGIN OF ty,
         name  TYPE string,
         value TYPE REF TO data,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.
DATA lv_type TYPE c LENGTH 1.
FIELD-SYMBOLS <val> TYPE any.

row-name = 'FIELD1'.
GET REFERENCE OF data-field1 INTO row-value.
APPEND row TO tab.

row-name = 'FIELD2'.
GET REFERENCE OF data-field2 INTO row-value.
APPEND row TO tab.

LOOP AT tab INTO row.
  ASSIGN row-value->* TO <val>.
  DESCRIBE FIELD <val> TYPE lv_type.
  WRITE / lv_type.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("g\nI");
  });

  it("append2", async () => {
    const code = `
TYPES: BEGIN OF dty,
         field1 TYPE string,
         field2 TYPE i,
       END OF dty.
DATA data TYPE REF TO dty.

TYPES: BEGIN OF ty,
         name  TYPE string,
         value TYPE REF TO data,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.
DATA lv_type TYPE c LENGTH 1.
FIELD-SYMBOLS <val> TYPE any.
FIELD-SYMBOLS <field> TYPE any.

CREATE DATA data.

row-name = 'FIELD1'.
ASSIGN data->(row-name) TO <field>.
GET REFERENCE OF <field> INTO row-value.
APPEND row TO tab.

row-name = 'FIELD2'.
ASSIGN data->(row-name) TO <field>.
GET REFERENCE OF <field> INTO row-value.
APPEND row TO tab.

LOOP AT tab INTO row.
  ASSIGN row-value->* TO <val>.
  DESCRIBE FIELD <val> TYPE lv_type.
  WRITE / lv_type.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("g\nI");
  });

});