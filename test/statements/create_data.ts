import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";
import {tabl_t100xml, zt100_ttyp} from "../_data";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CREATE DATA", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("CREATE DATA, check INITIAL", async () => {
    const code = `
DATA foo TYPE REF TO i.
ASSERT foo IS INITIAL.
CREATE DATA foo.
ASSERT foo IS NOT INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE DATA, assign value and write", async () => {
    const code = `
    DATA foo TYPE REF TO i.
    CREATE DATA foo.
    foo->* = 2.
    WRITE foo->*.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("CREATE DATA, structure", async () => {
    const code = `
TYPES: BEGIN OF ty_structure,
         field TYPE string,
       END OF ty_structure.
DATA ls_data TYPE REF TO ty_structure.
CREATE DATA ls_data.
ls_data->field = 'hello'.
WRITE ls_data->field.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("CREATE DATA, LIKE LINE OF", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA ref TYPE REF TO data.
FIELD-SYMBOLS <tab> TYPE ANY TABLE.
FIELD-SYMBOLS <row> TYPE any.
ASSIGN tab TO <tab>.
CREATE DATA ref LIKE LINE OF <tab>.
ASSIGN ref->* TO <row>.
INSERT <row> INTO TABLE <tab>.
WRITE / lines( <tab> ).
WRITE / lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n1");
  });

  it("CREATE DATA, LIKE", async () => {
    const code = `
DATA ip_value TYPE t.
DATA lo_value TYPE REF TO data.
FIELD-SYMBOLS <fs_value> TYPE simple.
ip_value = '115555'.
CREATE DATA lo_value LIKE ip_value.
ASSIGN lo_value->* TO <fs_value>.
<fs_value> = ip_value.
WRITE <fs_value>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("115555");
  });

  it("CREATE DATA, LENGTH", async () => {
    const code = `
DATA ref TYPE REF TO data.
DATA lv_len TYPE i.
FIELD-SYMBOLS <fs> TYPE any.
lv_len = 2.
CREATE DATA ref TYPE c LENGTH lv_len.
ASSIGN ref->* TO <fs>.
<fs> = '123'.
WRITE <fs>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12");
  });

  it("CREATE DATA, date", async () => {
    const code = `
DATA dref TYPE REF TO data.
FIELD-SYMBOLS <fs> TYPE any.
CREATE DATA dref TYPE d.
ASSIGN dref->* TO <fs>.
WRITE <fs>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00000000");
  });

  it("CREATE DATA, str fs and refs", async () => {
    const code = `
DATA ref TYPE REF TO data.
DATA lv_type TYPE c LENGTH 1.
FIELD-SYMBOLS <foo> TYPE any.
FIELD-SYMBOLS <sub> TYPE any.
ASSIGN ref TO <foo>.
CREATE DATA <foo> TYPE string.
ASSIGN <foo>->* TO <sub>.
DESCRIBE FIELD <sub> TYPE lv_type.
WRITE lv_type.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("g");
  });

  it("CREATE DATA, more str fs and refs", async () => {
    const code = `
DATA ref TYPE REF TO data.
DATA lv_type TYPE c LENGTH 1.
FIELD-SYMBOLS <foo> TYPE any.
FIELD-SYMBOLS <sub> TYPE any.
ASSIGN ref TO <foo>.
CREATE DATA <foo> TYPE string.
ASSIGN ref->* TO <sub>.
DESCRIBE FIELD <sub> TYPE lv_type.
WRITE lv_type.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("g");
  });

  it("CREATE DATA, TYPE T100", async () => {
    const code = `
    DATA foo type ref to data.
    CREATE DATA foo TYPE T100.
    ASSERT foo IS NOT INITIAL.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE DATA, TYPE t100", async () => {
    const code = `
    DATA foo type ref to data.
    CREATE DATA foo TYPE t100.
    ASSERT foo IS NOT INITIAL.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE DATA, TYPE dynamic T100", async () => {
    const code = `
    DATA foo type ref to data.
    CREATE DATA foo TYPE ('T100').
    ASSERT foo IS NOT INITIAL.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE DATA, TYPE dynamic T100 space", async () => {
    const code = `
    DATA foo type ref to data.
    CREATE DATA foo TYPE ('T100 ').
    ASSERT foo IS NOT INITIAL.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE DATA, TYPE dynamic var", async () => {
    const code = `
    DATA lv_name TYPE c LENGTH 10.
    DATA foo TYPE REF TO data.
    lv_name = 'T100'.
    CREATE DATA foo TYPE (lv_name).
    ASSERT foo IS NOT INITIAL.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE DATA, TYPE T100 dynamic table spaces", async () => {
    const code = `
    DATA foo type ref to data.
    CREATE DATA foo TYPE TABLE OF ('T100 ').
    ASSERT foo IS NOT INITIAL.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE DATA, references", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo CHANGING foo TYPE REF TO data.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    CREATE DATA foo TYPE i.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA bar TYPE REF TO data.
  FIELD-SYMBOLS <any> TYPE any.
  lcl=>foo( CHANGING foo = bar ).
  ASSIGN bar->* TO <any>.
  WRITE <any>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("CREATE DATA, ABAP_BOOL", async () => {
    const code = `
DATA lo_data  TYPE REF TO data.
CREATE DATA lo_data TYPE ('ABAP_BOOL').`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE DATA, STRING", async () => {
    const code = `
DATA parameter_value TYPE REF TO data.
CREATE DATA parameter_value TYPE ('STRING').`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE DATA, I", async () => {
    const code = `
DATA parameter_value TYPE REF TO data.
CREATE DATA parameter_value TYPE ('I').`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE DATA, STRING 2", async () => {
    const code = `
DATA parameter_value TYPE REF TO data.
DATA foo TYPE c LENGTH 60.
foo = 'STRING'.
CREATE DATA parameter_value TYPE (foo).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE DATA, T", async () => {
    const code = `
DATA val TYPE REF TO data.
DATA foo TYPE c LENGTH 60.
foo = 'T'.
CREATE DATA val TYPE (foo).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE DATA, TYPE LINE OF", async () => {
    const code = `
  DATA ref TYPE REF TO data.
  DATA lv_type TYPE c LENGTH 1.
  FIELD-SYMBOLS <line> TYPE any.

  CREATE DATA ref TYPE LINE OF ('ZT100_TTYP').
  ASSIGN ref->* TO <line>.
  DESCRIBE FIELD <line> TYPE lv_type.
  WRITE lv_type.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zt100_ttyp.ttyp.xml", contents: zt100_ttyp},
    ]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("u");
  });

  it("CREATE DATA, INT8, dynamic", async () => {
    const code = `
DATA ref_int8 TYPE REF TO data.
FIELD-SYMBOLS <value> TYPE simple.
CREATE DATA ref_int8 TYPE ('INT8').
ASSIGN ref_int8->* TO <value>.
<value> = 123.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE DATA, INT8, static", async () => {
    const code = `
DATA ref_int8 TYPE REF TO data.
FIELD-SYMBOLS <value> TYPE simple.
CREATE DATA ref_int8 TYPE INT8.
ASSIGN ref_int8->* TO <value>.
<value> = 123.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE DATA, like and inputs and field symbols", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS set_cell IMPORTING ip_value TYPE simple OPTIONAL.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD set_cell.
    DATA lo_value TYPE REF TO data.
    FIELD-SYMBOLS <fs_value> TYPE simple.

    CREATE DATA lo_value LIKE ip_value.
    ASSIGN lo_value->* TO <fs_value>.
    <fs_value> = ip_value.
    WRITE / <fs_value>.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.
  lo->set_cell( 2 ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("CREATE DATA, empty name", async () => {
    const code = `
DATA rdata TYPE REF TO data.
CREATE DATA rdata TYPE ('').`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch(e) {
      expect(e.toString()).to.contain("CX_SY_CREATE_DATA_ERROR");
    }
  });
});
