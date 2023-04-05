import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";
import {tabl_t100xml} from "../_data";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CREATE DATA", () => {

  beforeEach(async () => {
    abap = new ABAP();
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
});