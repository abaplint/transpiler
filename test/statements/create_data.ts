import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

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

  it.skip("CREATE DATA, LENGTH", async () => {
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
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12");
  });

});